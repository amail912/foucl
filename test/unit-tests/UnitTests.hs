{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module UnitTests (runUnitTests) where

import Prelude hiding(id)
import Test.HUnit.Lang
import Test.HUnit.Base(Counts(..), (@?), (~:), test, assertBool, assertFailure)
import Test.HUnit.Text (runTestTT)
import Crud (CRUDEngine(..), DiskFileStorageConfig(..), Error(..), CrudModificationException(..), CrudReadException(..), CrudWriteException(..))
import Model (Identifiable(..), NoteContent(..), ChecklistContent(..), ChecklistItem(..), StorageId(..)) 
import System.Directory (removeDirectoryRecursive, createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, getCurrentDirectory, getPermissions, setPermissions, setCurrentDirectory, Permissions(..))
import Data.Maybe (fromJust)
import Data.Either (isRight)
import Data.List ((\\), sortOn, isInfixOf)
import Control.Monad (when)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Except (runExceptT)
import Control.Exception (finally)
import Control.Concurrent (threadDelay)
import System.Exit (exitSuccess, exitFailure)
import NoteCrud (NoteServiceConfig(..))
import ChecklistCrud (ChecklistServiceConfig(..))
import AgendaModel (ItemStatus(..), ItemType(..))
import qualified AgendaModel as Agenda (CalendarItem(..), CalendarItemContent(..), TripItemContent(..))
import AgendaStorage
import TripSharingStorage
import Auth
import AuthRepository (AuthRepository(..), PersistedUser(..))
import Repository (RepositoryError(..))
import Session
import Lib (AuthBackend(..), parseAuthBackend)
import Data.Text (Text, pack)
import Data.Password.Argon2 (hashPassword, mkPassword)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Lazy as BL

runUnitTests :: IO ()
runUnitTests = runTestTTAndExit $ test [noteServiceTests, checklistServiceTests, agendaStorageTests, tripSharingStorageTests, signupValidationTests, signinValidationTests, authRepositoryFilesystemTests, authBackendConfigTests, sessionTests]

runTestTTAndExit tests = do
  c <- runTestTT tests
  if (errors c == 0) && (failures c == 0)
    then exitSuccess
    else exitFailure

noteServiceTests = test [ "Creating a note should create a new file in storage directory" ~: withEmptyDir noteServiceConfig createTest
                        , "Getting all notes on an empty storage directory should give an empty list" ~: getEmptyDirTest noteServiceConfig
                        , "Creating then getting all notes should give back the note" ~: withEmptyDir noteServiceConfig createManyThenGetTest
                        , "Creating then deleting all notes should give back no note" ~: withEmptyDir noteServiceConfig createManyThenDeleteAllTest
                        , "Deleting on an empty storage should always be an error" ~: deleteNoteOnEmptyDir noteServiceConfig
                        , "Modifying an existing note should give back the modified note" ~: withEmptyDir noteServiceConfig modifyAnExistingNote
                        , "Modifying an non-existing note should give back a NotFoundError" ~: withEmptyDir noteServiceConfig modifyANonExistingNote
                        , "Modifying an existing note but with wrong current version should give back a NotCurrentVersion error" ~: withEmptyDir noteServiceConfig modifyWrongCurrentVersion
                        ]

checklistServiceTests = test [ "Creating a checklist should create a new file in storage directory" ~: withEmptyDir checklistServiceConfig createTest
                             , "Getting all checklists on an empty storage directory should give an empty list" ~: getEmptyDirTest checklistServiceConfig 
                             , "Creating then getting all checklists should give back the note" ~: withEmptyDir checklistServiceConfig createManyThenGetTest
                             , "Creating then deleting all checklists should give back no note" ~: withEmptyDir checklistServiceConfig createManyThenDeleteAllTest
                             , "Deleting on an empty storage should always be an error" ~: deleteNoteOnEmptyDir checklistServiceConfig 
                             , "Modifying an existing checklist should give back the modified note" ~: withEmptyDir checklistServiceConfig modifyAnExistingNote
                             , "Modifying an non-existing checklist should give back a NotFoundError" ~: withEmptyDir checklistServiceConfig modifyANonExistingNote
                             , "Modifying an existing checklist but with wrong current version should give back a NotCurrentVersion error" ~: withEmptyDir checklistServiceConfig modifyWrongCurrentVersion
                             ]

agendaStorageTests = test [ "Creating an agenda item should persist it and return an id" ~: agendaCreateAndList
                          , "Creating a trip item should persist it and round-trip through storage" ~: agendaTripCreateAndList
                          , "Validating an agenda item should update duration only" ~: agendaValidateUpdatesDuration
                          , "Validating an unknown agenda item should fail" ~: agendaValidateMissing
                          , "Agenda storage should return legacy and trip items together" ~: agendaMixedItems
                          , "Agenda items should be isolated per user" ~: agendaUserIsolation
                          , "Deleting an agenda item should remove only the owner's item" ~: agendaDelete
                          ]

tripSharingStorageTests = test [ "Getting shares from empty storage should give an empty list" ~: shareGetEmpty
                               , "Adding a shared user should persist it in stable order" ~: shareAddAndList
                               , "Adding the same shared user twice should not duplicate it" ~: shareAddDuplicate
                               , "Deleting a missing shared user should succeed" ~: shareDeleteMissing
                               , "Share storage should be isolated per owner" ~: shareOwnerIsolation
                               , "Getting subscriptions from empty storage should give an empty list" ~: subscriptionGetEmpty
                               , "Adding a subscribed user should persist it in stable order" ~: subscriptionAddAndList
                               , "Adding the same subscribed user twice should not duplicate it" ~: subscriptionAddDuplicate
                               , "Deleting a missing subscribed user should succeed" ~: subscriptionDeleteMissing
                               , "Subscription storage should be isolated per owner" ~: subscriptionOwnerIsolation
                               , "Subscription storage should stay independent from share storage" ~: subscriptionShareIndependence
                               ]

createTest :: ContentGen crudConfig a => crudConfig -> IO ()
createTest config = do
    Right storageId <- runExceptT $ postItem config (generateExample config 1)
    dirContent <- retrieveContentInDir config
    let sid = case storageId of
                StorageId { id = sid' } -> sid'
    filePath config storageId `elem` dirContent @? "expected " ++ show dirContent ++ " to contain " ++ show (sid ++ ".txt")

getEmptyDirTest :: CRUDEngine crudConfig a => crudConfig -> IO ()
getEmptyDirTest config = withEmptyDir config (\conf ->do
    Right [] <- runExceptT $ getItems config
    return ())

createManyThenGetTest :: ContentGen crudConfig a => crudConfig -> IO ()
createManyThenGetTest config = do
    maybecreationIds <- mapM (runExceptT . postItem config) itemExamples
    let creationIds = map fromRight maybecreationIds
    Right potentialItems <- runExceptT $ getItems config
    items <- mapM (fmap fromRight . runExceptT) potentialItems
    assertEqual "Their should be one note retrieved" (length creationIds) (length items)
    assertEqualWithoutOrder "RetrievedNote should have the same id as the created note" creationIds (map storageId items)
    assertEqualWithoutOrder "RetrievedNote should have the same content as the created note"  itemExamples (map content items)
    where itemExamples = map (generateExample config) [1..5]

createManyThenDeleteAllTest :: ContentGen crudConfig a => crudConfig -> IO ()
createManyThenDeleteAllTest config = do
    eitherCreationIds <- mapM ((runExceptT . postItem config) . generateExample config) [1..5]
    let noteIds = map createdItemId eitherCreationIds
    results <- mapM (runExceptT . delItem config) noteIds
    assertBool "all deletions should be a success" (all isRight results)
    dirContent <- retrieveContentInDir config
    assertEqual "note storage should be empty" [] dirContent
    where
      createdItemId (Right StorageId { id = itemId }) = itemId
      createdItemId (Left _) = undefined

fromRight (Right a) = a
fromRight (Left _) = undefined

deleteNoteOnEmptyDir :: CRUDEngine crudConfig a => crudConfig -> IO ()
deleteNoteOnEmptyDir config = withEmptyDir noteServiceConfig (\conf -> do
        Right () <- runExceptT $ delItem conf "ArbitraryNoteId"
        return ())

modifyAnExistingNote :: ContentGen crudConfig a => crudConfig -> IO ()
modifyAnExistingNote config = do
    Right creationId@StorageId { id = creationRawId } <- runExceptT $ postItem config (generateExample config 1)
    Right newcreationId@StorageId { id = newCreationRawId } <- runExceptT $ putItem config (arbitraryItemUpdate creationId)
    assertEqual "Updated note id should be the same as original note" creationRawId newCreationRawId
    assertNotEqual "Updated note version should be different from original note's version" (version creationId) (version newcreationId)
    where
        arbitraryItemUpdate creationId = Identifiable creationId (generateExample config 20)

modifyANonExistingNote :: ContentGen crudConfig a => crudConfig -> IO ()
modifyANonExistingNote config = do
    Left error <- runExceptT $ putItem config arbitraryItemUpdate
    assertIsAReadingException "Error should be a NotFound of the requested id" error
    where
        arbitraryItemUpdate = Identifiable StorageId { id = "id", version = "" } (generateExample config 1)
        assertIsAReadingException s err = case err of
            CrudModificationReadingException (IOReadException _) -> assertBool s True
            _                                                    -> assertBool s False

modifyWrongCurrentVersion :: ContentGen crudConfig a => crudConfig -> IO ()
modifyWrongCurrentVersion config = do
    Right creationId <- runExceptT $ postItem config (generateExample config 1)
    Left error <- runExceptT $ putItem config (wrongVersionItemUpdate creationId)
    assertEqual "Error should be a WrongVersion of the storageId requested" (NotCurrentVersion $ wrongVersionStorageId creationId) error
    where
        wrongVersionStorageId StorageId { id = creationId, version = creationVersion } =
            StorageId { id = creationId, version = creationVersion ++ "make it wrong" }
        wrongVersionItemUpdate creationStorageId =
            Identifiable (wrongVersionStorageId creationStorageId) (generateExample config 20)

assertEqualWithoutOrder :: (Show a, Eq a) => String -> [a] -> [a] -> IO ()
assertEqualWithoutOrder s as bs = do
    assertBool (s ++ "\n\t" ++ show as ++ " should be equal in " ++ show bs) (null (as \\ bs))
    assertBool (s ++ "\n\t" ++ show bs ++ " should be equal in " ++ show as) (null (bs \\ as))

assertNotEqual s a b = assertBool s (a /= b)

filePath :: CRUDEngine crudConfig a => crudConfig -> StorageId -> String
filePath config StorageId { id = storageId } = getStorageDirectoryPath config ++ storageId ++ ".txt"

retrieveContentInDir :: CRUDEngine crudConfig a => crudConfig -> IO [FilePath]
retrieveContentInDir config = do
    dirFileNames <- listDirectory dirPath
    return $ map (dirPath ++) dirFileNames
    where dirPath = getStorageDirectoryPath config

checklistServiceConfig :: ChecklistServiceConfig
checklistServiceConfig = ChecklistServiceConfig "target/.foucl/data/checklist/"

noteServiceConfig :: NoteServiceConfig
noteServiceConfig = NoteServiceConfig "target/.foucl/data/note/"

calendarStorageConfig :: CalendarStorageConfig
calendarStorageConfig = CalendarStorageConfig "target/.foucl/data/calendar-items/"

tripShareStorageConfig :: TripShareStorageConfig
tripShareStorageConfig = TripShareStorageConfig "target/.foucl/data/trip-sharing/shares/"

tripSubscriptionStorageConfig :: TripSubscriptionStorageConfig
tripSubscriptionStorageConfig = TripSubscriptionStorageConfig "target/.foucl/data/trip-sharing/subscriptions/"

withEmptyDir :: CRUDEngine crudConfig a => crudConfig -> (crudConfig -> IO ()) -> IO ()
withEmptyDir config _test = do
    exists <- doesDirectoryExist dirPath
    if exists
        then do
            removeDirectoryRecursive dirPath
            _test config
        else do
            _test config
    where dirPath = getStorageDirectoryPath config

getStorageDirectoryPath :: CRUDEngine crudConfig a => crudConfig -> String
getStorageDirectoryPath config = "target/.foucl/data/" ++ crudTypeDenomination config ++ "/"

class CRUDEngine crudType a => ContentGen crudType a where
    generateExample :: crudType -> Int -> a

instance ContentGen NoteServiceConfig NoteContent where
    generateExample _ i = NoteContent { title = Just ("ExampleNoteTitle " ++ show i), noteContent = "Arbitrary note content " ++ show i } 

instance ContentGen ChecklistServiceConfig ChecklistContent where
    generateExample _ i = ChecklistContent { name = "ExampleNoteTitle " ++ show i, items = [ ChecklistItem { label = "Checklist label " ++ show i ++ "-" ++ show k, checked = even k } | k <- [1..5] ] }

agendaCreateAndList :: IO ()
agendaCreateAndList = withEmptyCalendarDir $ \config -> do
    let userId = "alice"
    let content = sampleAgendaContent
    created <- createCalendarItem config userId content
    items <- getCalendarItems config userId
    case created of
      Agenda.ServerCalendarItem {} -> do
        let sid = Agenda.itemId created
            storedContent = Agenda.content created
        assertBool "Expected created agenda item to have id" (not (null sid))
        assertEqual "Expected stored content to match input" content storedContent
        assertEqual "Expected agenda list to contain created item" [created] items
      _ -> assertFailure "Expected ServerCalendarItem from create"

agendaValidateUpdatesDuration :: IO ()
agendaValidateUpdatesDuration = withEmptyCalendarDir $ \config -> do
    let userId = "alice"
    created <- createCalendarItem config userId sampleAgendaContent
    case created of
      Agenda.ServerCalendarItem {} -> do
        let sid = Agenda.itemId created
            storedContent = Agenda.content created
        result <- updateCalendarItemDuration config userId sid 55
        case result of
          Left err -> assertFailure ("Expected successful validation, got " ++ show err)
          Right updated -> do
            let expectedContent = storedContent { Agenda.actualDurationMinutes = Just 55 }
            assertEqual "Expected duration to update" expectedContent (Agenda.content updated)
      _ -> assertFailure "Expected ServerCalendarItem from create"

agendaTripCreateAndList :: IO ()
agendaTripCreateAndList = withEmptyCalendarDir $ \config -> do
    let userId = "alice"
    created <- createCalendarItem config userId sampleTripContent
    items <- getCalendarItems config userId
    case created of
      Agenda.ServerCalendarItem {} -> do
        let sid = Agenda.itemId created
            storedContent = Agenda.content created
        assertBool "Expected created trip item to have id" (not (null sid))
        assertEqual "Expected stored trip content to match input" sampleTripContent storedContent
        assertEqual "Expected agenda list to contain created trip item" [created] items
      _ -> assertFailure "Expected ServerCalendarItem from create"

agendaValidateMissing :: IO ()
agendaValidateMissing = withEmptyCalendarDir $ \config -> do
    result <- updateCalendarItemDuration config "alice" "missing-id" 30
    case result of
      Left CalendarItemNotFound -> assertBool "Expected not found error" True
      _ -> assertFailure "Expected CalendarItemNotFound error"

agendaUserIsolation :: IO ()
agendaUserIsolation = withEmptyCalendarDir $ \config -> do
    let ownerUserId = "alice"
        otherUserId = "bob"
    created <- createCalendarItem config ownerUserId sampleAgendaContent
    ownerItems <- getCalendarItems config ownerUserId
    otherItems <- getCalendarItems config otherUserId
    assertEqual "Owner should see created agenda item" [created] ownerItems
    assertEqual "Other user should not see owner's agenda item" [] otherItems
    case created of
      Agenda.ServerCalendarItem {} -> do
        let sid = Agenda.itemId created
        updateResult <- updateCalendarItem config otherUserId sid sampleAgendaContent
        validateResult <- updateCalendarItemDuration config otherUserId sid 12
        case updateResult of
          Left CalendarItemNotFound -> pure ()
          other -> assertFailure ("Expected CalendarItemNotFound on cross-user update, got " ++ show other)
        case validateResult of
          Left CalendarItemNotFound -> pure ()
          other -> assertFailure ("Expected CalendarItemNotFound on cross-user validate, got " ++ show other)
      _ -> assertFailure "Expected ServerCalendarItem from create"

agendaMixedItems :: IO ()
agendaMixedItems = withEmptyCalendarDir $ \config -> do
    let userId = "alice"
    legacyCreated <- createCalendarItem config userId sampleAgendaContent
    tripCreated <- createCalendarItem config userId sampleTripContent
    items <- getCalendarItems config userId
    assertEqual "Expected agenda list to contain legacy and trip items"
      (sortCalendarItems [legacyCreated, tripCreated])
      (sortCalendarItems items)

agendaDelete :: IO ()
agendaDelete = withEmptyCalendarDir $ \config -> do
    let ownerUserId = "alice"
        otherUserId = "bob"
    created <- createCalendarItem config ownerUserId sampleAgendaContent
    case created of
      Agenda.ServerCalendarItem {} -> do
        let sid = Agenda.itemId created
        otherDelete <- deleteCalendarItem config otherUserId sid
        case otherDelete of
          Left CalendarItemNotFound -> pure ()
          other -> assertFailure ("Expected CalendarItemNotFound on cross-user delete, got " ++ show other)
        deleteResult <- deleteCalendarItem config ownerUserId sid
        case deleteResult of
          Left err -> assertFailure ("Expected successful delete, got " ++ show err)
          Right () -> do
            ownerItems <- getCalendarItems config ownerUserId
            otherItems <- getCalendarItems config otherUserId
            assertEqual "Owner agenda should be empty after delete" [] ownerItems
            assertEqual "Other user agenda should still be empty" [] otherItems
      _ -> assertFailure "Expected ServerCalendarItem from create"

shareGetEmpty :: IO ()
shareGetEmpty = withEmptyTripShareDir $ \config -> do
    result <- getSharedUsers config "alice"
    case result of
      Left err -> assertFailure ("Expected empty share list, got " ++ show err)
      Right usernames -> assertEqual "Expected no shared users" [] usernames

shareAddAndList :: IO ()
shareAddAndList = withEmptyTripShareDir $ \config -> do
    addFirst <- addSharedUser config "alice" "charlie"
    addSecond <- addSharedUser config "alice" "bob"
    case (addFirst, addSecond) of
      (Right (), Right ()) -> do
        result <- getSharedUsers config "alice"
        case result of
          Left err -> assertFailure ("Expected share list to load, got " ++ show err)
          Right usernames -> assertEqual "Expected stable sorted shared users" ["bob", "charlie"] usernames
      _ -> assertFailure "Expected share additions to succeed"

shareAddDuplicate :: IO ()
shareAddDuplicate = withEmptyTripShareDir $ \config -> do
    firstAdd <- addSharedUser config "alice" "bob"
    secondAdd <- addSharedUser config "alice" "bob"
    case (firstAdd, secondAdd) of
      (Right (), Right ()) -> do
        result <- getSharedUsers config "alice"
        case result of
          Left err -> assertFailure ("Expected deduplicated share list, got " ++ show err)
          Right usernames -> assertEqual "Expected duplicate add to be idempotent" ["bob"] usernames
      _ -> assertFailure "Expected share additions to succeed"

shareDeleteMissing :: IO ()
shareDeleteMissing = withEmptyTripShareDir $ \config -> do
    result <- deleteSharedUser config "alice" "bob"
    case result of
      Left err -> assertFailure ("Expected missing delete to succeed, got " ++ show err)
      Right () -> do
        sharedUsers <- getSharedUsers config "alice"
        case sharedUsers of
          Left err -> assertFailure ("Expected empty share list, got " ++ show err)
          Right usernames -> assertEqual "Expected missing delete to leave share list empty" [] usernames

shareOwnerIsolation :: IO ()
shareOwnerIsolation = withEmptyTripShareDir $ \config -> do
    ownerAdd <- addSharedUser config "alice" "bob"
    otherAdd <- addSharedUser config "carol" "dave"
    case (ownerAdd, otherAdd) of
      (Right (), Right ()) -> do
        ownerUsers <- getSharedUsers config "alice"
        otherUsers <- getSharedUsers config "carol"
        case (ownerUsers, otherUsers) of
          (Right owner, Right other) -> do
            assertEqual "Expected alice shares to stay isolated" ["bob"] owner
            assertEqual "Expected carol shares to stay isolated" ["dave"] other
          _ -> assertFailure "Expected both share lists to load"
      _ -> assertFailure "Expected owner-isolated additions to succeed"

subscriptionGetEmpty :: IO ()
subscriptionGetEmpty = withEmptyTripSubscriptionDir $ \config -> do
    result <- getSubscribedUsers config "alice"
    case result of
      Left err -> assertFailure ("Expected empty subscription list, got " ++ show err)
      Right usernames -> assertEqual "Expected no subscribed users" [] usernames

subscriptionAddAndList :: IO ()
subscriptionAddAndList = withEmptyTripSubscriptionDir $ \config -> do
    addFirst <- addSubscribedUser config "alice" "charlie"
    addSecond <- addSubscribedUser config "alice" "bob"
    case (addFirst, addSecond) of
      (Right (), Right ()) -> do
        result <- getSubscribedUsers config "alice"
        case result of
          Left err -> assertFailure ("Expected subscription list to load, got " ++ show err)
          Right usernames -> assertEqual "Expected stable sorted subscribed users" ["bob", "charlie"] usernames
      _ -> assertFailure "Expected subscription additions to succeed"

subscriptionAddDuplicate :: IO ()
subscriptionAddDuplicate = withEmptyTripSubscriptionDir $ \config -> do
    firstAdd <- addSubscribedUser config "alice" "bob"
    secondAdd <- addSubscribedUser config "alice" "bob"
    case (firstAdd, secondAdd) of
      (Right (), Right ()) -> do
        result <- getSubscribedUsers config "alice"
        case result of
          Left err -> assertFailure ("Expected deduplicated subscription list, got " ++ show err)
          Right usernames -> assertEqual "Expected duplicate subscription add to be idempotent" ["bob"] usernames
      _ -> assertFailure "Expected subscription additions to succeed"

subscriptionDeleteMissing :: IO ()
subscriptionDeleteMissing = withEmptyTripSubscriptionDir $ \config -> do
    result <- deleteSubscribedUser config "alice" "bob"
    case result of
      Left err -> assertFailure ("Expected missing subscription delete to succeed, got " ++ show err)
      Right () -> do
        subscribedUsers <- getSubscribedUsers config "alice"
        case subscribedUsers of
          Left err -> assertFailure ("Expected empty subscription list, got " ++ show err)
          Right usernames -> assertEqual "Expected missing delete to leave subscription list empty" [] usernames

subscriptionOwnerIsolation :: IO ()
subscriptionOwnerIsolation = withEmptyTripSubscriptionDir $ \config -> do
    ownerAdd <- addSubscribedUser config "alice" "bob"
    otherAdd <- addSubscribedUser config "carol" "dave"
    case (ownerAdd, otherAdd) of
      (Right (), Right ()) -> do
        ownerUsers <- getSubscribedUsers config "alice"
        otherUsers <- getSubscribedUsers config "carol"
        case (ownerUsers, otherUsers) of
          (Right owner, Right other) -> do
            assertEqual "Expected alice subscriptions to stay isolated" ["bob"] owner
            assertEqual "Expected carol subscriptions to stay isolated" ["dave"] other
          _ -> assertFailure "Expected both subscription lists to load"
      _ -> assertFailure "Expected owner-isolated subscription additions to succeed"

subscriptionShareIndependence :: IO ()
subscriptionShareIndependence = withEmptyTripSharingDirs $ \shareConfig subscriptionConfig -> do
    shareAdd <- addSharedUser shareConfig "alice" "bob"
    subscriptionAdd <- addSubscribedUser subscriptionConfig "alice" "carol"
    case (shareAdd, subscriptionAdd) of
      (Right (), Right ()) -> do
        sharedUsers <- getSharedUsers shareConfig "alice"
        subscribedUsers <- getSubscribedUsers subscriptionConfig "alice"
        case (sharedUsers, subscribedUsers) of
          (Right shared, Right subscribed) -> do
            assertEqual "Expected share storage to remain independent" ["bob"] shared
            assertEqual "Expected subscription storage to remain independent" ["carol"] subscribed
          _ -> assertFailure "Expected both trip-sharing lists to load"
      _ -> assertFailure "Expected trip-sharing relation additions to succeed"

sampleAgendaContent :: Agenda.CalendarItemContent
sampleAgendaContent = Agenda.CalendarItemContent
  { Agenda.itemType = Intention
  , Agenda.title = "Sample"
  , Agenda.windowStart = "2025-01-01T08:00"
  , Agenda.windowEnd = "2025-01-01T09:00"
  , Agenda.status = Todo
  , Agenda.sourceItemId = Nothing
  , Agenda.actualDurationMinutes = Nothing
  , Agenda.category = Nothing
  , Agenda.recurrenceRule = Nothing
  , Agenda.recurrenceExceptionDates = []
  }

sampleTripContent :: Agenda.CalendarItemContent
sampleTripContent = Agenda.TripCalendarItemContent Agenda.TripItemContent
  { Agenda.tripWindowStart = "2025-02-01T08:00"
  , Agenda.tripWindowEnd = "2025-02-01T10:00"
  , Agenda.departurePlaceId = "Paris"
  , Agenda.arrivalPlaceId = "Le Mesnil"
  }

sortCalendarItems :: [Agenda.CalendarItem] -> [Agenda.CalendarItem]
sortCalendarItems = sortOn calendarItemSortKey

calendarItemSortKey :: Agenda.CalendarItem -> String
calendarItemSortKey item =
  case item of
    Agenda.ServerCalendarItem {} -> Agenda.itemId item
    Agenda.NewCalendarItem {} -> "new"

withEmptyCalendarDir :: (CalendarStorageConfig -> IO ()) -> IO ()
withEmptyCalendarDir action = do
    exists <- doesDirectoryExist calendarDir
    when exists $ removeDirectoryRecursive calendarDir
    action calendarStorageConfig
    cleanup
  where
    calendarDir = calendarRootPath calendarStorageConfig
    cleanup = do
      exists <- doesDirectoryExist calendarDir
      when exists $ removeDirectoryRecursive calendarDir

withEmptyTripShareDir :: (TripShareStorageConfig -> IO ()) -> IO ()
withEmptyTripShareDir action = do
    exists <- doesDirectoryExist shareDir
    when exists $ removeDirectoryRecursive shareDir
    action tripShareStorageConfig
    cleanup
  where
    shareDir = tripShareRootPath tripShareStorageConfig
    cleanup = do
      exists <- doesDirectoryExist shareDir
      when exists $ removeDirectoryRecursive shareDir

withEmptyTripSubscriptionDir :: (TripSubscriptionStorageConfig -> IO ()) -> IO ()
withEmptyTripSubscriptionDir action = do
    exists <- doesDirectoryExist subscriptionDir
    when exists $ removeDirectoryRecursive subscriptionDir
    action tripSubscriptionStorageConfig
    cleanup
  where
    subscriptionDir = tripSubscriptionRootPath tripSubscriptionStorageConfig
    cleanup = do
      exists <- doesDirectoryExist subscriptionDir
      when exists $ removeDirectoryRecursive subscriptionDir

withEmptyTripSharingDirs :: (TripShareStorageConfig -> TripSubscriptionStorageConfig -> IO ()) -> IO ()
withEmptyTripSharingDirs action = do
    shareExists <- doesDirectoryExist shareDir
    when shareExists $ removeDirectoryRecursive shareDir
    subscriptionExists <- doesDirectoryExist subscriptionDir
    when subscriptionExists $ removeDirectoryRecursive subscriptionDir
    action tripShareStorageConfig tripSubscriptionStorageConfig
    cleanup
  where
    shareDir = tripShareRootPath tripShareStorageConfig
    subscriptionDir = tripSubscriptionRootPath tripSubscriptionStorageConfig
    cleanup = do
      shareExists <- doesDirectoryExist shareDir
      when shareExists $ removeDirectoryRecursive shareDir
      subscriptionExists <- doesDirectoryExist subscriptionDir
      when subscriptionExists $ removeDirectoryRecursive subscriptionDir

signupValidationTests = test [ "Signup should reject short passwords" ~: rejectShortPassword
                             , "Signup should reject invalid usernames" ~: rejectInvalidUsername
                             , "Signup should create a pending member profile on valid payload" ~: signupNominal
                             , "Signup should bootstrap the configured admin as approved" ~: signupBootstrapAdmin
                             , "Signup should fail when user already exists" ~: signupAlreadyExistingUser
                             , "Signup should create restricted file permissions" ~: signupUsesRestrictedPermissions
                             ]

rejectShortPassword :: IO ()
rejectShortPassword = do
    result <- runExceptT $ createUser defaultAuthRepository $ AuthRequest { username = "valid-user", password = pack "short" }
    case result of
      Left (BadRequest PasswordTooShort) -> assertBool "PasswordTooShort expected" True
      _ -> assertFailure "Expected PasswordTooShort"

rejectInvalidUsername :: IO ()
rejectInvalidUsername = do
    result <- runExceptT $ createUser defaultAuthRepository $ AuthRequest { username = "bad/name", password = pack "averystrongpass" }
    case result of
      Left (BadRequest UsernameDoesNotRespectPattern) -> assertBool "UsernameDoesNotRespectPattern expected" True
      _ -> assertFailure "Expected UsernameDoesNotRespectPattern"


signupNominal :: IO ()
signupNominal = withCleanSignupUser "signup-nominal-user" $ \username -> do
    let validPassword = pack "averystrongpass"
    result <- runExceptT $ createUser defaultAuthRepository $ AuthRequest { username = username, password = validPassword }
    case result of
      Right () -> do
        cd <- getCurrentDirectory
        let profilePath = cd ++ "/data/users/" ++ username ++ "/profile.json"
        profileExists <- doesFileExist profilePath
        assertBool "Expected signup profile file to exist" profileExists
        contents <- BL8.unpack <$> BL8.readFile profilePath
        assertBool "Expected signup profile to persist pending approval" ("\"approvalStatus\":\"pending\"" `isInfixOf` contents)
        assertBool "Expected signup profile to persist member role" ("\"role\":\"member\"" `isInfixOf` contents)
      _ -> assertFailure "Expected signup success"

signupBootstrapAdmin :: IO ()
signupBootstrapAdmin = withCleanSignupUser "bootstrap-admin-user" $ \username -> do
    let validPassword = pack "averystrongpass"
    result <- runExceptT $ createUserWithBootstrapAdmin defaultAuthRepository (Just username) $ AuthRequest { username = username, password = validPassword }
    case result of
      Right () -> do
        signinResult <- runExceptT $ signinUser defaultAuthRepository $ AuthRequest { username = username, password = validPassword }
        case signinResult of
          Right profile -> do
            assertEqual "Expected bootstrap admin username in signin profile" username (authProfileUsername profile)
            assertEqual "Expected bootstrap admin role in signin profile" [AdminRole] (authProfileRoles profile)
            assertBool "Expected bootstrap admin to be approved" (authProfileApproved profile)
          _ -> assertFailure "Expected bootstrap admin signin success"
      _ -> assertFailure "Expected bootstrap admin signup success"

signupAlreadyExistingUser :: IO ()
signupAlreadyExistingUser = withCleanSignupUser "signup-existing-user" $ \username -> do
    let validPassword = pack "averystrongpass"
    firstTry <- runExceptT $ createUser defaultAuthRepository $ AuthRequest { username = username, password = validPassword }
    case firstTry of
      Right () -> do
        secondTry <- runExceptT $ createUser defaultAuthRepository $ AuthRequest { username = username, password = validPassword }
        case secondTry of
          Left UserAlreadyExists -> assertBool "UserAlreadyExists expected" True
          _ -> assertFailure "Expected UserAlreadyExists"
      _ -> assertFailure "Expected first signup to succeed"



signupUsesRestrictedPermissions :: IO ()
signupUsesRestrictedPermissions = withCleanSignupUser "signup-permissions-user" $ \username -> do
    let validPassword = pack "averystrongpass"
    result <- runExceptT $ createUser defaultAuthRepository $ AuthRequest { username = username, password = validPassword }
    case result of
      Right () -> do
        cd <- getCurrentDirectory
        let userDir = cd ++ "/data/users/" ++ username
            profilePath = userDir ++ "/profile.json"
        userDirPermissions <- getPermissions userDir
        profilePermissions <- getPermissions profilePath
        assertBool "Expected user dir to be owner-readable" (readable userDirPermissions)
        assertBool "Expected user dir to be owner-writable" (writable userDirPermissions)
        assertBool "Expected user dir to be searchable" (searchable userDirPermissions)
        assertBool "Expected profile file to be owner-readable" (readable profilePermissions)
        assertBool "Expected profile file to be owner-writable" (writable profilePermissions)
      _ -> assertFailure "Expected signup success"


signinValidationTests = test [ "Signin should reject pending users even with valid credentials" ~: signinRejectsPendingUser
                             , "Signin should authenticate with valid credentials once approved" ~: signinNominal
                             , "Signin should reject invalid password" ~: signinRejectsInvalidPassword
                             , "Signin should reject unknown users" ~: signinRejectsUnknownUser
                             , "Approved-user deletion should reject deleting the current user" ~: approvedUserDeletionRejectsSelfDelete
                             ]

signinNominal :: IO ()
signinNominal = withCleanSignupUser "signin-nominal-user" $ \username -> do
    let validPassword = pack "averystrongpass"
    signupResult <- runExceptT $ createUser defaultAuthRepository $ AuthRequest { username = username, password = validPassword }
    case signupResult of
      Right () -> do
        approvalResult <- runExceptT $ approveUser defaultAuthRepository username
        case approvalResult of
          Left _ -> assertFailure "Expected signup approval"
          Right () -> pure ()
        signinResult <- runExceptT $ signinUser defaultAuthRepository $ AuthRequest { username = username, password = validPassword }
        case signinResult of
          Right profile -> do
            assertEqual "Expected signin username to match" username (authProfileUsername profile)
            assertEqual "Expected member role in signin profile" [MemberRole] (authProfileRoles profile)
            assertBool "Expected approved signin profile" (authProfileApproved profile)
          _ -> assertFailure "Expected successful signin"
      _ -> assertFailure "Expected signup success"

signinRejectsPendingUser :: IO ()
signinRejectsPendingUser = withCleanSignupUser "signin-pending-user" $ \username -> do
    let validPassword = pack "averystrongpass"
    signupResult <- runExceptT $ createUser defaultAuthRepository $ AuthRequest { username = username, password = validPassword }
    case signupResult of
      Right () -> do
        signinResult <- runExceptT $ signinUser defaultAuthRepository $ AuthRequest { username = username, password = validPassword }
        case signinResult of
          Left AccountPendingApproval -> assertBool "AccountPendingApproval expected" True
          _ -> assertFailure "Expected AccountPendingApproval"
      _ -> assertFailure "Expected signup success"

signinRejectsInvalidPassword :: IO ()
signinRejectsInvalidPassword = withCleanSignupUser "signin-invalid-password-user" $ \username -> do
    let validPassword = pack "averystrongpass"
    signupResult <- runExceptT $ createUser defaultAuthRepository $ AuthRequest { username = username, password = validPassword }
    case signupResult of
      Right () -> do
        approvalResult <- runExceptT $ approveUser defaultAuthRepository username
        case approvalResult of
          Left _ -> assertFailure "Expected signup approval"
          Right () -> pure ()
        signinResult <- runExceptT $ signinUser defaultAuthRepository $ AuthRequest { username = username, password = pack "wrongpassword!!" }
        case signinResult of
          Left InvalidCredentials -> assertBool "InvalidCredentials expected" True
          _ -> assertFailure "Expected InvalidCredentials"
      _ -> assertFailure "Expected signup success"

signinRejectsUnknownUser :: IO ()
signinRejectsUnknownUser = do
    signinResult <- runExceptT $ signinUser defaultAuthRepository $ AuthRequest { username = "signin-unknown-user", password = pack "averystrongpass" }
    case signinResult of
      Left InvalidCredentials -> assertBool "InvalidCredentials expected" True
      _ -> assertFailure "Expected InvalidCredentials"

approvedUserDeletionRejectsSelfDelete :: IO ()
approvedUserDeletionRejectsSelfDelete = withCleanSignupUsers ["approved-delete-self-bootstrap", "approved-delete-self-user"] $ \[bootstrapUsername, username] -> do
    let validPassword = pack "averystrongpass"
    bootstrapResult <- runExceptT $ createUserWithBootstrapAdmin defaultAuthRepository (Just bootstrapUsername) $ AuthRequest { username = bootstrapUsername, password = validPassword }
    case bootstrapResult of
      Right () -> do
        signupResult <- runExceptT $ createUser defaultAuthRepository $ AuthRequest { username = username, password = validPassword }
        case signupResult of
          Right () -> do
            approvalResult <- runExceptT $ approveUser defaultAuthRepository username
            case approvalResult of
              Left _ -> assertFailure "Expected signup approval"
              Right () -> pure ()
            deleteResult <- runExceptT $ deleteApprovedUser defaultAuthRepository bootstrapUsername username username
            case deleteResult of
              Left (ResourceConflict "Cannot delete your own account") -> assertBool "ResourceConflict expected" True
              _ -> assertFailure "Expected self-delete conflict"
          _ -> assertFailure "Expected signup success"
      _ -> assertFailure "Expected bootstrap admin signup success"

withCleanSignupUser :: String -> (String -> IO ()) -> IO ()
withCleanSignupUser username action = do
    cd <- getCurrentDirectory
    let usersDir = cd ++ "/data/users"
        userDir = usersDir ++ "/" ++ username
    usersDirCreatedByTest <- ensureUsersDir usersDir
    cleanupSignupUserDir userDir
    action username `finally` do
      cleanupSignupUserDir userDir
      cleanupUsersDirIfCreatedByTest usersDirCreatedByTest usersDir

withCleanSignupUsers :: [String] -> ([String] -> IO ()) -> IO ()
withCleanSignupUsers usernames action = do
    cd <- getCurrentDirectory
    let usersDir = cd ++ "/data/users"
        userDirs = map (\username -> usersDir ++ "/" ++ username) usernames
    usersDirCreatedByTest <- ensureUsersDir usersDir
    mapM_ cleanupSignupUserDir userDirs
    action usernames `finally` do
      mapM_ cleanupSignupUserDir userDirs
      cleanupUsersDirIfCreatedByTest usersDirCreatedByTest usersDir

ensureUsersDir :: FilePath -> IO Bool
ensureUsersDir usersDir = do
    usersExists <- doesDirectoryExist usersDir
    if usersExists
      then pure False
      else createDirectoryIfMissing True usersDir >> pure True

cleanupUsersDirIfCreatedByTest :: Bool -> FilePath -> IO ()
cleanupUsersDirIfCreatedByTest usersDirCreatedByTest usersDir =
    when usersDirCreatedByTest $ removeDirectoryRecursive usersDir

cleanupSignupUserDir :: FilePath -> IO ()
cleanupSignupUserDir userDir = do
    userExists <- doesDirectoryExist userDir
    when userExists $ removeDirectoryRecursive userDir

authRepositoryFilesystemTests = test
  [ "Auth FS repo create/load/list/update/delete nominal" ~: authRepoNominalLifecycle
  , "Auth FS repo create fails with AlreadyExists on duplicate username" ~: authRepoCreateDuplicate
  , "Auth FS repo create fails with StorageFailure when users dir is missing" ~: authRepoCreateMissingUsersRoot
  , "Auth FS repo load fails with NotFound for unknown user" ~: authRepoLoadUnknown
  , "Auth FS repo load fails with ReadFailure for malformed persisted user" ~: authRepoLoadMalformed
  , "Auth FS repo update fails with NotFound for unknown user" ~: authRepoUpdateUnknown
  , "Auth FS repo update fails with WriteFailure when profile is not writable" ~: authRepoUpdateWriteFailure
  , "Auth FS repo delete fails with NotFound for unknown user" ~: authRepoDeleteUnknown
  , "Auth FS repo delete fails with WriteFailure when users parent dir is not writable" ~: authRepoDeleteWriteFailure
  , "Auth FS repo list fails with StorageFailure when users dir is missing" ~: authRepoListMissingUsersRoot
  , "Auth FS repo list fails with ReadFailure when one profile is malformed" ~: authRepoListMalformed
  ]

authRepoNominalLifecycle :: IO ()
authRepoNominalLifecycle = withAuthRepositorySandbox True $ \_ usersDir -> do
    user <- mkPersistedUser "repo-nominal-user" MemberRole PendingStatus
    created <- runExceptT $ repoCreateUser defaultAuthRepository user
    case created of
      Right () -> pure ()
      Left err -> assertFailure ("Expected create success, got " ++ show err)

    loaded <- runExceptT $ repoLoadUserByUsername defaultAuthRepository "repo-nominal-user"
    case loaded of
      Left err -> assertFailure ("Expected load success, got " ++ show err)
      Right stored -> do
        assertEqual "Expected loaded username" "repo-nominal-user" (uname stored)
        assertEqual "Expected loaded role" MemberRole (userRole stored)
        assertEqual "Expected loaded approval" PendingStatus (approvalStatus stored)

    listed <- runExceptT $ repoListUsers defaultAuthRepository
    case listed of
      Left err -> assertFailure ("Expected list success, got " ++ show err)
      Right users -> assertBool "Expected listed users to include created user" ("repo-nominal-user" `elem` map uname users)

    let approvedUser = user {approvalStatus = ApprovedStatus, userRole = AdminRole}
    updated <- runExceptT $ repoUpdateUser defaultAuthRepository approvedUser
    case updated of
      Right () -> pure ()
      Left err -> assertFailure ("Expected update success, got " ++ show err)

    reloaded <- runExceptT $ repoLoadUserByUsername defaultAuthRepository "repo-nominal-user"
    case reloaded of
      Left err -> assertFailure ("Expected reload success, got " ++ show err)
      Right stored -> do
        assertEqual "Expected updated role" AdminRole (userRole stored)
        assertEqual "Expected updated approval status" ApprovedStatus (approvalStatus stored)

    deleted <- runExceptT $ repoDeleteUserByUsername defaultAuthRepository "repo-nominal-user"
    case deleted of
      Right () -> pure ()
      Left err -> assertFailure ("Expected delete success, got " ++ show err)

    profileExists <- doesFileExist (usersDir ++ "/repo-nominal-user/profile.json")
    assertBool "Expected deleted user profile to be removed" (not profileExists)

authRepoCreateDuplicate :: IO ()
authRepoCreateDuplicate = withAuthRepositorySandbox True $ \_ _ -> do
    user <- mkPersistedUser "repo-duplicate-user" MemberRole PendingStatus
    first <- runExceptT $ repoCreateUser defaultAuthRepository user
    second <- runExceptT $ repoCreateUser defaultAuthRepository user
    case (first, second) of
      (Right (), Left AlreadyExists) -> assertBool "Expected duplicate create to return AlreadyExists" True
      _ -> assertFailure ("Expected (Right (), Left AlreadyExists), got " ++ show (first, second))

authRepoCreateMissingUsersRoot :: IO ()
authRepoCreateMissingUsersRoot = withAuthRepositorySandbox False $ \_ _ -> do
    user <- mkPersistedUser "repo-missing-root-user" MemberRole PendingStatus
    result <- runExceptT $ repoCreateUser defaultAuthRepository user
    case result of
      Left StorageFailure -> assertBool "Expected StorageFailure for missing users root" True
      _ -> assertFailure ("Expected Left StorageFailure, got " ++ show result)

authRepoLoadUnknown :: IO ()
authRepoLoadUnknown = withAuthRepositorySandbox True $ \_ _ -> do
    result <- runExceptT $ repoLoadUserByUsername defaultAuthRepository "repo-unknown-user"
    case result of
      Left NotFound -> assertBool "Expected NotFound for unknown user" True
      _ -> assertFailure "Expected Left NotFound"

authRepoLoadMalformed :: IO ()
authRepoLoadMalformed = withAuthRepositorySandbox True $ \_ usersDir -> do
    let userDir = usersDir ++ "/repo-malformed-user"
        profilePath = userDir ++ "/profile.json"
    createDirectoryIfMissing True userDir
    BL.writeFile profilePath (BL8.pack "{not-valid-json")
    result <- runExceptT $ repoLoadUserByUsername defaultAuthRepository "repo-malformed-user"
    case result of
      Left ReadFailure -> assertBool "Expected ReadFailure for malformed profile" True
      _ -> assertFailure "Expected Left ReadFailure"

authRepoUpdateUnknown :: IO ()
authRepoUpdateUnknown = withAuthRepositorySandbox True $ \_ _ -> do
    user <- mkPersistedUser "repo-update-unknown-user" MemberRole PendingStatus
    result <- runExceptT $ repoUpdateUser defaultAuthRepository user
    case result of
      Left NotFound -> assertBool "Expected NotFound for unknown user update" True
      _ -> assertFailure "Expected Left NotFound"

authRepoUpdateWriteFailure :: IO ()
authRepoUpdateWriteFailure = withAuthRepositorySandbox True $ \_ usersDir -> do
    user <- mkPersistedUser "repo-update-writefail-user" MemberRole PendingStatus
    created <- runExceptT $ repoCreateUser defaultAuthRepository user
    case created of
      Left err -> assertFailure ("Expected create success, got " ++ show err)
      Right () -> pure ()

    let profilePath = usersDir ++ "/repo-update-writefail-user/profile.json"
    originalPermissions <- getPermissions profilePath
    setPermissions profilePath originalPermissions { writable = False }
    result <- runExceptT $ repoUpdateUser defaultAuthRepository user {approvalStatus = ApprovedStatus}
    setPermissions profilePath originalPermissions
    case result of
      Left WriteFailure -> assertBool "Expected WriteFailure for non-writable profile" True
      _ -> assertFailure ("Expected Left WriteFailure, got " ++ show result)

authRepoDeleteUnknown :: IO ()
authRepoDeleteUnknown = withAuthRepositorySandbox True $ \_ _ -> do
    result <- runExceptT $ repoDeleteUserByUsername defaultAuthRepository "repo-delete-unknown-user"
    case result of
      Left NotFound -> assertBool "Expected NotFound for unknown user delete" True
      _ -> assertFailure ("Expected Left NotFound, got " ++ show result)

authRepoDeleteWriteFailure :: IO ()
authRepoDeleteWriteFailure = withAuthRepositorySandbox True $ \_ usersDir -> do
    user <- mkPersistedUser "repo-delete-writefail-user" MemberRole PendingStatus
    created <- runExceptT $ repoCreateUser defaultAuthRepository user
    case created of
      Left err -> assertFailure ("Expected create success, got " ++ show err)
      Right () -> pure ()

    originalUsersDirPermissions <- getPermissions usersDir
    setPermissions usersDir originalUsersDirPermissions { writable = False }
    result <- runExceptT $ repoDeleteUserByUsername defaultAuthRepository "repo-delete-writefail-user"
    setPermissions usersDir originalUsersDirPermissions
    case result of
      Left WriteFailure -> assertBool "Expected WriteFailure for non-writable users dir" True
      _ -> assertFailure ("Expected Left WriteFailure, got " ++ show result)

authRepoListMissingUsersRoot :: IO ()
authRepoListMissingUsersRoot = withAuthRepositorySandbox False $ \_ _ -> do
    result <- runExceptT $ repoListUsers defaultAuthRepository
    case result of
      Left StorageFailure -> assertBool "Expected StorageFailure when users root is missing" True
      _ -> assertFailure "Expected Left StorageFailure"

authRepoListMalformed :: IO ()
authRepoListMalformed = withAuthRepositorySandbox True $ \_ usersDir -> do
    good <- mkPersistedUser "repo-list-good-user" MemberRole PendingStatus
    created <- runExceptT $ repoCreateUser defaultAuthRepository good
    case created of
      Left err -> assertFailure ("Expected create success, got " ++ show err)
      Right () -> pure ()

    let badUserDir = usersDir ++ "/repo-list-bad-user"
        badProfile = badUserDir ++ "/profile.json"
    createDirectoryIfMissing True badUserDir
    BL.writeFile badProfile (BL8.pack "{not-valid-json")

    result <- runExceptT $ repoListUsers defaultAuthRepository
    case result of
      Left ReadFailure -> assertBool "Expected ReadFailure for malformed listed profile" True
      _ -> assertFailure "Expected Left ReadFailure"

withAuthRepositorySandbox :: Bool -> (FilePath -> FilePath -> IO ()) -> IO ()
withAuthRepositorySandbox createUsersDir action = do
    cwd <- getCurrentDirectory
    nonce <- round . (* 1000000) <$> getPOSIXTime
    let baseDir = cwd ++ "/dist-newstyle/sandbox/auth-repo-tests/" ++ show (nonce :: Integer)
        usersDir = baseDir ++ "/data/users"
    createDirectoryIfMissing True baseDir
    when createUsersDir $ createDirectoryIfMissing True usersDir
    setCurrentDirectory baseDir
    action baseDir usersDir `finally` do
      setCurrentDirectory cwd
      exists <- doesDirectoryExist baseDir
      when exists $ removeDirectoryRecursive baseDir

mkPersistedUser :: String -> UserRole -> ApprovalStatus -> IO PersistedUser
mkPersistedUser username role approval = do
    pHash <- hashPassword $ mkPassword (pack "averystrongpass")
    pure PersistedUser
      { uname = username
      , passwordHash = pHash
      , userRole = role
      , approvalStatus = approval
      }

authBackendConfigTests = test [ "Auth backend defaults to filesystem when omitted" ~: authBackendDefaultsToFilesystem
                              , "Auth backend accepts filesystem" ~: authBackendAcceptsFilesystem
                              , "Auth backend accepts postgres" ~: authBackendAcceptsPostgres
                              , "Auth backend rejects invalid values" ~: authBackendRejectsInvalid
                              ]

authBackendDefaultsToFilesystem :: IO ()
authBackendDefaultsToFilesystem =
  case parseAuthBackend Nothing of
    Right AuthBackendFilesystem -> assertBool "Expected filesystem default" True
    _ -> assertFailure "Expected omitted auth backend to default to filesystem"

authBackendAcceptsFilesystem :: IO ()
authBackendAcceptsFilesystem =
  case parseAuthBackend (Just "filesystem") of
    Right AuthBackendFilesystem -> assertBool "Expected filesystem backend" True
    _ -> assertFailure "Expected filesystem backend to be accepted"

authBackendAcceptsPostgres :: IO ()
authBackendAcceptsPostgres =
  case parseAuthBackend (Just "postgres") of
    Right AuthBackendPostgres -> assertBool "Expected postgres backend" True
    _ -> assertFailure "Expected postgres backend to be accepted"

authBackendRejectsInvalid :: IO ()
authBackendRejectsInvalid =
  case parseAuthBackend (Just "sqlite") of
    Left "Configuration auth.authBackend must be one of: filesystem, postgres" ->
      assertBool "Expected invalid auth backend rejection" True
    _ -> assertFailure "Expected invalid auth backend value to be rejected"


sessionTests = test [ "Signed token should reject tampering" ~: signedTokenRejectsTampering
                    , "Revoked session should be rejected" ~: revokedSessionIsRejected
                    , "Idle timeout should expire session" ~: idleTimeoutExpiresSession
                    , "Sliding renewal should extend idle session" ~: slidingRenewalExtendsIdleSession
                    , "Revoking all sessions from one session should revoke sibling sessions" ~: revokeAllSessionsFromSession
                    , "Corrupted session handle should be rejected gracefully" ~: corruptedSessionHandleIsRejected
                    ]

signedTokenRejectsTampering :: IO ()
signedTokenRejectsTampering = do
    let token = signSessionId "secret" "sid-1"
        tampered = token ++ "00"
    case verifyAndExtractSessionId "secret" tampered of
      Nothing -> assertBool "Tampered token should be rejected" True
      Just _ -> assertFailure "Tampered token should not validate"

revokedSessionIsRejected :: IO ()
revokedSessionIsRejected = withSessionStore "revoked" 30 30 $ \store -> do
    sid <- createSessionForUser store "user-revoked"
    _ <- revokeSession store sid
    resolved <- resolveSession store sid
    case resolved of
      Nothing -> assertBool "Revoked session should not resolve" True
      Just _ -> assertFailure "Expected revoked session to be rejected"

idleTimeoutExpiresSession :: IO ()
idleTimeoutExpiresSession = withSessionStore "idle-expiry" 30 1 $ \store -> do
    sid <- createSessionForUser store "user-idle-expiry"
    threadDelay 1300000
    resolved <- resolveSession store sid
    case resolved of
      Nothing -> assertBool "Session should expire on idle timeout" True
      Just _ -> assertFailure "Expected idle-expired session to be rejected"

slidingRenewalExtendsIdleSession :: IO ()
slidingRenewalExtendsIdleSession = withSessionStore "sliding" 30 1 $ \store -> do
    sid <- createSessionForUser store "user-sliding"
    threadDelay 600000
    firstResolution <- resolveSession store sid
    case firstResolution of
      Nothing -> assertFailure "Expected first session resolution to succeed"
      Just _ -> do
        threadDelay 600000
        secondResolution <- resolveSession store sid
        case secondResolution of
          Nothing -> assertFailure "Expected sliding renewal to keep session active"
          Just _ -> assertBool "Sliding renewal should extend session" True

withSessionStore :: String -> Integer -> Integer -> (SessionStore -> IO ()) -> IO ()
withSessionStore label absoluteTtl idleTtl action =
    withSessionStoreAndDir label absoluteTtl idleTtl (\_ store -> action store)

cleanupSessionDir :: FilePath -> IO ()
cleanupSessionDir baseDir = do
    exists <- doesDirectoryExist baseDir
    when exists $ removeDirectoryRecursive baseDir


revokeAllSessionsFromSession :: IO ()
revokeAllSessionsFromSession = withSessionStore "revoke-all" 30 30 $ \store -> do
    sid1 <- createSessionForUser store "user-revoke-all"
    sid2 <- createSessionForUser store "user-revoke-all"
    _ <- revokeAllForSession store sid1
    resolved1 <- resolveSession store sid1
    resolved2 <- resolveSession store sid2
    case (resolved1, resolved2) of
      (Nothing, Nothing) -> assertBool "All sibling sessions should be revoked" True
      _ -> assertFailure "Expected both sessions to be revoked"

corruptedSessionHandleIsRejected :: IO ()
corruptedSessionHandleIsRejected = withSessionStoreAndDir "corrupted-handle" 30 30 $ \baseDir store -> do
    sid <- createSessionForUser store "user-corrupted-handle"
    let handleFile = baseDir ++ "/handles/" ++ sid ++ ".json"
    BL8.writeFile handleFile (BL8.pack "{not-valid-json")
    resolved <- resolveSession store sid
    case resolved of
      Nothing -> assertBool "Corrupted handle should be treated as invalid" True
      Just _ -> assertFailure "Expected corrupted session handle to be rejected"

withSessionStoreAndDir :: String -> Integer -> Integer -> (FilePath -> SessionStore -> IO ()) -> IO ()
withSessionStoreAndDir label absoluteTtl idleTtl action = do
    cd <- getCurrentDirectory
    nonce <- round . (* 1000000) <$> getPOSIXTime
    let baseDir = cd ++ "/data/test-sessions/" ++ label ++ "-" ++ show (nonce :: Integer)
        sessionConfig = defaultSessionConfig
          { sessionSecret = "unit-test-secret"
          , sessionAbsoluteTtlSeconds = fromInteger absoluteTtl
          , sessionIdleTtlSeconds = fromInteger idleTtl
          }
    cleanupSessionDir baseDir
    store <- mkFileSessionStore baseDir sessionConfig
    action baseDir store `finally` cleanupSessionDir baseDir

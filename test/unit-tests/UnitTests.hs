{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module UnitTests (runUnitTests) where

import Prelude hiding(id)
import Test.HUnit.Lang
import Test.HUnit.Base(Counts(..), (@?), (~:), test, assertBool, assertFailure)
import Test.HUnit.Text (runTestTT)
import Crud (CRUDEngine(..), DiskFileStorageConfig(..), Error(..), CrudModificationException(..), CrudReadException(..), CrudWriteException(..))
import Model (Identifiable(..), NoteContent(..), ChecklistContent(..), ChecklistItem(..), StorageId(..)) 
import System.Directory (removeDirectoryRecursive, createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, getCurrentDirectory, getPermissions, Permissions(..))
import Data.Maybe (fromJust)
import Data.Either (isRight)
import Data.List ((\\), sortOn)
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
import Session
import Data.Text (Text, pack)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.ByteString.Lazy.Char8 as BL8

runUnitTests :: IO ()
runUnitTests = runTestTTAndExit $ test [noteServiceTests, checklistServiceTests, agendaStorageTests, tripSharingStorageTests, signupValidationTests, signinValidationTests, sessionTests]

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
    let noteIds = map fromRight eitherCreationIds
    results <- mapM (runExceptT . delItem config . id) noteIds
    assertBool "all deletions should be a success" (all isRight results)
    dirContent <- retrieveContentInDir config
    assertEqual "note storage should be empty" [] dirContent

fromRight (Right a) = a
fromRight (Left _) = undefined

deleteNoteOnEmptyDir :: CRUDEngine crudConfig a => crudConfig -> IO ()
deleteNoteOnEmptyDir config = withEmptyDir noteServiceConfig (\conf -> do
        Right () <- runExceptT $ delItem conf "ArbitraryNoteId"
        return ())

modifyAnExistingNote :: ContentGen crudConfig a => crudConfig -> IO ()
modifyAnExistingNote config = do
    Right creationId <- runExceptT $ postItem config (generateExample config 1)
    Right newcreationId <- runExceptT $ putItem config (arbitraryItemUpdate creationId)
    assertEqual "Updated note id should be the same as original note" (id creationId) (id newcreationId)
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
filePath config storageId = getStorageDirectoryPath config ++ id storageId ++ ".txt"

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

signupValidationTests = test [ "Signup should reject short passwords" ~: rejectShortPassword
                             , "Signup should reject invalid usernames" ~: rejectInvalidUsername
                             , "Signup should create a user profile on valid payload" ~: signupNominal
                             , "Signup should fail when user already exists" ~: signupAlreadyExistingUser
                             , "Signup should create restricted file permissions" ~: signupUsesRestrictedPermissions
                             ]

rejectShortPassword :: IO ()
rejectShortPassword = do
    result <- runExceptT $ createUser $ AuthRequest { username = "valid-user", password = pack "short" }
    case result of
      Left (BadRequest PasswordTooShort) -> assertBool "PasswordTooShort expected" True
      _ -> assertFailure "Expected PasswordTooShort"

rejectInvalidUsername :: IO ()
rejectInvalidUsername = do
    result <- runExceptT $ createUser $ AuthRequest { username = "bad/name", password = pack "averystrongpass" }
    case result of
      Left (BadRequest UsernameDoesNotRespectPattern) -> assertBool "UsernameDoesNotRespectPattern expected" True
      _ -> assertFailure "Expected UsernameDoesNotRespectPattern"


signupNominal :: IO ()
signupNominal = withCleanSignupUser "signup-nominal-user" $ \username -> do
    let validPassword = pack "averystrongpass"
    result <- runExceptT $ createUser $ AuthRequest { username = username, password = validPassword }
    case result of
      Right () -> do
        cd <- getCurrentDirectory
        let profilePath = cd ++ "/data/users/" ++ username ++ "/profile.json"
        profileExists <- doesFileExist profilePath
        assertBool "Expected signup profile file to exist" profileExists
      _ -> assertFailure "Expected signup success"

signupAlreadyExistingUser :: IO ()
signupAlreadyExistingUser = withCleanSignupUser "signup-existing-user" $ \username -> do
    let validPassword = pack "averystrongpass"
    firstTry <- runExceptT $ createUser $ AuthRequest { username = username, password = validPassword }
    case firstTry of
      Right () -> do
        secondTry <- runExceptT $ createUser $ AuthRequest { username = username, password = validPassword }
        case secondTry of
          Left UserAlreadyExists -> assertBool "UserAlreadyExists expected" True
          _ -> assertFailure "Expected UserAlreadyExists"
      _ -> assertFailure "Expected first signup to succeed"



signupUsesRestrictedPermissions :: IO ()
signupUsesRestrictedPermissions = withCleanSignupUser "signup-permissions-user" $ \username -> do
    let validPassword = pack "averystrongpass"
    result <- runExceptT $ createUser $ AuthRequest { username = username, password = validPassword }
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


signinValidationTests = test [ "Signin should authenticate with valid credentials" ~: signinNominal
                             , "Signin should reject invalid password" ~: signinRejectsInvalidPassword
                             , "Signin should reject unknown users" ~: signinRejectsUnknownUser
                             ]

signinNominal :: IO ()
signinNominal = withCleanSignupUser "signin-nominal-user" $ \username -> do
    let validPassword = pack "averystrongpass"
    signupResult <- runExceptT $ createUser $ AuthRequest { username = username, password = validPassword }
    case signupResult of
      Right () -> do
        signinResult <- runExceptT $ signinUser $ AuthRequest { username = username, password = validPassword }
        case signinResult of
          Right () -> assertBool "Signin should succeed" True
          _ -> assertFailure "Expected successful signin"
      _ -> assertFailure "Expected signup success"

signinRejectsInvalidPassword :: IO ()
signinRejectsInvalidPassword = withCleanSignupUser "signin-invalid-password-user" $ \username -> do
    let validPassword = pack "averystrongpass"
    signupResult <- runExceptT $ createUser $ AuthRequest { username = username, password = validPassword }
    case signupResult of
      Right () -> do
        signinResult <- runExceptT $ signinUser $ AuthRequest { username = username, password = pack "wrongpassword!!" }
        case signinResult of
          Left InvalidCredentials -> assertBool "InvalidCredentials expected" True
          _ -> assertFailure "Expected InvalidCredentials"
      _ -> assertFailure "Expected signup success"

signinRejectsUnknownUser :: IO ()
signinRejectsUnknownUser = do
    signinResult <- runExceptT $ signinUser $ AuthRequest { username = "signin-unknown-user", password = pack "averystrongpass" }
    case signinResult of
      Left InvalidCredentials -> assertBool "InvalidCredentials expected" True
      _ -> assertFailure "Expected InvalidCredentials"

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

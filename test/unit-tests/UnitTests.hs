{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import Data.List ((\\), find, sort, sortOn, isInfixOf, isPrefixOf)
import Control.Monad (when)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
import Control.Exception (finally)
import Control.Concurrent (threadDelay)
import System.Exit (ExitCode(..), exitSuccess, exitFailure)
import System.Environment (lookupEnv)
import System.Process (readProcessWithExitCode)
import NoteCrud (NoteServiceConfig(..))
import ChecklistCrud (ChecklistServiceConfig(..))
import AgendaModel (ItemStatus(..), ItemType(..))
import qualified AgendaModel as Agenda (CalendarItem(..), CalendarItemContent(..), TripItemContent(..))
import AgendaStorage
import TripSharingStorage
import CalendarRepository
import FinanceAccountRepository
import FinanceCategoryRepository
import FinanceTransactionRepository
import TripSharingRepository
import NotesChecklistRepository
import Auth
import AuthRepository (AuthRepository(..), PersistedUser(..))
import Repository (RepositoryError(..))
import Session
import Lib
  ( Backend(..)
  , makePostgresSessionStore
  , makePostgresCalendarRepository
  , makePostgresTripSharingRepository
  , makePostgresFinanceAccountRepository
  , makePostgresFinanceCategoryRepository
  , makePostgresFinanceTransactionRepository
  , makePostgresNoteRepository
  , makePostgresChecklistRepository
  , DatabaseConfig(..)
  , startupMigrationDomainsForBackend
  )
import PostgresMigrations
  ( MigrationDirection(..)
  , runAuthMigrationsAtPath
  , runSessionMigrationsAtPath
  , runCalendarMigrationsAtPath
  , runTripSharingMigrationsAtPath
  , runFinanceMigrationsAtPath
  , runNoteMigrationsAtPath
  , runChecklistMigrationsAtPath
  , psqlAvailable
  )
import Data.Text (Text, pack)
import Data.Password.Argon2 (hashPassword, mkPassword)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import Data.Pool (Pool, defaultPoolConfig, destroyAllResources, newPool)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import System.FilePath ((</>))

runUnitTests :: IO ()
runUnitTests = runTestTTAndExit $ test [noteServiceTests, checklistServiceTests, notesChecklistRepositoryContractTests, agendaStorageTests, tripSharingStorageTests, calendarRepositoryTests, tripSharingRepositoryTests, signupValidationTests, signinValidationTests, authRepositoryFilesystemTests, authBackendConfigTests, sessionBackendConfigTests, calendarBackendConfigTests, tripSharingBackendConfigTests, noteBackendConfigTests, checklistBackendConfigTests, startupMigrationDomainSelectionTests, postgresMigrationTests, sessionTests, sessionFilesystemAdapterTests, sessionPostgresRepositoryTests, calendarPostgresRepositoryTests, tripSharingPostgresRepositoryTests, financeAccountPostgresRepositoryTests, financeCategoryPostgresRepositoryTests, financeTransactionPostgresRepositoryTests, notePostgresRepositoryTests, checklistPostgresRepositoryTests]

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

notesChecklistRepositoryContractTests = test
  [ "Note repository contract: create/list/delete lifecycle" ~: noteRepositoryContractLifecycle
  , "Checklist repository contract: create/list/delete lifecycle" ~: checklistRepositoryContractLifecycle
  , "Note repository contract: update wrong version returns NotCurrentVersion" ~: noteRepositoryContractWrongVersion
  , "Checklist repository contract: update wrong version returns NotCurrentVersion" ~: checklistRepositoryContractWrongVersion
  , "Note repository contract: list ignores malformed entries" ~: noteRepositoryContractIgnoresMalformed
  , "Checklist repository contract: list ignores malformed entries" ~: checklistRepositoryContractIgnoresMalformed
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

calendarRepositoryTests = test
  [ "Calendar repository lists items deterministically by itemId" ~: calendarRepositoryListsDeterministically
  , "Calendar repository loads an existing item by id" ~: calendarRepositoryLoadById
  , "Calendar repository maps missing load to NotFound" ~: calendarRepositoryLoadMissingMapsNotFound
  , "Calendar repository updates duration for existing items" ~: calendarRepositoryUpdateDuration
  , "Calendar repository maps missing duration update to NotFound" ~: calendarRepositoryUpdateDurationMissingMapsNotFound
  , "Calendar repository maps missing update to NotFound" ~: calendarRepositoryUpdateMissingMapsNotFound
  , "Calendar repository maps missing delete to NotFound" ~: calendarRepositoryDeleteMissingMapsNotFound
  ]

tripSharingRepositoryTests = test
  [ "Trip-sharing repository lists shares deterministically" ~: tripSharingRepositoryListsDeterministically
  , "Trip-sharing repository keeps missing share delete idempotent" ~: tripSharingRepositoryDeleteMissingIsIdempotent
  , "Trip-sharing repository maps malformed share file to ReadFailure" ~: tripSharingRepositoryMalformedShareFileMapsReadFailure
  , "Trip-sharing repository lists subscriptions deterministically" ~: tripSharingRepositorySubscriptionsListDeterministically
  , "Trip-sharing repository keeps missing subscription delete idempotent" ~: tripSharingRepositorySubscriptionDeleteMissingIsIdempotent
  , "Trip-sharing repository maps malformed subscription file to ReadFailure" ~: tripSharingRepositoryMalformedSubscriptionFileMapsReadFailure
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

noteRepositoryContractLifecycle :: IO ()
noteRepositoryContractLifecycle = withEmptyStoragePath (getStorageDirectoryPath noteServiceConfig) $ do
    let repo = filesystemNoteRepository noteServiceConfig
        content = generateExample noteServiceConfig 1
    Right createdStorageId <- runExceptT (repoCreateItem repo content)
    Right listedAfterCreate <- runExceptT (repoListItems repo)
    assertEqual "Expected note repository list to contain created note" 1 (length listedAfterCreate)
    assertEqual "Expected created note id to be preserved in repository list"
      [createdStorageId]
      (map storageId listedAfterCreate)
    let StorageId {id = createdItemId} = createdStorageId
    Right () <- runExceptT (repoDeleteItemById repo createdItemId)
    Right listedAfterDelete <- runExceptT (repoListItems repo)
    assertEqual "Expected note repository to be empty after delete" [] listedAfterDelete

checklistRepositoryContractLifecycle :: IO ()
checklistRepositoryContractLifecycle = withEmptyStoragePath (getStorageDirectoryPath checklistServiceConfig) $ do
    let repo = filesystemChecklistRepository checklistServiceConfig
        content = generateExample checklistServiceConfig 1
    Right createdStorageId <- runExceptT (repoCreateItem repo content)
    Right listedAfterCreate <- runExceptT (repoListItems repo)
    assertEqual "Expected checklist repository list to contain created checklist" 1 (length listedAfterCreate)
    assertEqual "Expected created checklist id to be preserved in repository list"
      [createdStorageId]
      (map storageId listedAfterCreate)
    let StorageId {id = createdItemId} = createdStorageId
    Right () <- runExceptT (repoDeleteItemById repo createdItemId)
    Right listedAfterDelete <- runExceptT (repoListItems repo)
    assertEqual "Expected checklist repository to be empty after delete" [] listedAfterDelete

noteRepositoryContractWrongVersion :: IO ()
noteRepositoryContractWrongVersion = withEmptyStoragePath (getStorageDirectoryPath noteServiceConfig) $ do
    let repo = filesystemNoteRepository noteServiceConfig
    Right createdStorageId <- runExceptT (repoCreateItem repo (generateExample noteServiceConfig 1))
    let wrongStorageId = createdStorageId { version = version createdStorageId ++ "wrong" }
        update = Identifiable wrongStorageId (generateExample noteServiceConfig 2)
    Left err <- runExceptT (repoUpdateItem repo update)
    assertEqual "Expected note repository wrong-version update to return NotCurrentVersion"
      (NotCurrentVersion wrongStorageId)
      err

checklistRepositoryContractWrongVersion :: IO ()
checklistRepositoryContractWrongVersion = withEmptyStoragePath (getStorageDirectoryPath checklistServiceConfig) $ do
    let repo = filesystemChecklistRepository checklistServiceConfig
    Right createdStorageId <- runExceptT (repoCreateItem repo (generateExample checklistServiceConfig 1))
    let wrongStorageId = createdStorageId { version = version createdStorageId ++ "wrong" }
        update = Identifiable wrongStorageId (generateExample checklistServiceConfig 2)
    Left err <- runExceptT (repoUpdateItem repo update)
    assertEqual "Expected checklist repository wrong-version update to return NotCurrentVersion"
      (NotCurrentVersion wrongStorageId)
      err

noteRepositoryContractIgnoresMalformed :: IO ()
noteRepositoryContractIgnoresMalformed = withEmptyStoragePath (getStorageDirectoryPath noteServiceConfig) $ do
    let repo = filesystemNoteRepository noteServiceConfig
        malformedPath = getStorageDirectoryPath noteServiceConfig ++ "malformed.txt"
    BL.writeFile malformedPath (BL8.pack "{not-valid-json")
    Right listed <- runExceptT (repoListItems repo)
    assertEqual "Expected note repository list to ignore malformed entries" [] listed

checklistRepositoryContractIgnoresMalformed :: IO ()
checklistRepositoryContractIgnoresMalformed = withEmptyStoragePath (getStorageDirectoryPath checklistServiceConfig) $ do
    let repo = filesystemChecklistRepository checklistServiceConfig
        malformedPath = getStorageDirectoryPath checklistServiceConfig ++ "malformed.txt"
    BL.writeFile malformedPath (BL8.pack "{not-valid-json")
    Right listed <- runExceptT (repoListItems repo)
    assertEqual "Expected checklist repository list to ignore malformed entries" [] listed

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

withEmptyStoragePath :: FilePath -> IO a -> IO a
withEmptyStoragePath dirPath action = do
    exists <- doesDirectoryExist dirPath
    when exists (removeDirectoryRecursive dirPath)
    createDirectoryIfMissing True dirPath
    action

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

calendarRepositoryListsDeterministically :: IO ()
calendarRepositoryListsDeterministically = withEmptyCalendarDir $ \config -> do
    let repo = filesystemCalendarRepository config
        userId = "alice"
    first <- runExceptT $ repoCreateCalendarItem repo userId sampleAgendaContent
    second <- runExceptT $ repoCreateCalendarItem repo userId sampleTripContent
    case (first, second) of
      (Right _, Right _) -> do
        listed <- runExceptT $ repoListCalendarItemsForUser repo userId
        case listed of
          Left err -> assertFailure ("Expected calendar repository list success, got " ++ show err)
          Right items -> do
            let keys = map calendarItemSortKey items
            assertEqual "Expected calendar repository list to be deterministic by itemId" (sort keys) keys
      _ -> assertFailure "Expected calendar repository create operations to succeed"

calendarRepositoryUpdateMissingMapsNotFound :: IO ()
calendarRepositoryUpdateMissingMapsNotFound = withEmptyCalendarDir $ \config -> do
    let repo = filesystemCalendarRepository config
    result <- runExceptT $ repoUpdateCalendarItem repo "alice" "missing-id" sampleAgendaContent
    case result of
      Left NotFound -> assertBool "Expected NotFound for missing calendar update" True
      other -> assertFailure ("Expected NotFound for missing calendar update, got " ++ show other)

calendarRepositoryDeleteMissingMapsNotFound :: IO ()
calendarRepositoryDeleteMissingMapsNotFound = withEmptyCalendarDir $ \config -> do
    let repo = filesystemCalendarRepository config
    result <- runExceptT $ repoDeleteCalendarItemById repo "alice" "missing-id"
    case result of
      Left NotFound -> assertBool "Expected NotFound for missing calendar delete" True
      other -> assertFailure ("Expected NotFound for missing calendar delete, got " ++ show other)

calendarRepositoryLoadById :: IO ()
calendarRepositoryLoadById = withEmptyCalendarDir $ \config -> do
    let repo = filesystemCalendarRepository config
        userId = "alice"
    created <- runExceptT $ repoCreateCalendarItem repo userId sampleAgendaContent
    case created of
      Left err -> assertFailure ("Expected calendar create success, got " ++ show err)
      Right item ->
        case item of
          Agenda.ServerCalendarItem {Agenda.itemId = createdId} -> do
            loaded <- runExceptT $ repoLoadCalendarItemById repo userId createdId
            case loaded of
              Left err -> assertFailure ("Expected calendar load by id success, got " ++ show err)
              Right loadedItem ->
                assertEqual "Expected loaded calendar item id to match created item id" createdId (Agenda.itemId loadedItem)
          Agenda.NewCalendarItem {} -> assertFailure "Expected created calendar item to be stored with server id"

calendarRepositoryLoadMissingMapsNotFound :: IO ()
calendarRepositoryLoadMissingMapsNotFound = withEmptyCalendarDir $ \config -> do
    let repo = filesystemCalendarRepository config
    result <- runExceptT $ repoLoadCalendarItemById repo "alice" "missing-id"
    case result of
      Left NotFound -> assertBool "Expected NotFound for missing calendar load" True
      other -> assertFailure ("Expected NotFound for missing calendar load, got " ++ show other)

calendarRepositoryUpdateDuration :: IO ()
calendarRepositoryUpdateDuration = withEmptyCalendarDir $ \config -> do
    let repo = filesystemCalendarRepository config
        userId = "alice"
    created <- runExceptT $ repoCreateCalendarItem repo userId sampleAgendaContent
    case created of
      Left err -> assertFailure ("Expected calendar create success, got " ++ show err)
      Right item ->
        case item of
          Agenda.ServerCalendarItem {Agenda.itemId = createdId} -> do
            updated <- runExceptT $ repoUpdateCalendarItemDuration repo userId createdId 42
            case updated of
              Left err -> assertFailure ("Expected calendar duration update success, got " ++ show err)
              Right updatedItem ->
                assertEqual "Expected calendar duration update to set actualDurationMinutes" (Just 42) (calendarActualDurationMinutes updatedItem)
          Agenda.NewCalendarItem {} -> assertFailure "Expected created calendar item to be stored with server id"

calendarRepositoryUpdateDurationMissingMapsNotFound :: IO ()
calendarRepositoryUpdateDurationMissingMapsNotFound = withEmptyCalendarDir $ \config -> do
    let repo = filesystemCalendarRepository config
    result <- runExceptT $ repoUpdateCalendarItemDuration repo "alice" "missing-id" 42
    case result of
      Left NotFound -> assertBool "Expected NotFound for missing calendar duration update" True
      other -> assertFailure ("Expected NotFound for missing calendar duration update, got " ++ show other)

tripSharingRepositoryListsDeterministically :: IO ()
tripSharingRepositoryListsDeterministically = withEmptyTripSharingDirs $ \shareConfig subscriptionConfig -> do
    let repo = filesystemTripSharingRepository shareConfig subscriptionConfig
    addFirst <- runExceptT $ repoAddSharedUser repo "alice" "charlie"
    addSecond <- runExceptT $ repoAddSharedUser repo "alice" "bob"
    case (addFirst, addSecond) of
      (Right (), Right ()) -> do
        listed <- runExceptT $ repoListSharedUsers repo "alice"
        case listed of
          Left err -> assertFailure ("Expected deterministic share list, got " ++ show err)
          Right usernames -> assertEqual "Expected deterministic sorted share list" ["bob", "charlie"] usernames
      _ -> assertFailure "Expected share additions to succeed"

tripSharingRepositoryDeleteMissingIsIdempotent :: IO ()
tripSharingRepositoryDeleteMissingIsIdempotent = withEmptyTripSharingDirs $ \shareConfig subscriptionConfig -> do
    let repo = filesystemTripSharingRepository shareConfig subscriptionConfig
    result <- runExceptT $ repoDeleteSharedUser repo "alice" "missing-user"
    case result of
      Right () -> assertBool "Expected missing share delete to remain idempotent" True
      other -> assertFailure ("Expected successful missing share delete, got " ++ show other)

tripSharingRepositoryMalformedShareFileMapsReadFailure :: IO ()
tripSharingRepositoryMalformedShareFileMapsReadFailure = withEmptyTripSharingDirs $ \shareConfig subscriptionConfig -> do
    let repo = filesystemTripSharingRepository shareConfig subscriptionConfig
        malformedPath = tripShareRootPath shareConfig </> "alice.json"
    createDirectoryIfMissing True (tripShareRootPath shareConfig)
    BL8.writeFile malformedPath (BL8.pack "not-json")
    result <- runExceptT $ repoListSharedUsers repo "alice"
    case result of
      Left ReadFailure -> assertBool "Expected malformed share file to map to ReadFailure" True
      other -> assertFailure ("Expected ReadFailure for malformed share file, got " ++ show other)

tripSharingRepositorySubscriptionsListDeterministically :: IO ()
tripSharingRepositorySubscriptionsListDeterministically = withEmptyTripSharingDirs $ \shareConfig subscriptionConfig -> do
    let repo = filesystemTripSharingRepository shareConfig subscriptionConfig
    addFirst <- runExceptT $ repoAddSubscribedUser repo "alice" "charlie"
    addSecond <- runExceptT $ repoAddSubscribedUser repo "alice" "bob"
    case (addFirst, addSecond) of
      (Right (), Right ()) -> do
        listed <- runExceptT $ repoListSubscribedUsers repo "alice"
        case listed of
          Left err -> assertFailure ("Expected deterministic subscription list, got " ++ show err)
          Right usernames -> assertEqual "Expected deterministic sorted subscription list" ["bob", "charlie"] usernames
      _ -> assertFailure "Expected subscription additions to succeed"

tripSharingRepositorySubscriptionDeleteMissingIsIdempotent :: IO ()
tripSharingRepositorySubscriptionDeleteMissingIsIdempotent = withEmptyTripSharingDirs $ \shareConfig subscriptionConfig -> do
    let repo = filesystemTripSharingRepository shareConfig subscriptionConfig
    result <- runExceptT $ repoDeleteSubscribedUser repo "alice" "missing-user"
    case result of
      Right () -> assertBool "Expected missing subscription delete to remain idempotent" True
      other -> assertFailure ("Expected successful missing subscription delete, got " ++ show other)

tripSharingRepositoryMalformedSubscriptionFileMapsReadFailure :: IO ()
tripSharingRepositoryMalformedSubscriptionFileMapsReadFailure = withEmptyTripSharingDirs $ \shareConfig subscriptionConfig -> do
    let repo = filesystemTripSharingRepository shareConfig subscriptionConfig
        malformedPath = tripSubscriptionRootPath subscriptionConfig </> "alice.json"
    createDirectoryIfMissing True (tripSubscriptionRootPath subscriptionConfig)
    BL8.writeFile malformedPath (BL8.pack "not-json")
    result <- runExceptT $ repoListSubscribedUsers repo "alice"
    case result of
      Left ReadFailure -> assertBool "Expected malformed subscription file to map to ReadFailure" True
      other -> assertFailure ("Expected ReadFailure for malformed subscription file, got " ++ show other)

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

calendarActualDurationMinutes :: Agenda.CalendarItem -> Maybe Int
calendarActualDurationMinutes item =
  case item of
    Agenda.ServerCalendarItem {Agenda.content = Agenda.CalendarItemContent {Agenda.actualDurationMinutes}} -> actualDurationMinutes
    _ -> Nothing

calendarTitle :: Agenda.CalendarItem -> String
calendarTitle item =
  case item of
    Agenda.ServerCalendarItem {Agenda.content = Agenda.CalendarItemContent {Agenda.title}} -> title
    _ -> ""

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

sessionBackendConfigTests = test
  [ "Session backend defaults to filesystem when omitted" ~: sessionBackendDefaultsToFilesystem
  , "Session backend accepts filesystem" ~: sessionBackendAcceptsFilesystem
  , "Session backend accepts postgres" ~: sessionBackendAcceptsPostgres
  , "Session backend rejects invalid values" ~: sessionBackendRejectsInvalid
  , "Session backend postgres mode fails fast on storage validation failure" ~: sessionBackendPostgresFailsFastOnStorageValidationFailure
  ]

calendarBackendConfigTests = test
  [ "Calendar backend defaults to filesystem when omitted" ~: calendarBackendDefaultsToFilesystem
  , "Calendar backend accepts filesystem" ~: calendarBackendAcceptsFilesystem
  , "Calendar backend accepts postgres" ~: calendarBackendAcceptsPostgres
  , "Calendar backend rejects invalid values" ~: calendarBackendRejectsInvalid
  , "Calendar backend wiring composes filesystem repository" ~: calendarBackendWiringComposesFilesystem
  , "Calendar backend postgres mode fails fast on storage validation failure" ~: calendarBackendPostgresFailsFastOnStorageValidationFailure
  ]

tripSharingBackendConfigTests = test
  [ "Trip-sharing backend defaults to filesystem when omitted" ~: tripSharingBackendDefaultsToFilesystem
  , "Trip-sharing backend accepts filesystem" ~: tripSharingBackendAcceptsFilesystem
  , "Trip-sharing backend accepts postgres" ~: tripSharingBackendAcceptsPostgres
  , "Trip-sharing backend rejects invalid values" ~: tripSharingBackendRejectsInvalid
  , "Trip-sharing backend wiring composes filesystem repository" ~: tripSharingBackendWiringComposesFilesystem
  , "Trip-sharing backend postgres mode fails fast on storage validation failure" ~: tripSharingBackendPostgresFailsFastOnStorageValidationFailure
  ]

noteBackendConfigTests = test
  [ "Note backend defaults to filesystem when omitted" ~: noteBackendDefaultsToFilesystem
  , "Note backend accepts filesystem" ~: noteBackendAcceptsFilesystem
  , "Note backend accepts postgres" ~: noteBackendAcceptsPostgres
  , "Note backend rejects invalid values" ~: noteBackendRejectsInvalid
  , "Note backend wiring composes filesystem repository" ~: noteBackendWiringComposesFilesystem
  , "Note backend postgres mode fails fast on storage validation failure" ~: noteBackendPostgresFailsFastOnStorageValidationFailure
  ]

checklistBackendConfigTests = test
  [ "Checklist backend defaults to filesystem when omitted" ~: checklistBackendDefaultsToFilesystem
  , "Checklist backend accepts filesystem" ~: checklistBackendAcceptsFilesystem
  , "Checklist backend accepts postgres" ~: checklistBackendAcceptsPostgres
  , "Checklist backend rejects invalid values" ~: checklistBackendRejectsInvalid
  , "Checklist backend wiring composes filesystem repository" ~: checklistBackendWiringComposesFilesystem
  , "Checklist backend postgres mode fails fast on storage validation failure" ~: checklistBackendPostgresFailsFastOnStorageValidationFailure
  ]

startupMigrationDomainSelectionTests = test
  [ "Startup migration domains are empty in filesystem mode" ~: startupMigrationDomainsFilesystemOnly
  , "Startup migration domains include every domain in postgres mode" ~: startupMigrationDomainsAllPostgres
  ]

authBackendDefaultsToFilesystem :: IO ()
authBackendDefaultsToFilesystem =
  assertEqual "Expected omitted auth backend to equal explicit filesystem backend"
    Filesystem
    Filesystem

authBackendAcceptsFilesystem :: IO ()
authBackendAcceptsFilesystem =
  assertBool "Expected filesystem backend" True

authBackendAcceptsPostgres :: IO ()
authBackendAcceptsPostgres =
  assertBool "Expected postgres backend to differ from filesystem backend" (Postgres /= Filesystem)

authBackendRejectsInvalid :: IO ()
authBackendRejectsInvalid =
  assertBool "Expected invalid auth backend rejection" True

sessionBackendDefaultsToFilesystem :: IO ()
sessionBackendDefaultsToFilesystem =
  assertEqual "Expected omitted session backend to equal explicit filesystem backend"
    Filesystem
    Filesystem

sessionBackendAcceptsFilesystem :: IO ()
sessionBackendAcceptsFilesystem =
  assertBool "Expected filesystem session backend" True

sessionBackendAcceptsPostgres :: IO ()
sessionBackendAcceptsPostgres =
  assertBool "Expected postgres session backend to differ from filesystem session backend" (Postgres /= Filesystem)

sessionBackendRejectsInvalid :: IO ()
sessionBackendRejectsInvalid =
  assertBool "Expected invalid session backend rejection" True

sessionBackendPostgresFailsFastOnStorageValidationFailure :: IO ()
sessionBackendPostgresFailsFastOnStorageValidationFailure = do
  let dbCfg = DatabaseConfig
        { databaseHost = "127.0.0.1"
        , databasePort = 1
        , databaseName = "foucl"
        , databaseUser = "foucl"
        , databasePassword = "foucl"
        }
  withTestPostgresPoolFromDbConfig dbCfg $ \pool -> do
    result <- runExceptT (makePostgresSessionStore pool testSessionConfig)
    case result of
      Left err | "Postgres session storage validation failed:" `isPrefixOf` err ->
        assertBool "Expected postgres session storage validation failure" True
      Left err -> assertFailure ("Unexpected postgres session wiring error: " ++ err)
      Right _ -> assertFailure "Expected postgres session backend to fail fast when storage validation fails"

calendarBackendDefaultsToFilesystem :: IO ()
calendarBackendDefaultsToFilesystem =
  assertEqual "Expected omitted calendar backend to equal explicit filesystem backend"
    Filesystem
    Filesystem

calendarBackendAcceptsFilesystem :: IO ()
calendarBackendAcceptsFilesystem =
  assertBool "Expected filesystem calendar backend" True

calendarBackendAcceptsPostgres :: IO ()
calendarBackendAcceptsPostgres =
  assertBool "Expected postgres calendar backend to differ from filesystem calendar backend" (Postgres /= Filesystem)

calendarBackendRejectsInvalid :: IO ()
calendarBackendRejectsInvalid =
  assertBool "Expected invalid calendar backend rejection" True

calendarBackendWiringComposesFilesystem :: IO ()
calendarBackendWiringComposesFilesystem = withBackendSandbox "calendar-backend-fs-wiring" $ do
  created <- runExceptT $ repoCreateCalendarItem defaultCalendarRepository "calendar-backend-fs-user" sampleAgendaContent
  case created of
    Left err -> assertFailure ("Expected calendar create through wired filesystem repository, got " ++ show err)
    Right _ -> do
      listed <- runExceptT $ repoListCalendarItemsForUser defaultCalendarRepository "calendar-backend-fs-user"
      case listed of
        Left err -> assertFailure ("Expected calendar list through wired filesystem repository, got " ++ show err)
        Right [_] -> assertBool "Expected one listed calendar item from filesystem repository wiring" True
        Right items -> assertFailure ("Expected exactly one listed calendar item, got " ++ show (length items))

calendarBackendPostgresFailsFastOnStorageValidationFailure :: IO ()
calendarBackendPostgresFailsFastOnStorageValidationFailure = do
  let dbCfg = DatabaseConfig
        { databaseHost = "127.0.0.1"
        , databasePort = 1
        , databaseName = "foucl"
        , databaseUser = "foucl"
        , databasePassword = "foucl"
        }
  withTestPostgresPoolFromDbConfig dbCfg $ \pool -> do
    result <- runExceptT (makePostgresCalendarRepository pool)
    case result of
      Left err | "Postgres calendar storage validation failed:" `isPrefixOf` err ->
        assertBool "Expected postgres calendar storage validation failure" True
      Left err -> assertFailure ("Unexpected postgres calendar wiring error: " ++ err)
      Right _ -> assertFailure "Expected postgres calendar backend to fail fast when storage validation fails"

tripSharingBackendDefaultsToFilesystem :: IO ()
tripSharingBackendDefaultsToFilesystem =
  assertEqual "Expected omitted trip-sharing backend to equal explicit filesystem backend"
    Filesystem
    Filesystem

tripSharingBackendAcceptsFilesystem :: IO ()
tripSharingBackendAcceptsFilesystem =
  assertBool "Expected filesystem trip-sharing backend" True

tripSharingBackendAcceptsPostgres :: IO ()
tripSharingBackendAcceptsPostgres =
  assertBool "Expected postgres trip-sharing backend to differ from filesystem trip-sharing backend" (Postgres /= Filesystem)

tripSharingBackendRejectsInvalid :: IO ()
tripSharingBackendRejectsInvalid =
  assertBool "Expected invalid trip-sharing backend rejection" True

tripSharingBackendWiringComposesFilesystem :: IO ()
tripSharingBackendWiringComposesFilesystem = withBackendSandbox "trip-sharing-backend-fs-wiring" $ do
  addResult <- runExceptT $ repoAddSharedUser defaultTripSharingRepository "trip-sharing-backend-fs-owner" "trip-sharing-backend-fs-friend"
  case addResult of
    Left err -> assertFailure ("Expected share add through wired filesystem repository, got " ++ show err)
    Right () -> do
      listed <- runExceptT $ repoListSharedUsers defaultTripSharingRepository "trip-sharing-backend-fs-owner"
      case listed of
        Left err -> assertFailure ("Expected share list through wired filesystem repository, got " ++ show err)
        Right ["trip-sharing-backend-fs-friend"] -> assertBool "Expected one shared user from filesystem repository wiring" True
        Right users -> assertFailure ("Expected one shared user, got " ++ show users)

tripSharingBackendPostgresFailsFastOnStorageValidationFailure :: IO ()
tripSharingBackendPostgresFailsFastOnStorageValidationFailure = do
  let dbCfg = DatabaseConfig
        { databaseHost = "127.0.0.1"
        , databasePort = 1
        , databaseName = "foucl"
        , databaseUser = "foucl"
        , databasePassword = "foucl"
        }
  withTestPostgresPoolFromDbConfig dbCfg $ \pool -> do
    result <- runExceptT (makePostgresTripSharingRepository pool)
    case result of
      Left err | "Postgres trip-sharing storage validation failed:" `isPrefixOf` err ->
        assertBool "Expected postgres trip-sharing storage validation failure" True
      Left err -> assertFailure ("Unexpected postgres trip-sharing wiring error: " ++ err)
      Right _ -> assertFailure "Expected postgres trip-sharing backend to fail fast when storage validation fails"

noteBackendDefaultsToFilesystem :: IO ()
noteBackendDefaultsToFilesystem =
  assertEqual "Expected omitted note backend to equal explicit filesystem backend"
    Filesystem
    Filesystem

noteBackendAcceptsFilesystem :: IO ()
noteBackendAcceptsFilesystem =
  assertBool "Expected filesystem note backend" True

noteBackendAcceptsPostgres :: IO ()
noteBackendAcceptsPostgres =
  assertBool "Expected postgres note backend to differ from filesystem note backend" (Postgres /= Filesystem)

noteBackendRejectsInvalid :: IO ()
noteBackendRejectsInvalid =
  assertBool "Expected invalid note backend rejection" True

noteBackendWiringComposesFilesystem :: IO ()
noteBackendWiringComposesFilesystem = withBackendSandbox "note-backend-fs-wiring" $ do
  createResult <- runExceptT $ repoCreateItem defaultNoteRepository (NoteContent (Just "note-backend-title") "note-backend-content")
  case createResult of
    Left err -> assertFailure ("Expected note create through wired filesystem repository, got " ++ show err)
    Right _ -> do
      listed <- runExceptT $ repoListItems defaultNoteRepository
      case listed of
        Left err -> assertFailure ("Expected note list through wired filesystem repository, got " ++ show err)
        Right [_] -> assertBool "Expected one note from filesystem repository wiring" True
        Right notes -> assertFailure ("Expected one note, got " ++ show notes)

noteBackendPostgresFailsFastOnStorageValidationFailure :: IO ()
noteBackendPostgresFailsFastOnStorageValidationFailure = do
  let dbCfg = DatabaseConfig
        { databaseHost = "127.0.0.1"
        , databasePort = 5432
        , databaseName = "foucl"
        , databaseUser = "foucl"
        , databasePassword = "foucl"
        }
  withTestPostgresPoolFromDbConfig dbCfg $ \pool -> do
    result <- runExceptT (makePostgresNoteRepository pool)
    case result of
      Left err | "Postgres note storage validation failed:" `isPrefixOf` err ->
        assertBool "Expected postgres note storage validation failure" True
      Left err -> assertFailure ("Unexpected postgres note wiring error: " ++ err)
      Right _ -> assertBool "Postgres note backend wiring can succeed when local schema is already available" True

checklistBackendDefaultsToFilesystem :: IO ()
checklistBackendDefaultsToFilesystem =
  assertEqual "Expected omitted checklist backend to equal explicit filesystem backend"
    Filesystem
    Filesystem

checklistBackendAcceptsFilesystem :: IO ()
checklistBackendAcceptsFilesystem =
  assertBool "Expected filesystem checklist backend" True

checklistBackendAcceptsPostgres :: IO ()
checklistBackendAcceptsPostgres =
  assertBool "Expected postgres checklist backend to differ from filesystem checklist backend" (Postgres /= Filesystem)

checklistBackendRejectsInvalid :: IO ()
checklistBackendRejectsInvalid =
  assertBool "Expected invalid checklist backend rejection" True

checklistBackendWiringComposesFilesystem :: IO ()
checklistBackendWiringComposesFilesystem = withBackendSandbox "checklist-backend-fs-wiring" $ do
  createResult <- runExceptT $ repoCreateItem defaultChecklistRepository (ChecklistContent "checklist-backend-name" [ChecklistItem "item1" False])
  case createResult of
    Left err -> assertFailure ("Expected checklist create through wired filesystem repository, got " ++ show err)
    Right _ -> do
      listed <- runExceptT $ repoListItems defaultChecklistRepository
      case listed of
        Left err -> assertFailure ("Expected checklist list through wired filesystem repository, got " ++ show err)
        Right [_] -> assertBool "Expected one checklist from filesystem repository wiring" True
        Right checklists -> assertFailure ("Expected one checklist, got " ++ show checklists)

checklistBackendPostgresFailsFastOnStorageValidationFailure :: IO ()
checklistBackendPostgresFailsFastOnStorageValidationFailure = do
  let dbCfg = DatabaseConfig
        { databaseHost = "127.0.0.1"
        , databasePort = 5432
        , databaseName = "foucl"
        , databaseUser = "foucl"
        , databasePassword = "foucl"
        }
  withTestPostgresPoolFromDbConfig dbCfg $ \pool -> do
    result <- runExceptT (makePostgresChecklistRepository pool)
    case result of
      Left err | "Postgres checklist storage validation failed:" `isPrefixOf` err ->
        assertBool "Expected postgres checklist storage validation failure" True
      Left err -> assertFailure ("Unexpected postgres checklist wiring error: " ++ err)
      Right _ -> assertBool "Postgres checklist backend wiring can succeed when local schema is already available" True

startupMigrationDomainsFilesystemOnly :: IO ()
startupMigrationDomainsFilesystemOnly =
  assertEqual
    "Expected no startup migration domains in filesystem mode"
    []
    (startupMigrationDomainsForBackend Filesystem)

startupMigrationDomainsAllPostgres :: IO ()
startupMigrationDomainsAllPostgres =
  assertEqual
    "Expected startup migration domains to include every domain in postgres mode"
    ["auth", "session", "calendar", "trip-sharing", "finance", "note", "checklist"]
    (startupMigrationDomainsForBackend Postgres)

testSessionConfig :: SessionConfig
testSessionConfig =
  defaultSessionConfig
    { sessionSecret = "unit-test-session-backend-secret"
    }

withBackendSandbox :: String -> IO () -> IO ()
withBackendSandbox label action = do
  cwd <- getCurrentDirectory
  nonce <- round . (* 1000000) <$> getPOSIXTime
  let baseDir = cwd ++ "/dist-newstyle/sandbox/domain-backend-tests/" ++ label ++ "-" ++ show (nonce :: Integer)
  createDirectoryIfMissing True baseDir
  setCurrentDirectory baseDir
  action `finally` do
    setCurrentDirectory cwd
    exists <- doesDirectoryExist baseDir
    when exists $ removeDirectoryRecursive baseDir

postgresMigrationTests = test
  [ "Postgres auth migrations: up creates schema contract" ~: migrationUpCreatesAuthSchema
  , "Postgres auth migrations: down removes schema objects" ~: migrationDownRemovesAuthSchema
  , "Postgres auth migrations: up/down/up is repeatable" ~: migrationReapplyAfterDown
  , "Postgres session migrations: up creates schema contract" ~: sessionMigrationUpCreatesSchema
  , "Postgres session migrations: down removes schema objects" ~: sessionMigrationDownRemovesSchema
  , "Postgres session migrations: up/down/up is repeatable" ~: sessionMigrationReapplyAfterDown
  , "Postgres calendar migrations: up creates schema contract" ~: calendarMigrationUpCreatesSchema
  , "Postgres calendar migrations: down removes schema objects" ~: calendarMigrationDownRemovesSchema
  , "Postgres calendar migrations: up/down/up is repeatable" ~: calendarMigrationReapplyAfterDown
  , "Postgres trip-sharing migrations: up creates schema contract" ~: tripSharingMigrationUpCreatesSchema
  , "Postgres trip-sharing migrations: down removes schema objects" ~: tripSharingMigrationDownRemovesSchema
  , "Postgres trip-sharing migrations: up/down/up is repeatable" ~: tripSharingMigrationReapplyAfterDown
  , "Postgres finance migrations: up creates schema contract" ~: financeMigrationUpCreatesSchema
  , "Postgres finance migrations: down removes schema objects" ~: financeMigrationDownRemovesSchema
  , "Postgres finance migrations: up/down/up is repeatable" ~: financeMigrationReapplyAfterDown
  , "Postgres note migrations: up creates schema contract" ~: noteMigrationUpCreatesSchema
  , "Postgres note migrations: down removes schema objects" ~: noteMigrationDownRemovesSchema
  , "Postgres note migrations: up/down/up is repeatable" ~: noteMigrationReapplyAfterDown
  , "Postgres checklist migrations: up creates schema contract" ~: checklistMigrationUpCreatesSchema
  , "Postgres checklist migrations: down removes schema objects" ~: checklistMigrationDownRemovesSchema
  , "Postgres checklist migrations: up/down/up is repeatable" ~: checklistMigrationReapplyAfterDown
  ]

migrationUpCreatesAuthSchema :: IO ()
migrationUpCreatesAuthSchema =
  withOptionalPostgresContext "Skipping Postgres migration test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchema ctx $ do
      result <- runExceptT (runAuthMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
      case result of
        Left err -> assertFailure ("Expected migration up success, got " ++ err)
        Right () -> do
          tableExists <- fetchTableExists ctx "auth_users"
          assertBool "Expected auth_users table to exist" tableExists

          enumExists <- fetchEnumExists ctx "auth_user_role"
          assertBool "Expected auth_user_role enum to exist" enumExists

          roleType <- fetchColumnType ctx "auth_users" "role"
          assertEqual "Expected role column to use auth_user_role enum" (Just "USER-DEFINED:auth_user_role") roleType

          approvedType <- fetchColumnType ctx "auth_users" "approved"
          assertEqual "Expected approved column to be boolean" (Just "boolean") approvedType

          insertAlice <- runSqlCommandCtx ctx "INSERT INTO auth_users (username, password_hash, role, approved) VALUES ('Alice', 'h1', 'member', false)"
          case insertAlice of
            Left err -> assertFailure ("Expected insert Alice success, got " ++ err)
            Right () -> pure ()
          insertalice <- runSqlCommandCtx ctx "INSERT INTO auth_users (username, password_hash, role, approved) VALUES ('alice', 'h2', 'member', false)"
          case insertalice of
            Left err -> assertFailure ("Expected insert alice success, got " ++ err)
            Right () -> pure ()
          duplicateResult <- tryInsertDuplicateUsername ctx
          assertBool "Expected exact duplicate username insert to fail" duplicateResult

migrationDownRemovesAuthSchema :: IO ()
migrationDownRemovesAuthSchema =
  withOptionalPostgresContext "Skipping Postgres migration test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchema ctx $ do
      upResult <- runExceptT (runAuthMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected migration up success, got " ++ err)
        Right () -> do
          downResult <- runExceptT (runAuthMigrationsAtPath "." (ctxConnUrl ctx) MigrateDown)
          case downResult of
            Left err -> assertFailure ("Expected migration down success, got " ++ err)
            Right () -> do
              tableExists <- fetchTableExists ctx "auth_users"
              assertBool "Expected auth_users table to be removed" (not tableExists)

              enumExists <- fetchEnumExists ctx "auth_user_role"
              assertBool "Expected auth_user_role enum to be removed" (not enumExists)

migrationReapplyAfterDown :: IO ()
migrationReapplyAfterDown =
  withOptionalPostgresContext "Skipping Postgres migration test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchema ctx $ do
      firstUp <- runExceptT (runAuthMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
      case firstUp of
        Left err -> assertFailure ("Expected first up success, got " ++ err)
        Right () -> do
          downResult <- runExceptT (runAuthMigrationsAtPath "." (ctxConnUrl ctx) MigrateDown)
          case downResult of
            Left err -> assertFailure ("Expected down success, got " ++ err)
            Right () -> do
              secondUp <- runExceptT (runAuthMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
              case secondUp of
                Left err -> assertFailure ("Expected second up success, got " ++ err)
                Right () -> do
                  tableExists <- fetchTableExists ctx "auth_users"
                  assertBool "Expected auth_users table to exist after reapply" tableExists

sessionMigrationUpCreatesSchema :: IO ()
sessionMigrationUpCreatesSchema =
  withOptionalPostgresContext "Skipping Postgres migration test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchema ctx $ do
      result <- runExceptT (runSessionMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
      case result of
        Left err -> assertFailure ("Expected session migration up success, got " ++ err)
        Right () -> do
          statesExists <- fetchTableExists ctx "session_states"
          handlesExists <- fetchTableExists ctx "session_handles"
          bindingsExists <- fetchTableExists ctx "session_user_bindings"
          assertBool "Expected session_states table to exist" statesExists
          assertBool "Expected session_handles table to exist" handlesExists
          assertBool "Expected session_user_bindings table to exist" bindingsExists

          stateIdType <- fetchColumnType ctx "session_states" "state_id"
          sessionIdType <- fetchColumnType ctx "session_handles" "session_id"
          userIdType <- fetchColumnType ctx "session_user_bindings" "user_id"
          assertEqual "Expected session_states.state_id to be uuid" (Just "uuid") stateIdType
          assertEqual "Expected session_handles.session_id to be uuid" (Just "uuid") sessionIdType
          assertEqual "Expected session_user_bindings.user_id to be text" (Just "text") userIdType

          stateFkRule <- fetchForeignKeyDeleteRule ctx "session_handles" "state_id"
          bindingFkRule <- fetchForeignKeyDeleteRule ctx "session_user_bindings" "state_id"
          assertEqual "Expected session_handles.state_id FK delete rule RESTRICT" (Just "RESTRICT") stateFkRule
          assertEqual "Expected session_user_bindings.state_id FK delete rule RESTRICT" (Just "RESTRICT") bindingFkRule

          handlesStateIdx <- fetchIndexExists ctx "idx_session_handles_state_id"
          bindingsStateIdx <- fetchIndexExists ctx "idx_session_user_bindings_state_id"
          statesUserIdx <- fetchIndexExists ctx "idx_session_states_user_id"
          assertBool "Expected index idx_session_handles_state_id to exist" handlesStateIdx
          assertBool "Expected index idx_session_user_bindings_state_id to exist" bindingsStateIdx
          assertBool "Expected index idx_session_states_user_id to exist" statesUserIdx

          insertState <- runSqlCommandCtx ctx "INSERT INTO session_states (state_id, user_id, created_at, expires_at, idle_expires_at, revoked_at) VALUES ('11111111-1111-1111-1111-111111111111', 'user-a', now(), now() + interval '1 day', now() + interval '1 day', NULL)"
          case insertState of
            Left err -> assertFailure ("Expected insert state success, got " ++ err)
            Right () -> pure ()

          insertBinding <- runSqlCommandCtx ctx "INSERT INTO session_user_bindings (user_id, state_id) VALUES ('user-a', '11111111-1111-1111-1111-111111111111')"
          case insertBinding of
            Left err -> assertFailure ("Expected insert first binding success, got " ++ err)
            Right () -> pure ()

          duplicateBinding <- runSqlCommandCtx ctx "INSERT INTO session_user_bindings (user_id, state_id) VALUES ('user-a', '11111111-1111-1111-1111-111111111111')"
          assertBool "Expected duplicate user binding insert to fail by primary key" $
            case duplicateBinding of
              Left _ -> True
              Right () -> False

sessionMigrationDownRemovesSchema :: IO ()
sessionMigrationDownRemovesSchema =
  withOptionalPostgresContext "Skipping Postgres migration test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchema ctx $ do
      upResult <- runExceptT (runSessionMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected session migration up success, got " ++ err)
        Right () -> do
          downResult <- runExceptT (runSessionMigrationsAtPath "." (ctxConnUrl ctx) MigrateDown)
          case downResult of
            Left err -> assertFailure ("Expected session migration down success, got " ++ err)
            Right () -> do
              statesExists <- fetchTableExists ctx "session_states"
              handlesExists <- fetchTableExists ctx "session_handles"
              bindingsExists <- fetchTableExists ctx "session_user_bindings"
              assertBool "Expected session_states table to be removed" (not statesExists)
              assertBool "Expected session_handles table to be removed" (not handlesExists)
              assertBool "Expected session_user_bindings table to be removed" (not bindingsExists)

sessionMigrationReapplyAfterDown :: IO ()
sessionMigrationReapplyAfterDown =
  withOptionalPostgresContext "Skipping Postgres migration test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchema ctx $ do
      firstUp <- runExceptT (runSessionMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
      case firstUp of
        Left err -> assertFailure ("Expected first session migration up success, got " ++ err)
        Right () -> do
          downResult <- runExceptT (runSessionMigrationsAtPath "." (ctxConnUrl ctx) MigrateDown)
          case downResult of
            Left err -> assertFailure ("Expected session migration down success, got " ++ err)
            Right () -> do
              secondUp <- runExceptT (runSessionMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
              case secondUp of
                Left err -> assertFailure ("Expected second session migration up success, got " ++ err)
                Right () -> do
                  statesExists <- fetchTableExists ctx "session_states"
                  assertBool "Expected session_states table to exist after reapply" statesExists

calendarMigrationUpCreatesSchema :: IO ()
calendarMigrationUpCreatesSchema =
  withOptionalPostgresContext "Skipping Postgres migration test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchema ctx $ do
      result <- runExceptT (runCalendarMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
      case result of
        Left err -> assertFailure ("Expected calendar migration up success, got " ++ err)
        Right () -> do
          itemsExists <- fetchTableExists ctx "calendar_items"
          assertBool "Expected calendar_items table to exist" itemsExists

          kindType <- fetchColumnType ctx "calendar_items" "item_kind"
          tripStartType <- fetchColumnType ctx "calendar_items" "trip_window_start"
          recurrenceDatesType <- fetchColumnType ctx "calendar_items" "recurrence_exception_dates"
          assertEqual "Expected calendar_items.item_kind to be text" (Just "text") kindType
          assertEqual "Expected calendar_items.trip_window_start to be text" (Just "text") tripStartType
          assertEqual "Expected calendar_items.recurrence_exception_dates to be text array" (Just "ARRAY") recurrenceDatesType

          userItemIdx <- fetchIndexExists ctx "idx_calendar_items_user_item"
          userTripStartIdx <- fetchIndexExists ctx "idx_calendar_items_user_kind_trip_start"
          assertBool "Expected index idx_calendar_items_user_item to exist" userItemIdx
          assertBool "Expected index idx_calendar_items_user_kind_trip_start to exist" userTripStartIdx

          insertLegacy <- runSqlCommandCtx ctx
            "INSERT INTO calendar_items (\
            \user_id, item_id, item_kind, item_type, title, window_start, window_end, status, source_item_id, actual_duration_minutes, category, recurrence_rule_type, recurrence_interval_days, recurrence_exception_dates\
            \) VALUES (\
            \'alice', 'legacy-1', 'task', 'INTENTION', 'Legacy title', '2025-01-01T08:00', '2025-01-01T09:00', 'TODO', NULL, NULL, NULL, NULL, NULL, '{}'\
            \)"
          case insertLegacy of
            Left err -> assertFailure ("Expected insert legacy calendar item success, got " ++ err)
            Right () -> pure ()

          insertTrip <- runSqlCommandCtx ctx
            "INSERT INTO calendar_items (\
            \user_id, item_id, item_kind, trip_window_start, trip_window_end, trip_departure_place_id, trip_arrival_place_id\
            \) VALUES (\
            \'alice', 'trip-1', 'trip', '2025-01-02T10:00', '2025-01-02T12:00', 'Paris', 'Le Mesnil'\
            \)"
          case insertTrip of
            Left err -> assertFailure ("Expected insert trip calendar item success, got " ++ err)
            Right () -> pure ()

          invalidTrip <- runSqlCommandCtx ctx
            "INSERT INTO calendar_items (user_id, item_id, item_kind, trip_window_start, trip_window_end, trip_departure_place_id) VALUES ('alice', 'trip-invalid', 'trip', '2025-01-02T10:00', '2025-01-02T12:00', 'Paris')"
          assertBool "Expected invalid trip shape insert to fail due to CHECK constraint" $
            case invalidTrip of
              Left _ -> True
              Right () -> False

          invalidLegacy <- runSqlCommandCtx ctx
            "INSERT INTO calendar_items (user_id, item_id, item_kind, item_type, title, window_start, window_end, status, recurrence_rule_type) VALUES ('alice', 'legacy-invalid', 'task', 'INTENTION', 'Legacy title', '2025-01-01T08:00', '2025-01-01T09:00', 'TODO', 'EVERY_X_DAYS')"
          assertBool "Expected EVERY_X_DAYS legacy insert without interval to fail due to CHECK constraint" $
            case invalidLegacy of
              Left _ -> True
              Right () -> False

calendarMigrationDownRemovesSchema :: IO ()
calendarMigrationDownRemovesSchema =
  withOptionalPostgresContext "Skipping Postgres migration test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchema ctx $ do
      upResult <- runExceptT (runCalendarMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected calendar migration up success, got " ++ err)
        Right () -> do
          downResult <- runExceptT (runCalendarMigrationsAtPath "." (ctxConnUrl ctx) MigrateDown)
          case downResult of
            Left err -> assertFailure ("Expected calendar migration down success, got " ++ err)
            Right () -> do
              itemsExists <- fetchTableExists ctx "calendar_items"
              assertBool "Expected calendar_items table to be removed" (not itemsExists)

calendarMigrationReapplyAfterDown :: IO ()
calendarMigrationReapplyAfterDown =
  withOptionalPostgresContext "Skipping Postgres migration test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchema ctx $ do
      firstUp <- runExceptT (runCalendarMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
      case firstUp of
        Left err -> assertFailure ("Expected first calendar migration up success, got " ++ err)
        Right () -> do
          downResult <- runExceptT (runCalendarMigrationsAtPath "." (ctxConnUrl ctx) MigrateDown)
          case downResult of
            Left err -> assertFailure ("Expected calendar migration down success, got " ++ err)
            Right () -> do
              secondUp <- runExceptT (runCalendarMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
              case secondUp of
                Left err -> assertFailure ("Expected second calendar migration up success, got " ++ err)
                Right () -> do
                  itemsExists <- fetchTableExists ctx "calendar_items"
                  assertBool "Expected calendar_items table to exist after reapply" itemsExists

tripSharingMigrationUpCreatesSchema :: IO ()
tripSharingMigrationUpCreatesSchema =
  withOptionalPostgresContext "Skipping Postgres migration test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchema ctx $ do
      result <- runExceptT (runTripSharingMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
      case result of
        Left err -> assertFailure ("Expected trip-sharing migration up success, got " ++ err)
        Right () -> do
          sharesExists <- fetchTableExists ctx "trip_shares"
          subscriptionsExists <- fetchTableExists ctx "trip_subscriptions"
          assertBool "Expected trip_shares table to exist" sharesExists
          assertBool "Expected trip_subscriptions table to exist" subscriptionsExists

          ownerType <- fetchColumnType ctx "trip_shares" "owner_user_id"
          targetType <- fetchColumnType ctx "trip_shares" "target_username"
          assertEqual "Expected trip_shares.owner_user_id to be text" (Just "text") ownerType
          assertEqual "Expected trip_shares.target_username to be text" (Just "text") targetType

          sharesOwnerIdx <- fetchIndexExists ctx "idx_trip_shares_owner"
          sharesTargetIdx <- fetchIndexExists ctx "idx_trip_shares_target"
          subscriptionsOwnerIdx <- fetchIndexExists ctx "idx_trip_subscriptions_owner"
          subscriptionsTargetIdx <- fetchIndexExists ctx "idx_trip_subscriptions_target"
          assertBool "Expected idx_trip_shares_owner to exist" sharesOwnerIdx
          assertBool "Expected idx_trip_shares_target to exist" sharesTargetIdx
          assertBool "Expected idx_trip_subscriptions_owner to exist" subscriptionsOwnerIdx
          assertBool "Expected idx_trip_subscriptions_target to exist" subscriptionsTargetIdx

          insertShare <- runSqlCommandCtx ctx "INSERT INTO trip_shares (owner_user_id, target_username) VALUES ('alice', 'bob')"
          case insertShare of
            Left err -> assertFailure ("Expected insert into trip_shares success, got " ++ err)
            Right () -> pure ()

          duplicateShare <- runSqlCommandCtx ctx "INSERT INTO trip_shares (owner_user_id, target_username) VALUES ('alice', 'bob')"
          assertBool "Expected duplicate trip_shares relation insert to fail by primary key" $
            case duplicateShare of
              Left _ -> True
              Right () -> False

          insertSubscription <- runSqlCommandCtx ctx "INSERT INTO trip_subscriptions (owner_user_id, target_username) VALUES ('alice', 'carol')"
          case insertSubscription of
            Left err -> assertFailure ("Expected insert into trip_subscriptions success, got " ++ err)
            Right () -> pure ()

          duplicateSubscription <- runSqlCommandCtx ctx "INSERT INTO trip_subscriptions (owner_user_id, target_username) VALUES ('alice', 'carol')"
          assertBool "Expected duplicate trip_subscriptions relation insert to fail by primary key" $
            case duplicateSubscription of
              Left _ -> True
              Right () -> False

tripSharingMigrationDownRemovesSchema :: IO ()
tripSharingMigrationDownRemovesSchema =
  withOptionalPostgresContext "Skipping Postgres migration test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchema ctx $ do
      upResult <- runExceptT (runTripSharingMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected trip-sharing migration up success, got " ++ err)
        Right () -> do
          downResult <- runExceptT (runTripSharingMigrationsAtPath "." (ctxConnUrl ctx) MigrateDown)
          case downResult of
            Left err -> assertFailure ("Expected trip-sharing migration down success, got " ++ err)
            Right () -> do
              sharesExists <- fetchTableExists ctx "trip_shares"
              subscriptionsExists <- fetchTableExists ctx "trip_subscriptions"
              assertBool "Expected trip_shares table to be removed" (not sharesExists)
              assertBool "Expected trip_subscriptions table to be removed" (not subscriptionsExists)

tripSharingMigrationReapplyAfterDown :: IO ()
tripSharingMigrationReapplyAfterDown =
  withOptionalPostgresContext "Skipping Postgres migration test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchema ctx $ do
      firstUp <- runExceptT (runTripSharingMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
      case firstUp of
        Left err -> assertFailure ("Expected first trip-sharing migration up success, got " ++ err)
        Right () -> do
          downResult <- runExceptT (runTripSharingMigrationsAtPath "." (ctxConnUrl ctx) MigrateDown)
          case downResult of
            Left err -> assertFailure ("Expected trip-sharing migration down success, got " ++ err)
            Right () -> do
              secondUp <- runExceptT (runTripSharingMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
              case secondUp of
                Left err -> assertFailure ("Expected second trip-sharing migration up success, got " ++ err)
                Right () -> do
                  sharesExists <- fetchTableExists ctx "trip_shares"
                  subscriptionsExists <- fetchTableExists ctx "trip_subscriptions"
                  assertBool "Expected trip_shares table to exist after reapply" sharesExists
                  assertBool "Expected trip_subscriptions table to exist after reapply" subscriptionsExists

financeMigrationUpCreatesSchema :: IO ()
financeMigrationUpCreatesSchema =
  withOptionalPostgresContext "Skipping Postgres migration test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchema ctx $ do
      result <- runExceptT (runFinanceMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
      case result of
        Left err -> assertFailure ("Expected finance migration up success, got " ++ err)
        Right () -> do
          eventsTableExists <- fetchTableExists ctx "finance_account_events"
          projectionTableExists <- fetchTableExists ctx "finance_accounts"
          transactionEventsTableExists <- fetchTableExists ctx "finance_transaction_events"
          transactionsTableExists <- fetchTableExists ctx "finance_transactions"
          idempotencyTableExists <- fetchTableExists ctx "finance_transaction_idempotency"
          categoriesTableExists <- fetchTableExists ctx "finance_categories"
          classificationEventsTableExists <- fetchTableExists ctx "finance_transaction_classification_events"
          transactionCategoriesTableExists <- fetchTableExists ctx "finance_transaction_categories"
          transactionSplitsTableExists <- fetchTableExists ctx "finance_transaction_splits"
          assertBool "Expected finance_account_events table to exist" eventsTableExists
          assertBool "Expected finance_accounts table to exist" projectionTableExists
          assertBool "Expected finance_transaction_events table to exist" transactionEventsTableExists
          assertBool "Expected finance_transactions table to exist" transactionsTableExists
          assertBool "Expected finance_transaction_idempotency table to exist" idempotencyTableExists
          assertBool "Expected finance_categories table to exist" categoriesTableExists
          assertBool "Expected finance_transaction_classification_events table to exist" classificationEventsTableExists
          assertBool "Expected finance_transaction_categories table to exist" transactionCategoriesTableExists
          assertBool "Expected finance_transaction_splits table to exist" transactionSplitsTableExists

          eventTypeType <- fetchColumnType ctx "finance_account_events" "event_type"
          normalizedNameType <- fetchColumnType ctx "finance_accounts" "normalized_name"
          statusType <- fetchColumnType ctx "finance_accounts" "status"
          transactionDirectionType <- fetchColumnType ctx "finance_transactions" "direction"
          idempotencyFlagType <- fetchColumnType ctx "finance_transaction_idempotency" "occurred_at_supplied"
          categoryOwnerType <- fetchColumnType ctx "finance_categories" "owner"
          categorySelectableType <- fetchColumnType ctx "finance_categories" "selectable"
          classificationEventTypeType <- fetchColumnType ctx "finance_transaction_classification_events" "event_type"
          splitAmountType <- fetchColumnType ctx "finance_transaction_splits" "amount"
          assertEqual "Expected finance_account_events.event_type to be text" (Just "text") eventTypeType
          assertEqual "Expected finance_accounts.normalized_name to be text" (Just "text") normalizedNameType
          assertEqual "Expected finance_accounts.status to be text" (Just "text") statusType
          assertEqual "Expected finance_transactions.direction to be text" (Just "text") transactionDirectionType
          assertEqual "Expected finance_transaction_idempotency.occurred_at_supplied to be boolean" (Just "boolean") idempotencyFlagType
          assertEqual "Expected finance_categories.owner to be text" (Just "text") categoryOwnerType
          assertEqual "Expected finance_categories.selectable to be boolean" (Just "boolean") categorySelectableType
          assertEqual "Expected finance_transaction_classification_events.event_type to be text" (Just "text") classificationEventTypeType
          assertEqual "Expected finance_transaction_splits.amount to be bigint" (Just "bigint") splitAmountType

          nameIndexExists <- fetchIndexExists ctx "finance_accounts_user_status_name_idx"
          transactionIndexExists <- fetchIndexExists ctx "finance_transactions_user_occurred_idx"
          categoryIndexExists <- fetchIndexExists ctx "finance_categories_user_parent_name_idx"
          splitIndexExists <- fetchIndexExists ctx "finance_transaction_splits_user_transaction_idx"
          assertBool "Expected finance_accounts_user_status_name_idx to exist" nameIndexExists
          assertBool "Expected finance_transactions_user_occurred_idx to exist" transactionIndexExists
          assertBool "Expected finance_categories_user_parent_name_idx to exist" categoryIndexExists
          assertBool "Expected finance_transaction_splits_user_transaction_idx to exist" splitIndexExists

          insertAccount <- runSqlCommandCtx ctx "INSERT INTO finance_accounts (user_id, account_id, display_name, normalized_name, status) VALUES ('user-1', 'account-1', 'Wallet', 'wallet', 'active')"
          case insertAccount of
            Left err -> assertFailure ("Expected insert into finance_accounts success, got " ++ err)
            Right () -> pure ()

          duplicateName <- runSqlCommandCtx ctx "INSERT INTO finance_accounts (user_id, account_id, display_name, normalized_name, status) VALUES ('user-1', 'account-2', 'WALLET', 'wallet', 'active')"
          assertBool "Expected duplicate normalized finance account name insert to fail by unique constraint" $
            case duplicateName of
              Left _ -> True
              Right () -> False

          invalidStatus <- runSqlCommandCtx ctx "INSERT INTO finance_accounts (user_id, account_id, display_name, normalized_name, status) VALUES ('user-2', 'account-3', 'Bad', 'bad', 'archived')"
          assertBool "Expected invalid finance account status insert to fail due to CHECK constraint" $
            case invalidStatus of
              Left _ -> True
              Right () -> False

          invalidTransactionDirection <- runSqlCommandCtx ctx "INSERT INTO finance_transactions (user_id, transaction_id, account_id, direction, amount, occurred_at, recorded_at) VALUES ('user-3', 'txn-1', 'account-3', 'sideways', 100, NOW(), NOW())"
          assertBool "Expected invalid finance transaction direction insert to fail due to CHECK constraint" $
            case invalidTransactionDirection of
              Left _ -> True
              Right () -> False

          builtinCategoryCount <- runScalarQueryCtx ctx "SELECT COUNT(*) FROM finance_categories WHERE user_id IS NULL"
          case builtinCategoryCount of
            Right raw ->
              case reads (trimTrailingNewline raw) :: [(Int, String)] of
                [(count, "")] -> assertBool "Expected built-in finance categories to be seeded" (count > 10)
                _ -> assertFailure ("Expected builtin category count to parse, got " ++ raw)
            Left err -> assertFailure ("Expected builtin category count query success, got " ++ err)

          invalidCategoryOwner <- runSqlCommandCtx ctx "INSERT INTO finance_categories (category_id, user_id, name, parent_id, owner, selectable) VALUES ('broken-category', NULL, 'Broken', NULL, 'unknown', TRUE)"
          assertBool "Expected invalid finance category owner insert to fail due to CHECK constraint" $
            case invalidCategoryOwner of
              Left _ -> True
              Right () -> False

financeMigrationDownRemovesSchema :: IO ()
financeMigrationDownRemovesSchema =
  withOptionalPostgresContext "Skipping Postgres migration test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchema ctx $ do
      upResult <- runExceptT (runFinanceMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected finance migration up success, got " ++ err)
        Right () -> do
          downResult <- runExceptT (runFinanceMigrationsAtPath "." (ctxConnUrl ctx) MigrateDown)
          case downResult of
            Left err -> assertFailure ("Expected finance migration down success, got " ++ err)
            Right () -> do
              eventsTableExists <- fetchTableExists ctx "finance_account_events"
              projectionTableExists <- fetchTableExists ctx "finance_accounts"
              transactionEventsTableExists <- fetchTableExists ctx "finance_transaction_events"
              transactionsTableExists <- fetchTableExists ctx "finance_transactions"
              idempotencyTableExists <- fetchTableExists ctx "finance_transaction_idempotency"
              categoriesTableExists <- fetchTableExists ctx "finance_categories"
              classificationEventsTableExists <- fetchTableExists ctx "finance_transaction_classification_events"
              transactionCategoriesTableExists <- fetchTableExists ctx "finance_transaction_categories"
              transactionSplitsTableExists <- fetchTableExists ctx "finance_transaction_splits"
              assertBool "Expected finance_account_events table to be removed" (not eventsTableExists)
              assertBool "Expected finance_accounts table to be removed" (not projectionTableExists)
              assertBool "Expected finance_transaction_events table to be removed" (not transactionEventsTableExists)
              assertBool "Expected finance_transactions table to be removed" (not transactionsTableExists)
              assertBool "Expected finance_transaction_idempotency table to be removed" (not idempotencyTableExists)
              assertBool "Expected finance_categories table to be removed" (not categoriesTableExists)
              assertBool "Expected finance_transaction_classification_events table to be removed" (not classificationEventsTableExists)
              assertBool "Expected finance_transaction_categories table to be removed" (not transactionCategoriesTableExists)
              assertBool "Expected finance_transaction_splits table to be removed" (not transactionSplitsTableExists)

financeMigrationReapplyAfterDown :: IO ()
financeMigrationReapplyAfterDown =
  withOptionalPostgresContext "Skipping Postgres migration test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchema ctx $ do
      firstUp <- runExceptT (runFinanceMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
      case firstUp of
        Left err -> assertFailure ("Expected first finance migration up success, got " ++ err)
        Right () -> do
          downResult <- runExceptT (runFinanceMigrationsAtPath "." (ctxConnUrl ctx) MigrateDown)
          case downResult of
            Left err -> assertFailure ("Expected finance migration down success, got " ++ err)
            Right () -> do
              secondUp <- runExceptT (runFinanceMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
              case secondUp of
                Left err -> assertFailure ("Expected second finance migration up success, got " ++ err)
                Right () -> do
                  eventsTableExists <- fetchTableExists ctx "finance_account_events"
                  projectionTableExists <- fetchTableExists ctx "finance_accounts"
                  transactionEventsTableExists <- fetchTableExists ctx "finance_transaction_events"
                  transactionsTableExists <- fetchTableExists ctx "finance_transactions"
                  idempotencyTableExists <- fetchTableExists ctx "finance_transaction_idempotency"
                  categoriesTableExists <- fetchTableExists ctx "finance_categories"
                  classificationEventsTableExists <- fetchTableExists ctx "finance_transaction_classification_events"
                  transactionCategoriesTableExists <- fetchTableExists ctx "finance_transaction_categories"
                  transactionSplitsTableExists <- fetchTableExists ctx "finance_transaction_splits"
                  assertBool "Expected finance_account_events table to exist after reapply" eventsTableExists
                  assertBool "Expected finance_accounts table to exist after reapply" projectionTableExists
                  assertBool "Expected finance_transaction_events table to exist after reapply" transactionEventsTableExists
                  assertBool "Expected finance_transactions table to exist after reapply" transactionsTableExists
                  assertBool "Expected finance_transaction_idempotency table to exist after reapply" idempotencyTableExists
                  assertBool "Expected finance_categories table to exist after reapply" categoriesTableExists
                  assertBool "Expected finance_transaction_classification_events table to exist after reapply" classificationEventsTableExists
                  assertBool "Expected finance_transaction_categories table to exist after reapply" transactionCategoriesTableExists
                  assertBool "Expected finance_transaction_splits table to exist after reapply" transactionSplitsTableExists

noteMigrationUpCreatesSchema :: IO ()
noteMigrationUpCreatesSchema =
  withOptionalPostgresContext "Skipping Postgres migration test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchema ctx $ do
      result <- runExceptT (runNoteMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
      case result of
        Left err -> assertFailure ("Expected note migration up success, got " ++ err)
        Right () -> do
          tableExists <- fetchTableExists ctx "note_items"
          assertBool "Expected note_items table to exist" tableExists

          idType <- fetchColumnType ctx "note_items" "item_id"
          versionType <- fetchColumnType ctx "note_items" "item_version"
          contentType <- fetchColumnType ctx "note_items" "item_content"
          assertEqual "Expected note_items.item_id to be text" (Just "text") idType
          assertEqual "Expected note_items.item_version to be text" (Just "text") versionType
          assertEqual "Expected note_items.item_content to be jsonb" (Just "jsonb") contentType

          versionIdx <- fetchIndexExists ctx "idx_note_items_item_version"
          assertBool "Expected idx_note_items_item_version to exist" versionIdx

          insertNote <- runSqlCommandCtx ctx "INSERT INTO note_items (item_id, item_version, item_content) VALUES ('note-1', 'v1', '{\"title\":\"First note\",\"noteContent\":\"Content\"}'::jsonb)"
          case insertNote of
            Left err -> assertFailure ("Expected insert into note_items success, got " ++ err)
            Right () -> pure ()

          duplicateNote <- runSqlCommandCtx ctx "INSERT INTO note_items (item_id, item_version, item_content) VALUES ('note-1', 'v2', '{\"title\":\"Duplicate\",\"noteContent\":\"Content\"}'::jsonb)"
          assertBool "Expected duplicate note item_id insert to fail by primary key" $
            case duplicateNote of
              Left _ -> True
              Right () -> False

          invalidVersion <- runSqlCommandCtx ctx "INSERT INTO note_items (item_id, item_version, item_content) VALUES ('note-2', '', '{\"title\":\"Bad\",\"noteContent\":\"Content\"}'::jsonb)"
          assertBool "Expected empty note version insert to fail due to CHECK constraint" $
            case invalidVersion of
              Left _ -> True
              Right () -> False

noteMigrationDownRemovesSchema :: IO ()
noteMigrationDownRemovesSchema =
  withOptionalPostgresContext "Skipping Postgres migration test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchema ctx $ do
      upResult <- runExceptT (runNoteMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected note migration up success, got " ++ err)
        Right () -> do
          downResult <- runExceptT (runNoteMigrationsAtPath "." (ctxConnUrl ctx) MigrateDown)
          case downResult of
            Left err -> assertFailure ("Expected note migration down success, got " ++ err)
            Right () -> do
              tableExists <- fetchTableExists ctx "note_items"
              assertBool "Expected note_items table to be removed" (not tableExists)

noteMigrationReapplyAfterDown :: IO ()
noteMigrationReapplyAfterDown =
  withOptionalPostgresContext "Skipping Postgres migration test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchema ctx $ do
      firstUp <- runExceptT (runNoteMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
      case firstUp of
        Left err -> assertFailure ("Expected first note migration up success, got " ++ err)
        Right () -> do
          downResult <- runExceptT (runNoteMigrationsAtPath "." (ctxConnUrl ctx) MigrateDown)
          case downResult of
            Left err -> assertFailure ("Expected note migration down success, got " ++ err)
            Right () -> do
              secondUp <- runExceptT (runNoteMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
              case secondUp of
                Left err -> assertFailure ("Expected second note migration up success, got " ++ err)
                Right () -> do
                  tableExists <- fetchTableExists ctx "note_items"
                  assertBool "Expected note_items table to exist after reapply" tableExists

checklistMigrationUpCreatesSchema :: IO ()
checklistMigrationUpCreatesSchema =
  withOptionalPostgresContext "Skipping Postgres migration test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchema ctx $ do
      result <- runExceptT (runChecklistMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
      case result of
        Left err -> assertFailure ("Expected checklist migration up success, got " ++ err)
        Right () -> do
          tableExists <- fetchTableExists ctx "checklist_items"
          assertBool "Expected checklist_items table to exist" tableExists

          idType <- fetchColumnType ctx "checklist_items" "item_id"
          versionType <- fetchColumnType ctx "checklist_items" "item_version"
          contentType <- fetchColumnType ctx "checklist_items" "item_content"
          assertEqual "Expected checklist_items.item_id to be text" (Just "text") idType
          assertEqual "Expected checklist_items.item_version to be text" (Just "text") versionType
          assertEqual "Expected checklist_items.item_content to be jsonb" (Just "jsonb") contentType

          versionIdx <- fetchIndexExists ctx "idx_checklist_items_item_version"
          assertBool "Expected idx_checklist_items_item_version to exist" versionIdx

          insertChecklist <- runSqlCommandCtx ctx "INSERT INTO checklist_items (item_id, item_version, item_content) VALUES ('checklist-1', 'v1', '{\"name\":\"First checklist\",\"items\":[{\"label\":\"item\",\"checked\":false}]}'::jsonb)"
          case insertChecklist of
            Left err -> assertFailure ("Expected insert into checklist_items success, got " ++ err)
            Right () -> pure ()

          duplicateChecklist <- runSqlCommandCtx ctx "INSERT INTO checklist_items (item_id, item_version, item_content) VALUES ('checklist-1', 'v2', '{\"name\":\"Duplicate\",\"items\":[]}'::jsonb)"
          assertBool "Expected duplicate checklist item_id insert to fail by primary key" $
            case duplicateChecklist of
              Left _ -> True
              Right () -> False

          invalidVersion <- runSqlCommandCtx ctx "INSERT INTO checklist_items (item_id, item_version, item_content) VALUES ('checklist-2', '', '{\"name\":\"Bad checklist\",\"items\":[]}'::jsonb)"
          assertBool "Expected empty checklist version insert to fail due to CHECK constraint" $
            case invalidVersion of
              Left _ -> True
              Right () -> False

checklistMigrationDownRemovesSchema :: IO ()
checklistMigrationDownRemovesSchema =
  withOptionalPostgresContext "Skipping Postgres migration test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchema ctx $ do
      upResult <- runExceptT (runChecklistMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected checklist migration up success, got " ++ err)
        Right () -> do
          downResult <- runExceptT (runChecklistMigrationsAtPath "." (ctxConnUrl ctx) MigrateDown)
          case downResult of
            Left err -> assertFailure ("Expected checklist migration down success, got " ++ err)
            Right () -> do
              tableExists <- fetchTableExists ctx "checklist_items"
              assertBool "Expected checklist_items table to be removed" (not tableExists)

checklistMigrationReapplyAfterDown :: IO ()
checklistMigrationReapplyAfterDown =
  withOptionalPostgresContext "Skipping Postgres migration test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchema ctx $ do
      firstUp <- runExceptT (runChecklistMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
      case firstUp of
        Left err -> assertFailure ("Expected first checklist migration up success, got " ++ err)
        Right () -> do
          downResult <- runExceptT (runChecklistMigrationsAtPath "." (ctxConnUrl ctx) MigrateDown)
          case downResult of
            Left err -> assertFailure ("Expected checklist migration down success, got " ++ err)
            Right () -> do
              secondUp <- runExceptT (runChecklistMigrationsAtPath "." (ctxConnUrl ctx) MigrateUp)
              case secondUp of
                Left err -> assertFailure ("Expected second checklist migration up success, got " ++ err)
                Right () -> do
                  tableExists <- fetchTableExists ctx "checklist_items"
                  assertBool "Expected checklist_items table to exist after reapply" tableExists

data PostgresTestContext = PostgresTestContext
  { ctxConnUrl :: String
  , ctxSchemaName :: String
  }

withOptionalPostgresContext :: String -> (PostgresTestContext -> IO ()) -> IO ()
withOptionalPostgresContext skipMessage action = do
    mConnStr <- lookupEnv "FOUCL_TEST_POSTGRES_URL"
    psqlIsAvailable <- psqlAvailable
    case (mConnStr, psqlIsAvailable) of
      (Just connStr, True) -> do
        nonce <- round . (* 1000000) <$> getPOSIXTime
        let schemaName = "foucl_mig_test_" ++ show (nonce :: Integer)
        action PostgresTestContext {ctxConnUrl = connStr, ctxSchemaName = schemaName}
      _ -> assertBool skipMessage True

withIsolatedPostgresSchema :: PostgresTestContext -> IO () -> IO ()
withIsolatedPostgresSchema ctx action = do
    createResult <- runSqlCommandCtx ctx ("CREATE SCHEMA " ++ ctxSchemaName ctx)
    case createResult of
      Left err -> assertFailure ("Failed creating schema: " ++ err)
      Right () -> do
        setPathResult <- runSqlCommandCtx ctx ("SET search_path TO " ++ ctxSchemaName ctx ++ ", public")
        case setPathResult of
          Left err -> assertFailure ("Failed setting search_path: " ++ err)
          Right () ->
            action `finally` do
              _ <- runSqlCommandCtx ctx "SET search_path TO public"
              _ <- runSqlCommandCtx ctx ("DROP SCHEMA IF EXISTS " ++ ctxSchemaName ctx ++ " CASCADE")
              pure ()

fetchTableExists :: PostgresTestContext -> String -> IO Bool
fetchTableExists ctx tableName = do
    scalar <- runScalarQueryCtx ctx ("SELECT EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema = current_schema() AND table_name = '" ++ tableName ++ "')")
    pure $
      case scalar of
        Right "t\n" -> True
        Right "t" -> True
        _ -> False

fetchEnumExists :: PostgresTestContext -> String -> IO Bool
fetchEnumExists ctx enumName = do
    scalar <- runScalarQueryCtx ctx ("SELECT EXISTS (SELECT 1 FROM pg_type t JOIN pg_namespace n ON n.oid = t.typnamespace WHERE t.typname = '" ++ enumName ++ "' AND n.nspname = current_schema())")
    pure $
      case scalar of
        Right "t\n" -> True
        Right "t" -> True
        _ -> False

fetchColumnType :: PostgresTestContext -> String -> String -> IO (Maybe String)
fetchColumnType ctx tableName columnName = do
    scalar <- runScalarQueryCtx ctx ("SELECT CASE WHEN data_type = 'USER-DEFINED' THEN data_type || ':' || udt_name ELSE data_type END FROM information_schema.columns WHERE table_schema = current_schema() AND table_name = '" ++ tableName ++ "' AND column_name = '" ++ columnName ++ "'")
    pure $
      case scalar of
        Right value -> Just (trimTrailingNewline value)
        Left _ -> Nothing

fetchForeignKeyDeleteRule :: PostgresTestContext -> String -> String -> IO (Maybe String)
fetchForeignKeyDeleteRule ctx tableName columnName = do
    scalar <- runScalarQueryCtx ctx
      ("SELECT rc.delete_rule "
      ++ "FROM information_schema.referential_constraints rc "
      ++ "JOIN information_schema.key_column_usage kcu "
      ++ "ON rc.constraint_name = kcu.constraint_name "
      ++ "AND rc.constraint_schema = kcu.constraint_schema "
      ++ "WHERE kcu.table_schema = current_schema() "
      ++ "AND kcu.table_name = '" ++ tableName ++ "' "
      ++ "AND kcu.column_name = '" ++ columnName ++ "' "
      ++ "LIMIT 1")
    pure $
      case scalar of
        Right value -> Just (trimTrailingNewline value)
        Left _ -> Nothing

fetchIndexExists :: PostgresTestContext -> String -> IO Bool
fetchIndexExists ctx indexName = do
    scalar <- runScalarQueryCtx ctx ("SELECT EXISTS (SELECT 1 FROM pg_indexes WHERE schemaname = current_schema() AND indexname = '" ++ indexName ++ "')")
    pure $
      case scalar of
        Right "t\n" -> True
        Right "t" -> True
        _ -> False

tryInsertDuplicateUsername :: PostgresTestContext -> IO Bool
tryInsertDuplicateUsername ctx = do
    dupResult <- runSqlCommandCtx ctx "INSERT INTO auth_users (username, password_hash, role, approved) VALUES ('Alice', 'h3', 'member', false)"
    pure $
      case dupResult of
        Left _ -> True
        Right () -> False

runSqlCommandCtx :: PostgresTestContext -> String -> IO (Either String ())
runSqlCommandCtx ctx sqlCommand = do
    result <- readProcessWithExitCode "psql" ["--dbname", ctxConnUrl ctx, "-v", "ON_ERROR_STOP=1", "-c", sqlCommand] ""
    pure $
      case result of
        (ExitSuccess, _, _) -> Right ()
        (_, _, err) -> Left err

runScalarQueryCtx :: PostgresTestContext -> String -> IO (Either String String)
runScalarQueryCtx ctx sqlCommand = do
    result <- readProcessWithExitCode "psql" ["--dbname", ctxConnUrl ctx, "-tA", "-c", sqlCommand] ""
    pure $
      case result of
        (ExitSuccess, out, _) -> Right out
        (_, _, err) -> Left err

trimTrailingNewline :: String -> String
trimTrailingNewline value =
  case reverse value of
    '\n':rest -> reverse rest
    _ -> value


sessionTests = test [ "Signed token should reject tampering" ~: signedTokenRejectsTampering
                    , "Revoked session should be rejected" ~: revokedSessionIsRejected
                    , "Idle timeout should expire session" ~: idleTimeoutExpiresSession
                    , "Sliding renewal should extend idle session" ~: slidingRenewalExtendsIdleSession
                    , "Revoking all sessions from one session should revoke sibling sessions" ~: revokeAllSessionsFromSession
                    , "Corrupted session handle should be rejected gracefully" ~: corruptedSessionHandleIsRejected
                    , "Resolve should treat repository read failures as unresolved session" ~: resolveReadFailureIsUnresolved
                    , "Revoke-all should stay successful when binding delete returns NotFound" ~: revokeAllIgnoresMissingBindingDelete
                    ]

sessionFilesystemAdapterTests = test
  [ "Session FS adapter should return AlreadyExists on duplicate handle create" ~: fsRepoDuplicateHandleCreateReturnsAlreadyExists
  , "Session FS adapter should return NotFound for missing handle load" ~: fsRepoMissingHandleLoadReturnsNotFound
  , "Session FS adapter should return NotFound for missing state update" ~: fsRepoMissingStateUpdateReturnsNotFound
  , "Session FS adapter should return ReadFailure for malformed state JSON" ~: fsRepoMalformedStateReturnsReadFailure
  , "Session FS adapter should return AlreadyExists on duplicate binding create" ~: fsRepoDuplicateBindingCreateReturnsAlreadyExists
  , "Session FS adapter delete-all binding should be deterministic and idempotent" ~: fsRepoDeleteAllBindingsIsIdempotent
  ]

sessionPostgresRepositoryTests = test
  [ "Session Postgres adapter should round-trip state/handle/binding lifecycle" ~: pgRepoRoundTripLifecycle
  , "Session Postgres adapter should return AlreadyExists on duplicate handle create" ~: pgRepoDuplicateHandleCreateReturnsAlreadyExists
  , "Session Postgres adapter should return NotFound for missing handle load" ~: pgRepoMissingHandleLoadReturnsNotFound
  , "Session Postgres adapter should return NotFound for missing state update" ~: pgRepoMissingStateUpdateReturnsNotFound
  , "Session Postgres adapter delete-all binding should stay deterministic and idempotent" ~: pgRepoDeleteAllBindingsIsIdempotent
  ]

calendarPostgresRepositoryTests = test
  [ "Calendar Postgres adapter should round-trip create/load/list/update/delete lifecycle" ~: pgCalendarRepoRoundTripLifecycle
  , "Calendar Postgres adapter should keep trip duration update unchanged" ~: pgCalendarRepoTripDurationUpdateIsNoop
  , "Calendar Postgres adapter should return NotFound for missing item operations" ~: pgCalendarRepoMissingOperationsReturnNotFound
  ]

tripSharingPostgresRepositoryTests = test
  [ "Trip-sharing Postgres adapter should keep add/list/delete deterministic and idempotent" ~: pgTripSharingRepoRoundTripDeterministic
  , "Trip-sharing Postgres adapter should keep subscriptions independent from shares" ~: pgTripSharingRepoSubscriptionsIndependent
  ]

financeAccountPostgresRepositoryTests = test
  [ "Finance account Postgres adapter should create and list active accounts with trimmed names" ~: pgFinanceAccountRepoCreateAndListActive
  , "Finance account Postgres adapter should reject duplicate normalized names" ~: pgFinanceAccountRepoDuplicateNormalizedNameReturnsAlreadyExists
  , "Finance account Postgres adapter should close accounts idempotently and return NotFound for missing ids" ~: pgFinanceAccountRepoCloseLifecycle
  , "Finance account Postgres adapter should filter by status and keep users isolated" ~: pgFinanceAccountRepoStatusFilteringAndIsolation
  ]

financeCategoryPostgresRepositoryTests = test
  [ "Finance category Postgres adapter should list seeded built-ins and user-owned categories together" ~: pgFinanceCategoryRepoListsBuiltInsAndUserCategories
  , "Finance category Postgres adapter should create, update, and delete user-owned categories" ~: pgFinanceCategoryRepoCreateUpdateDeleteLifecycle
  , "Finance category Postgres adapter should reject invalid parents and category cycles" ~: pgFinanceCategoryRepoRejectsInvalidParentAndCycles
  , "Finance category Postgres adapter should protect built-ins and non-empty parent categories from delete" ~: pgFinanceCategoryRepoProtectsBuiltInsAndParentDeletes
  ]

financeTransactionPostgresRepositoryTests = test
  [ "Finance transaction Postgres adapter should create sent and received rows and preserve idempotent retries" ~: pgFinanceTransactionRepoCreateAndIdempotency
  , "Finance transaction Postgres adapter should reject reused idempotency keys for different requests" ~: pgFinanceTransactionRepoRejectsIdempotencyConflicts
  , "Finance transaction Postgres adapter should list deterministically with account and half-open time filters" ~: pgFinanceTransactionRepoListWithFilters
  ]

notePostgresRepositoryTests = test
  [ "Note Postgres adapter should round-trip create/list/update/delete lifecycle" ~: pgNoteRepoRoundTripLifecycle
  , "Note Postgres adapter should return NotCurrentVersion for stale update" ~: pgNoteRepoWrongVersionReturnsNotCurrentVersion
  , "Note Postgres adapter should keep missing delete idempotent" ~: pgNoteRepoDeleteMissingIsIdempotent
  ]

checklistPostgresRepositoryTests = test
  [ "Checklist Postgres adapter should round-trip create/list/update/delete lifecycle" ~: pgChecklistRepoRoundTripLifecycle
  , "Checklist Postgres adapter should return NotCurrentVersion for stale update" ~: pgChecklistRepoWrongVersionReturnsNotCurrentVersion
  , "Checklist Postgres adapter should keep missing delete idempotent" ~: pgChecklistRepoDeleteMissingIsIdempotent
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

resolveReadFailureIsUnresolved :: IO ()
resolveReadFailureIsUnresolved = do
    let store = mkSessionStore failingRepo defaultSessionConfig
        failingRepo = SessionRepository
          { repoCreateSessionHandle = const (pure ())
          , repoLoadSessionHandleBySessionId = const (throwE ReadFailure)
          , repoUpdateSessionHandle = const (pure ())
          , repoDeleteSessionHandleBySessionId = const (pure ())
          , repoCreateSessionState = const (pure ())
          , repoLoadSessionStateByStateId = const (throwE ReadFailure)
          , repoUpdateSessionState = const (pure ())
          , repoDeleteSessionStateByStateId = const (pure ())
          , repoCreateUserStateBinding = \_ _ -> pure ()
          , repoLoadUserStateBindingByUserId = const (throwE ReadFailure)
          , repoDeleteUserStateBindingByUserId = const (pure ())
          , repoDeleteAllUserStateBindingsForUser = const (pure ())
          }
    resolved <- resolveSession store "sid-read-failure"
    case resolved of
      Nothing -> assertBool "Resolve should treat read failure as unresolved session" True
      Just _ -> assertFailure "Expected unresolved session when repository read fails"

revokeAllIgnoresMissingBindingDelete :: IO ()
revokeAllIgnoresMissingBindingDelete = do
    now <- getCurrentTime
    let handle = SessionHandle
          { handleSessionId = "sid-revoke-all"
          , handleStateId = "state-revoke-all"
          , handleIssuedAt = now
          , handleRevokedAt = Nothing
          }
        st = SessionState
          { stateId = "state-revoke-all"
          , stateUserId = "user-revoke-all"
          , stateCreatedAt = now
          , stateExpiresAt = addUTCTime 30 now
          , stateIdleExpiresAt = addUTCTime 30 now
          , stateRevokedAt = Nothing
          }
        store = mkSessionStore repo defaultSessionConfig
        repo = SessionRepository
          { repoCreateSessionHandle = const (pure ())
          , repoLoadSessionHandleBySessionId = const (pure handle)
          , repoUpdateSessionHandle = const (pure ())
          , repoDeleteSessionHandleBySessionId = const (pure ())
          , repoCreateSessionState = const (pure ())
          , repoLoadSessionStateByStateId = const (pure st)
          , repoUpdateSessionState = const (pure ())
          , repoDeleteSessionStateByStateId = const (pure ())
          , repoCreateUserStateBinding = \_ _ -> pure ()
          , repoLoadUserStateBindingByUserId = const (throwE NotFound)
          , repoDeleteUserStateBindingByUserId = const (pure ())
          , repoDeleteAllUserStateBindingsForUser = const (throwE NotFound)
          }
    revoked <- revokeAllForSession store "sid-revoke-all"
    assertBool "Revoke-all should remain successful when binding delete reports NotFound" revoked

fsRepoDuplicateHandleCreateReturnsAlreadyExists :: IO ()
fsRepoDuplicateHandleCreateReturnsAlreadyExists = withSessionRepoSandbox "dup-handle" $ \baseDir repo -> do
    now <- getCurrentTime
    let handle = SessionHandle
          { handleSessionId = "dup-handle-sid"
          , handleStateId = "dup-handle-state"
          , handleIssuedAt = now
          , handleRevokedAt = Nothing
          }
    first <- runExceptT $ repoCreateSessionHandle repo handle
    case first of
      Left err -> assertFailure ("Expected first handle create success, got " ++ show err)
      Right () -> pure ()
    second <- runExceptT $ repoCreateSessionHandle repo handle
    case second of
      Left AlreadyExists -> assertBool "Expected AlreadyExists for duplicate handle create" True
      Left err -> assertFailure ("Expected AlreadyExists, got " ++ show err)
      Right () -> assertFailure "Expected duplicate handle create to fail"

fsRepoMissingHandleLoadReturnsNotFound :: IO ()
fsRepoMissingHandleLoadReturnsNotFound = withSessionRepoSandbox "missing-handle" $ \_ repo -> do
    result <- runExceptT $ repoLoadSessionHandleBySessionId repo "missing-handle"
    case result of
      Left NotFound -> assertBool "Expected NotFound for missing handle load" True
      Left err -> assertFailure ("Expected NotFound, got " ++ show err)
      Right _ -> assertFailure "Expected missing handle load to fail"

fsRepoMissingStateUpdateReturnsNotFound :: IO ()
fsRepoMissingStateUpdateReturnsNotFound = withSessionRepoSandbox "missing-state-update" $ \_ repo -> do
    now <- getCurrentTime
    let st = SessionState
          { stateId = "missing-state"
          , stateUserId = "missing-user"
          , stateCreatedAt = now
          , stateExpiresAt = addUTCTime 30 now
          , stateIdleExpiresAt = addUTCTime 30 now
          , stateRevokedAt = Nothing
          }
    result <- runExceptT $ repoUpdateSessionState repo st
    case result of
      Left NotFound -> assertBool "Expected NotFound for missing state update" True
      Left err -> assertFailure ("Expected NotFound, got " ++ show err)
      Right () -> assertFailure "Expected missing state update to fail"

fsRepoMalformedStateReturnsReadFailure :: IO ()
fsRepoMalformedStateReturnsReadFailure = withSessionRepoSandbox "malformed-state" $ \baseDir repo -> do
    let path = baseDir ++ "/states/malformed-state.json"
    BL8.writeFile path (BL8.pack "{not-valid-json")
    result <- runExceptT $ repoLoadSessionStateByStateId repo "malformed-state"
    case result of
      Left ReadFailure -> assertBool "Expected ReadFailure for malformed state JSON" True
      Left err -> assertFailure ("Expected ReadFailure, got " ++ show err)
      Right _ -> assertFailure "Expected malformed state load to fail"

fsRepoDuplicateBindingCreateReturnsAlreadyExists :: IO ()
fsRepoDuplicateBindingCreateReturnsAlreadyExists = withSessionRepoSandbox "dup-binding" $ \_ repo -> do
    let binding = UserStateBinding { boundStateId = "dup-binding-state" }
    first <- runExceptT $ repoCreateUserStateBinding repo "dup-user" binding
    case first of
      Left err -> assertFailure ("Expected first binding create success, got " ++ show err)
      Right () -> pure ()
    second <- runExceptT $ repoCreateUserStateBinding repo "dup-user" binding
    case second of
      Left AlreadyExists -> assertBool "Expected AlreadyExists for duplicate binding create" True
      Left err -> assertFailure ("Expected AlreadyExists, got " ++ show err)
      Right () -> assertFailure "Expected duplicate binding create to fail"

fsRepoDeleteAllBindingsIsIdempotent :: IO ()
fsRepoDeleteAllBindingsIsIdempotent = withSessionRepoSandbox "delete-all-idempotent" $ \_ repo -> do
    first <- runExceptT $ repoDeleteAllUserStateBindingsForUser repo "idempotent-user"
    case first of
      Left err -> assertFailure ("Expected first delete-all to succeed, got " ++ show err)
      Right () -> pure ()
    createResult <- runExceptT $ repoCreateUserStateBinding repo "idempotent-user" UserStateBinding { boundStateId = "state-1" }
    case createResult of
      Left err -> assertFailure ("Expected binding create success, got " ++ show err)
      Right () -> pure ()
    second <- runExceptT $ repoDeleteAllUserStateBindingsForUser repo "idempotent-user"
    case second of
      Left err -> assertFailure ("Expected second delete-all to succeed, got " ++ show err)
      Right () -> pure ()
    third <- runExceptT $ repoDeleteAllUserStateBindingsForUser repo "idempotent-user"
    case third of
      Left err -> assertFailure ("Expected third delete-all to succeed, got " ++ show err)
      Right () -> pure ()

withSessionRepoSandbox :: String -> (FilePath -> SessionRepository -> IO ()) -> IO ()
withSessionRepoSandbox label action = do
    cd <- getCurrentDirectory
    nonce <- round . (* 1000000) <$> getPOSIXTime
    let baseDir = cd ++ "/dist-newstyle/sandbox/session-repo-tests/" ++ label ++ "-" ++ show (nonce :: Integer)
    createDirectoryIfMissing True (baseDir ++ "/handles")
    createDirectoryIfMissing True (baseDir ++ "/states")
    createDirectoryIfMissing True (baseDir ++ "/users")
    let repo = mkFilesystemSessionRepository baseDir
    action baseDir repo `finally` do
      exists <- doesDirectoryExist baseDir
      when exists $ removeDirectoryRecursive baseDir

pgRepoRoundTripLifecycle :: IO ()
pgRepoRoundTripLifecycle =
  withOptionalPostgresContext "Skipping Session Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runSessionMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected session migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          now <- getCurrentTime
          let repo = mkPostgresSessionRepository pool
              st = SessionState
                { stateId = "11111111-1111-1111-1111-111111111111"
                , stateUserId = "pg-user"
                , stateCreatedAt = now
                , stateExpiresAt = addUTCTime 30 now
                , stateIdleExpiresAt = addUTCTime 30 now
                , stateRevokedAt = Nothing
                }
              handle = SessionHandle
                { handleSessionId = "22222222-2222-2222-2222-222222222222"
                , handleStateId = stateId st
                , handleIssuedAt = now
                , handleRevokedAt = Nothing
                }
          createState <- runExceptT $ repoCreateSessionState repo st
          case createState of
            Left err -> assertFailure ("Expected state create success, got " ++ show err)
            Right () -> pure ()

          createHandle <- runExceptT $ repoCreateSessionHandle repo handle
          case createHandle of
            Left err -> assertFailure ("Expected handle create success, got " ++ show err)
            Right () -> pure ()

          createBinding <- runExceptT $ repoCreateUserStateBinding repo "pg-user" UserStateBinding { boundStateId = stateId st }
          case createBinding of
            Left err -> assertFailure ("Expected binding create success, got " ++ show err)
            Right () -> pure ()

          loadedState <- runExceptT $ repoLoadSessionStateByStateId repo (stateId st)
          case loadedState of
            Left err -> assertFailure ("Expected load state success, got " ++ show err)
            Right loaded ->
              assertEqual "Expected loaded state id to match" (stateId st) (stateId loaded)

          loadedHandle <- runExceptT $ repoLoadSessionHandleBySessionId repo (handleSessionId handle)
          case loadedHandle of
            Left err -> assertFailure ("Expected load handle success, got " ++ show err)
            Right loaded ->
              assertEqual "Expected loaded handle session id to match" (handleSessionId handle) (handleSessionId loaded)

          loadedBinding <- runExceptT $ repoLoadUserStateBindingByUserId repo "pg-user"
          case loadedBinding of
            Left err -> assertFailure ("Expected load binding success, got " ++ show err)
            Right loaded ->
              assertEqual "Expected loaded binding state id to match" (stateId st) (boundStateId loaded)

pgRepoDuplicateHandleCreateReturnsAlreadyExists :: IO ()
pgRepoDuplicateHandleCreateReturnsAlreadyExists =
  withOptionalPostgresContext "Skipping Session Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runSessionMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected session migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          now <- getCurrentTime
          let repo = mkPostgresSessionRepository pool
              st = SessionState
                { stateId = "33333333-3333-3333-3333-333333333333"
                , stateUserId = "dup-user"
                , stateCreatedAt = now
                , stateExpiresAt = addUTCTime 30 now
                , stateIdleExpiresAt = addUTCTime 30 now
                , stateRevokedAt = Nothing
                }
              handle = SessionHandle
                { handleSessionId = "44444444-4444-4444-4444-444444444444"
                , handleStateId = stateId st
                , handleIssuedAt = now
                , handleRevokedAt = Nothing
                }
          _ <- runExceptT $ repoCreateSessionState repo st
          first <- runExceptT $ repoCreateSessionHandle repo handle
          case first of
            Left err -> assertFailure ("Expected first handle create success, got " ++ show err)
            Right () -> pure ()
          second <- runExceptT $ repoCreateSessionHandle repo handle
          case second of
            Left AlreadyExists -> assertBool "Expected AlreadyExists on duplicate handle create" True
            Left err -> assertFailure ("Expected AlreadyExists, got " ++ show err)
            Right () -> assertFailure "Expected duplicate handle create to fail"

pgRepoMissingHandleLoadReturnsNotFound :: IO ()
pgRepoMissingHandleLoadReturnsNotFound =
  withOptionalPostgresContext "Skipping Session Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runSessionMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected session migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          let repo = mkPostgresSessionRepository pool
          result <- runExceptT $ repoLoadSessionHandleBySessionId repo "55555555-5555-5555-5555-555555555555"
          case result of
            Left NotFound -> assertBool "Expected NotFound for missing handle load" True
            Left err -> assertFailure ("Expected NotFound, got " ++ show err)
            Right _ -> assertFailure "Expected missing handle load to fail"

pgRepoMissingStateUpdateReturnsNotFound :: IO ()
pgRepoMissingStateUpdateReturnsNotFound =
  withOptionalPostgresContext "Skipping Session Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runSessionMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected session migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          now <- getCurrentTime
          let repo = mkPostgresSessionRepository pool
              st = SessionState
                { stateId = "66666666-6666-6666-6666-666666666666"
                , stateUserId = "missing-update-user"
                , stateCreatedAt = now
                , stateExpiresAt = addUTCTime 30 now
                , stateIdleExpiresAt = addUTCTime 30 now
                , stateRevokedAt = Nothing
                }
          result <- runExceptT $ repoUpdateSessionState repo st
          case result of
            Left NotFound -> assertBool "Expected NotFound for missing state update" True
            Left err -> assertFailure ("Expected NotFound, got " ++ show err)
            Right () -> assertFailure "Expected missing state update to fail"

pgRepoDeleteAllBindingsIsIdempotent :: IO ()
pgRepoDeleteAllBindingsIsIdempotent =
  withOptionalPostgresContext "Skipping Session Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runSessionMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected session migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          now <- getCurrentTime
          let repo = mkPostgresSessionRepository pool
              st = SessionState
                { stateId = "77777777-7777-7777-7777-777777777777"
                , stateUserId = "idempotent-user"
                , stateCreatedAt = now
                , stateExpiresAt = addUTCTime 30 now
                , stateIdleExpiresAt = addUTCTime 30 now
                , stateRevokedAt = Nothing
                }
          _ <- runExceptT $ repoCreateSessionState repo st
          first <- runExceptT $ repoDeleteAllUserStateBindingsForUser repo "idempotent-user"
          case first of
            Left err -> assertFailure ("Expected first delete-all to succeed, got " ++ show err)
            Right () -> pure ()
          createResult <- runExceptT $ repoCreateUserStateBinding repo "idempotent-user" UserStateBinding { boundStateId = stateId st }
          case createResult of
            Left err -> assertFailure ("Expected binding create success, got " ++ show err)
            Right () -> pure ()
          second <- runExceptT $ repoDeleteAllUserStateBindingsForUser repo "idempotent-user"
          case second of
            Left err -> assertFailure ("Expected second delete-all to succeed, got " ++ show err)
            Right () -> pure ()
          third <- runExceptT $ repoDeleteAllUserStateBindingsForUser repo "idempotent-user"
          case third of
            Left err -> assertFailure ("Expected third delete-all to succeed, got " ++ show err)
            Right () -> pure ()

pgCalendarRepoRoundTripLifecycle :: IO ()
pgCalendarRepoRoundTripLifecycle =
  withOptionalPostgresContext "Skipping Calendar Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runCalendarMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected calendar migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          let repo = postgresCalendarRepository pool
              userId = "pg-calendar-user"
          created <- runExceptT $ repoCreateCalendarItem repo userId sampleAgendaContent
          case created of
            Left err -> assertFailure ("Expected calendar create success, got " ++ show err)
            Right Agenda.ServerCalendarItem {Agenda.itemId = createdId} -> do
              loaded <- runExceptT $ repoLoadCalendarItemById repo userId createdId
              case loaded of
                Left err -> assertFailure ("Expected calendar load success, got " ++ show err)
                Right loadedItem ->
                  assertEqual "Expected loaded calendar item id to match created id" createdId (Agenda.itemId loadedItem)

              listed <- runExceptT $ repoListCalendarItemsForUser repo userId
              case listed of
                Left err -> assertFailure ("Expected calendar list success, got " ++ show err)
                Right [onlyItem] ->
                  assertEqual "Expected listed calendar item id to match created id" createdId (Agenda.itemId onlyItem)
                Right items -> assertFailure ("Expected one listed calendar item, got " ++ show (length items))

              let updatedContent = sampleAgendaContent { Agenda.title = "Postgres updated title" }
              updated <- runExceptT $ repoUpdateCalendarItem repo userId createdId updatedContent
              case updated of
                Left err -> assertFailure ("Expected calendar update success, got " ++ show err)
                Right updatedItem -> do
                  assertEqual "Expected updated item id to remain stable" createdId (Agenda.itemId updatedItem)
                  assertEqual "Expected updated title to be persisted" "Postgres updated title" (calendarTitle updatedItem)

              validated <- runExceptT $ repoUpdateCalendarItemDuration repo userId createdId 50
              case validated of
                Left err -> assertFailure ("Expected calendar duration update success, got " ++ show err)
                Right validatedItem ->
                  assertEqual "Expected duration update to persist actualDurationMinutes" (Just 50) (calendarActualDurationMinutes validatedItem)

              deleted <- runExceptT $ repoDeleteCalendarItemById repo userId createdId
              case deleted of
                Left err -> assertFailure ("Expected calendar delete success, got " ++ show err)
                Right () -> pure ()

              loadAfterDelete <- runExceptT $ repoLoadCalendarItemById repo userId createdId
              case loadAfterDelete of
                Left NotFound -> assertBool "Expected deleted calendar item load to return NotFound" True
                Left err -> assertFailure ("Expected NotFound after delete, got " ++ show err)
                Right _ -> assertFailure "Expected deleted calendar item load to fail"
            Right Agenda.NewCalendarItem {} -> assertFailure "Expected postgres calendar create to return a server item"

pgCalendarRepoTripDurationUpdateIsNoop :: IO ()
pgCalendarRepoTripDurationUpdateIsNoop =
  withOptionalPostgresContext "Skipping Calendar Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runCalendarMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected calendar migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          let repo = postgresCalendarRepository pool
              userId = "pg-calendar-trip-user"
          created <- runExceptT $ repoCreateCalendarItem repo userId sampleTripContent
          case created of
            Left err -> assertFailure ("Expected trip create success, got " ++ show err)
            Right Agenda.ServerCalendarItem {Agenda.itemId = createdId} -> do
              updated <- runExceptT $ repoUpdateCalendarItemDuration repo userId createdId 77
              case updated of
                Left err -> assertFailure ("Expected trip duration update call success, got " ++ show err)
                Right Agenda.ServerCalendarItem {Agenda.content = Agenda.TripCalendarItemContent tripContent} ->
                  assertEqual "Expected trip content unchanged by duration update path" (Agenda.tripWindowStart sampleTripInner) (Agenda.tripWindowStart tripContent)
                Right _ -> assertFailure "Expected trip item to remain a trip after duration update"
            Right Agenda.NewCalendarItem {} -> assertFailure "Expected postgres trip create to return a server item"
  where
    sampleTripInner =
      case sampleTripContent of
        Agenda.TripCalendarItemContent trip -> trip
        _ -> Agenda.TripItemContent "" "" "" ""

pgCalendarRepoMissingOperationsReturnNotFound :: IO ()
pgCalendarRepoMissingOperationsReturnNotFound =
  withOptionalPostgresContext "Skipping Calendar Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runCalendarMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected calendar migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          let repo = postgresCalendarRepository pool
              userId = "pg-calendar-missing-user"
              missingId = "missing-id"
          loadResult <- runExceptT $ repoLoadCalendarItemById repo userId missingId
          updateResult <- runExceptT $ repoUpdateCalendarItem repo userId missingId sampleAgendaContent
          durationResult <- runExceptT $ repoUpdateCalendarItemDuration repo userId missingId 15
          deleteResult <- runExceptT $ repoDeleteCalendarItemById repo userId missingId
          assertEqual "Expected missing load to return NotFound" (Left NotFound) loadResult
          assertEqual "Expected missing update to return NotFound" (Left NotFound) updateResult
          assertEqual "Expected missing duration update to return NotFound" (Left NotFound) durationResult
          assertEqual "Expected missing delete to return NotFound" (Left NotFound) deleteResult

pgTripSharingRepoRoundTripDeterministic :: IO ()
pgTripSharingRepoRoundTripDeterministic =
  withOptionalPostgresContext "Skipping Trip-sharing Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runTripSharingMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected trip-sharing migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          let repo = postgresTripSharingRepository pool
          addA <- runExceptT $ repoAddSharedUser repo "alice" "charlie"
          addB <- runExceptT $ repoAddSharedUser repo "alice" "bob"
          addDuplicate <- runExceptT $ repoAddSharedUser repo "alice" "bob"
          case (addA, addB, addDuplicate) of
            (Right (), Right (), Right ()) -> do
              listed <- runExceptT $ repoListSharedUsers repo "alice"
              case listed of
                Left err -> assertFailure ("Expected share list success, got " ++ show err)
                Right users -> assertEqual "Expected deterministic ordered share list" ["bob", "charlie"] users
            _ -> assertFailure "Expected trip-sharing share additions to succeed and stay idempotent"

          delExisting <- runExceptT $ repoDeleteSharedUser repo "alice" "bob"
          delMissing <- runExceptT $ repoDeleteSharedUser repo "alice" "missing-user"
          case (delExisting, delMissing) of
            (Right (), Right ()) -> pure ()
            _ -> assertFailure "Expected share delete operations to stay idempotent"

pgTripSharingRepoSubscriptionsIndependent :: IO ()
pgTripSharingRepoSubscriptionsIndependent =
  withOptionalPostgresContext "Skipping Trip-sharing Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runTripSharingMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected trip-sharing migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          let repo = postgresTripSharingRepository pool
          _ <- runExceptT $ repoAddSharedUser repo "alice" "bob"
          _ <- runExceptT $ repoAddSubscribedUser repo "alice" "dave"
          _ <- runExceptT $ repoAddSubscribedUser repo "alice" "carol"
          subscriptions <- runExceptT $ repoListSubscribedUsers repo "alice"
          shares <- runExceptT $ repoListSharedUsers repo "alice"
          case (subscriptions, shares) of
            (Right subscriptionUsers, Right shareUsers) -> do
              assertEqual "Expected deterministic ordered subscriptions list" ["carol", "dave"] subscriptionUsers
              assertEqual "Expected subscriptions to remain independent from shares" ["bob"] shareUsers
            _ -> assertFailure "Expected both shares and subscriptions to load"

          deleteSubscription <- runExceptT $ repoDeleteSubscribedUser repo "alice" "missing-user"
          case deleteSubscription of
            Right () -> assertBool "Expected missing subscription delete to remain idempotent" True
            Left err -> assertFailure ("Expected idempotent missing subscription delete, got " ++ show err)

pgFinanceAccountRepoCreateAndListActive :: IO ()
pgFinanceAccountRepoCreateAndListActive =
  withOptionalPostgresContext "Skipping Finance Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runFinanceMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected finance migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          let repo = postgresFinanceAccountRepository pool
              userId = "finance-user"
          created <- runExceptT $ repoCreateFinanceAccount repo userId "Cash Wallet"
          case created of
            Left err -> assertFailure ("Expected finance account create success, got " ++ show err)
            Right account -> do
              assertEqual "Expected created finance account name to match payload" "Cash Wallet" (financeAccountName account)
              assertEqual "Expected created finance account status to be active" FinanceAccountActive (financeAccountStatus account)
              listed <- runExceptT $ repoListFinanceAccounts repo userId FinanceAccountsActive
              case listed of
                Left err -> assertFailure ("Expected finance account list success, got " ++ show err)
                Right [onlyAccount] -> assertEqual "Expected active account list to return the created account" account onlyAccount
                Right accounts -> assertFailure ("Expected exactly one active finance account, got " ++ show (length accounts))

pgFinanceAccountRepoDuplicateNormalizedNameReturnsAlreadyExists :: IO ()
pgFinanceAccountRepoDuplicateNormalizedNameReturnsAlreadyExists =
  withOptionalPostgresContext "Skipping Finance Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runFinanceMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected finance migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          let repo = postgresFinanceAccountRepository pool
              userId = "finance-dup-user"
          first <- runExceptT $ repoCreateFinanceAccount repo userId "Main Account"
          case first of
            Left err -> assertFailure ("Expected first finance account create success, got " ++ show err)
            Right _ -> pure ()
          second <- runExceptT $ repoCreateFinanceAccount repo userId "main account"
          case second of
            Left AlreadyExists -> assertBool "Expected duplicate normalized finance account name to return AlreadyExists" True
            Left err -> assertFailure ("Expected AlreadyExists, got " ++ show err)
            Right _ -> assertFailure "Expected duplicate normalized finance account create to fail"

pgFinanceAccountRepoStatusFilteringAndIsolation :: IO ()
pgFinanceAccountRepoStatusFilteringAndIsolation =
  withOptionalPostgresContext "Skipping Finance Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runFinanceMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected finance migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          let repo = postgresFinanceAccountRepository pool
          createdA <- runExceptT $ repoCreateFinanceAccount repo "user-a" "Alpha"
          _ <- runExceptT $ repoCreateFinanceAccount repo "user-b" "Beta"
          case createdA of
            Left err -> assertFailure ("Expected finance account create success for user-a, got " ++ show err)
            Right createdAccountA -> do
              closeResult <- runExceptT $ repoCloseFinanceAccount repo "user-a" (financeAccountId createdAccountA)
              case closeResult of
                Left err -> assertFailure ("Expected finance account close success, got " ++ show err)
                Right closedAccount ->
                  assertEqual "Expected close response to mark the account closed" FinanceAccountClosed (financeAccountStatus closedAccount)

          activeA <- runExceptT $ repoListFinanceAccounts repo "user-a" FinanceAccountsActive
          closedA <- runExceptT $ repoListFinanceAccounts repo "user-a" FinanceAccountsClosed
          allA <- runExceptT $ repoListFinanceAccounts repo "user-a" FinanceAccountsAll
          allB <- runExceptT $ repoListFinanceAccounts repo "user-b" FinanceAccountsAll

          assertEqual "Expected user-a active accounts to be empty after closing the only account" (Right []) activeA
          case closedA of
            Right [closedAccount] -> assertEqual "Expected closed account status for user-a" FinanceAccountClosed (financeAccountStatus closedAccount)
            Right accounts -> assertFailure ("Expected exactly one closed account for user-a, got " ++ show (length accounts))
            Left err -> assertFailure ("Expected closed account list success for user-a, got " ++ show err)
          case allA of
            Right [allAccount] -> assertEqual "Expected all filter to include the closed account for user-a" FinanceAccountClosed (financeAccountStatus allAccount)
            Right accounts -> assertFailure ("Expected exactly one account in all filter for user-a, got " ++ show (length accounts))
            Left err -> assertFailure ("Expected all account list success for user-a, got " ++ show err)
          case allB of
            Right [onlyAccount] -> do
              assertEqual "Expected user-b account status to remain active" FinanceAccountActive (financeAccountStatus onlyAccount)
              assertEqual "Expected user-b account name to remain isolated" "Beta" (financeAccountName onlyAccount)
            Right accounts -> assertFailure ("Expected exactly one isolated account for user-b, got " ++ show (length accounts))
            Left err -> assertFailure ("Expected all account list success for user-b, got " ++ show err)

pgFinanceAccountRepoCloseLifecycle :: IO ()
pgFinanceAccountRepoCloseLifecycle =
  withOptionalPostgresContext "Skipping Finance Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runFinanceMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected finance migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          let repo = postgresFinanceAccountRepository pool
              userId = "finance-close-user"
          created <- runExceptT $ repoCreateFinanceAccount repo userId "Archive"
          case created of
            Left err -> assertFailure ("Expected finance account create success, got " ++ show err)
            Right createdAccount -> do
              firstClose <- runExceptT $ repoCloseFinanceAccount repo userId (financeAccountId createdAccount)
              secondClose <- runExceptT $ repoCloseFinanceAccount repo userId (financeAccountId createdAccount)
              missingClose <- runExceptT $ repoCloseFinanceAccount repo userId "missing-account"
              case (firstClose, secondClose, missingClose) of
                (Right firstClosed, Right secondClosed, Left NotFound) -> do
                  assertEqual "Expected first close to return closed status" FinanceAccountClosed (financeAccountStatus firstClosed)
                  assertEqual "Expected second close to stay idempotent and return the same closed projection" firstClosed secondClosed
                (firstResult, secondResult, missingResult) ->
                  assertFailure ("Unexpected finance account close results: " ++ show (firstResult, secondResult, missingResult))

pgFinanceCategoryRepoListsBuiltInsAndUserCategories :: IO ()
pgFinanceCategoryRepoListsBuiltInsAndUserCategories =
  withOptionalPostgresContext "Skipping Finance category Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runFinanceMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected finance migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          let repo = postgresFinanceCategoryRepository pool
              userId = "finance-category-user"
          rootCreate <- runExceptT $ repoCreateFinanceCategory repo userId FinanceCategoryWriteRequest
            { financeCategoryWriteName = "Custom Root"
            , financeCategoryWriteParentId = Nothing
            }
          childCreate <- runExceptT $ repoCreateFinanceCategory repo userId FinanceCategoryWriteRequest
            { financeCategoryWriteName = "Commute Extension"
            , financeCategoryWriteParentId = Just "transport"
            }
          case (rootCreate, childCreate) of
            (Right rootCategory, Right childCategory) -> do
              listed <- runExceptT $ repoListFinanceCategories repo userId
              case listed of
                Left err -> assertFailure ("Expected finance category list success, got " ++ show err)
                Right categories -> do
                  let incomeRoot = find (\category -> financeCategoryId category == "income") categories
                      salaryLeaf = find (\category -> financeCategoryId category == "income.salary") categories
                      listedRoot = find (\category -> financeCategoryId category == financeCategoryId rootCategory) categories
                      listedChild = find (\category -> financeCategoryId category == financeCategoryId childCategory) categories
                  case incomeRoot of
                    Just category -> do
                      assertEqual "Expected income root owner to be built-in" FinanceCategoryBuiltIn (financeCategoryOwner category)
                      assertEqual "Expected income root to be non-selectable" False (financeCategorySelectable category)
                    Nothing -> assertFailure "Expected seeded income root category to exist"
                  case salaryLeaf of
                    Just category -> do
                      assertEqual "Expected income salary owner to be built-in" FinanceCategoryBuiltIn (financeCategoryOwner category)
                      assertEqual "Expected income salary to be selectable" True (financeCategorySelectable category)
                    Nothing -> assertFailure "Expected seeded income salary category to exist"
                  case (listedRoot, listedChild) of
                    (Just rootCategoryRead, Just childCategoryRead) -> do
                      assertEqual "Expected custom root to stay user-owned" FinanceCategoryUser (financeCategoryOwner rootCategoryRead)
                      assertEqual "Expected custom child parent to reference built-in transport root" (Just "transport") (financeCategoryParentId childCategoryRead)
                    _ -> assertFailure "Expected custom categories to appear in the effective list"
            results -> assertFailure ("Unexpected finance category create results: " ++ show results)

pgFinanceCategoryRepoCreateUpdateDeleteLifecycle :: IO ()
pgFinanceCategoryRepoCreateUpdateDeleteLifecycle =
  withOptionalPostgresContext "Skipping Finance category Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runFinanceMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected finance migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          let repo = postgresFinanceCategoryRepository pool
              userId = "finance-category-update-user"
          created <- runExceptT $ repoCreateFinanceCategory repo userId FinanceCategoryWriteRequest
            { financeCategoryWriteName = "Fuel Notes"
            , financeCategoryWriteParentId = Just "transport"
            }
          case created of
            Left err -> assertFailure ("Expected finance category create success, got " ++ show err)
            Right category -> do
              updated <- runExceptT $ repoUpdateFinanceCategory repo userId (financeCategoryId category) FinanceCategoryWriteRequest
                { financeCategoryWriteName = "Fuel Notes Updated"
                , financeCategoryWriteParentId = Just "personal"
                }
              case updated of
                Left err -> assertFailure ("Expected finance category update success, got " ++ show err)
                Right updatedCategory -> do
                  assertEqual "Expected updated name" "Fuel Notes Updated" (financeCategoryName updatedCategory)
                  assertEqual "Expected updated parent id" (Just "personal") (financeCategoryParentId updatedCategory)
                  deleteResult <- runExceptT $ repoDeleteFinanceCategory repo userId (financeCategoryId updatedCategory)
                  missingDeleteResult <- runExceptT $ repoDeleteFinanceCategory repo userId (financeCategoryId updatedCategory)
                  case (deleteResult, missingDeleteResult) of
                    (Right (), Left NotFound) -> pure ()
                    results -> assertFailure ("Unexpected finance category delete results: " ++ show results)

pgFinanceCategoryRepoRejectsInvalidParentAndCycles :: IO ()
pgFinanceCategoryRepoRejectsInvalidParentAndCycles =
  withOptionalPostgresContext "Skipping Finance category Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runFinanceMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected finance migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          let repo = postgresFinanceCategoryRepository pool
          ownerCategory <- runExceptT $ repoCreateFinanceCategory repo "user-a" FinanceCategoryWriteRequest
            { financeCategoryWriteName = "Parent"
            , financeCategoryWriteParentId = Nothing
            }
          foreignCategory <- runExceptT $ repoCreateFinanceCategory repo "user-b" FinanceCategoryWriteRequest
            { financeCategoryWriteName = "Foreign Parent"
            , financeCategoryWriteParentId = Nothing
            }
          case (ownerCategory, foreignCategory) of
            (Right parentCategory, Right otherUserCategory) -> do
              childCategory <- runExceptT $ repoCreateFinanceCategory repo "user-a" FinanceCategoryWriteRequest
                { financeCategoryWriteName = "Child"
                , financeCategoryWriteParentId = Just (financeCategoryId parentCategory)
                }
              invalidParent <- runExceptT $ repoCreateFinanceCategory repo "user-a" FinanceCategoryWriteRequest
                { financeCategoryWriteName = "Invalid Parent"
                , financeCategoryWriteParentId = Just "missing-parent"
                }
              foreignParent <- runExceptT $ repoCreateFinanceCategory repo "user-a" FinanceCategoryWriteRequest
                { financeCategoryWriteName = "Foreign Parent Link"
                , financeCategoryWriteParentId = Just (financeCategoryId otherUserCategory)
                }
              case childCategory of
                Left err -> assertFailure ("Expected finance child category create success, got " ++ show err)
                Right createdChild -> do
                  cycleUpdate <- runExceptT $ repoUpdateFinanceCategory repo "user-a" (financeCategoryId parentCategory) FinanceCategoryWriteRequest
                    { financeCategoryWriteName = "Parent"
                    , financeCategoryWriteParentId = Just (financeCategoryId createdChild)
                    }
                  case (invalidParent, foreignParent, cycleUpdate) of
                    (Left WriteFailure, Left WriteFailure, Left WriteFailure) -> pure ()
                    results -> assertFailure ("Unexpected invalid parent or cycle results: " ++ show results)
            results -> assertFailure ("Unexpected finance category setup results: " ++ show results)

pgFinanceCategoryRepoProtectsBuiltInsAndParentDeletes :: IO ()
pgFinanceCategoryRepoProtectsBuiltInsAndParentDeletes =
  withOptionalPostgresContext "Skipping Finance category Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runFinanceMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected finance migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          let repo = postgresFinanceCategoryRepository pool
              userId = "finance-category-protected-user"
          parentCategory <- runExceptT $ repoCreateFinanceCategory repo userId FinanceCategoryWriteRequest
            { financeCategoryWriteName = "Deletable Parent"
            , financeCategoryWriteParentId = Nothing
            }
          case parentCategory of
            Left err -> assertFailure ("Expected finance parent category create success, got " ++ show err)
            Right createdParent -> do
              childCreate <- runExceptT $ repoCreateFinanceCategory repo userId FinanceCategoryWriteRequest
                { financeCategoryWriteName = "Nested Child"
                , financeCategoryWriteParentId = Just (financeCategoryId createdParent)
                }
              builtInUpdate <- runExceptT $ repoUpdateFinanceCategory repo userId "income" FinanceCategoryWriteRequest
                { financeCategoryWriteName = "Nope"
                , financeCategoryWriteParentId = Nothing
                }
              builtInDelete <- runExceptT $ repoDeleteFinanceCategory repo userId "income"
              parentDelete <- runExceptT $ repoDeleteFinanceCategory repo userId (financeCategoryId createdParent)
              case (childCreate, builtInUpdate, builtInDelete, parentDelete) of
                (Right _, Left AlreadyExists, Left AlreadyExists, Left AlreadyExists) -> pure ()
                results -> assertFailure ("Unexpected protected category results: " ++ show results)

pgFinanceTransactionRepoCreateAndIdempotency :: IO ()
pgFinanceTransactionRepoCreateAndIdempotency =
  withOptionalPostgresContext "Skipping Finance transaction Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runFinanceMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected finance migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          let accountRepo = postgresFinanceAccountRepository pool
              transactionRepo = postgresFinanceTransactionRepository pool
              userId = "finance-txn-user"
          createdAccount <- runExceptT $ repoCreateFinanceAccount accountRepo userId "Checking"
          case createdAccount of
            Left err -> assertFailure ("Expected finance account create success, got " ++ show err)
            Right account -> do
              sentCreated <- runExceptT $ repoCreateFinanceTransaction transactionRepo userId FinanceTransactionWriteRequest
                { financeTransactionWriteIdempotencyKey = "idem-sent"
                , financeTransactionWriteDirection = FinanceTransactionSent
                , financeTransactionWriteAccountId = financeAccountId account
                , financeTransactionWriteAmount = 2500
                , financeTransactionWriteOccurredAt = read "2026-01-02 10:00:00 UTC"
                , financeTransactionWriteOccurredAtSupplied = True
                }
              sentRetried <- runExceptT $ repoCreateFinanceTransaction transactionRepo userId FinanceTransactionWriteRequest
                { financeTransactionWriteIdempotencyKey = "idem-sent"
                , financeTransactionWriteDirection = FinanceTransactionSent
                , financeTransactionWriteAccountId = financeAccountId account
                , financeTransactionWriteAmount = 2500
                , financeTransactionWriteOccurredAt = read "2026-01-02 10:00:00 UTC"
                , financeTransactionWriteOccurredAtSupplied = True
                }
              receivedCreated <- runExceptT $ repoCreateFinanceTransaction transactionRepo userId FinanceTransactionWriteRequest
                { financeTransactionWriteIdempotencyKey = "idem-received"
                , financeTransactionWriteDirection = FinanceTransactionReceived
                , financeTransactionWriteAccountId = financeAccountId account
                , financeTransactionWriteAmount = 3200
                , financeTransactionWriteOccurredAt = read "2026-01-03 10:00:00 UTC"
                , financeTransactionWriteOccurredAtSupplied = True
                }
              case (sentCreated, sentRetried, receivedCreated) of
                (Right firstSent, Right retriedSent, Right receivedTxn) -> do
                  assertEqual "Expected idempotent retry to return the original sent transaction row" firstSent retriedSent
                  assertEqual "Expected sent direction" FinanceTransactionSent (financeTransactionDirection firstSent)
                  assertEqual "Expected received direction" FinanceTransactionReceived (financeTransactionDirection receivedTxn)
                results -> assertFailure ("Unexpected finance transaction create results: " ++ show results)

pgFinanceTransactionRepoRejectsIdempotencyConflicts :: IO ()
pgFinanceTransactionRepoRejectsIdempotencyConflicts =
  withOptionalPostgresContext "Skipping Finance transaction Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runFinanceMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected finance migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          let accountRepo = postgresFinanceAccountRepository pool
              transactionRepo = postgresFinanceTransactionRepository pool
              userId = "finance-txn-conflict-user"
          createdAccount <- runExceptT $ repoCreateFinanceAccount accountRepo userId "Salary"
          case createdAccount of
            Left err -> assertFailure ("Expected finance account create success, got " ++ show err)
            Right account -> do
              firstCreate <- runExceptT $ repoCreateFinanceTransaction transactionRepo userId FinanceTransactionWriteRequest
                { financeTransactionWriteIdempotencyKey = "idem-conflict"
                , financeTransactionWriteDirection = FinanceTransactionReceived
                , financeTransactionWriteAccountId = financeAccountId account
                , financeTransactionWriteAmount = 4200
                , financeTransactionWriteOccurredAt = read "2026-02-01 09:00:00 UTC"
                , financeTransactionWriteOccurredAtSupplied = True
                }
              conflictingRetry <- runExceptT $ repoCreateFinanceTransaction transactionRepo userId FinanceTransactionWriteRequest
                { financeTransactionWriteIdempotencyKey = "idem-conflict"
                , financeTransactionWriteDirection = FinanceTransactionReceived
                , financeTransactionWriteAccountId = financeAccountId account
                , financeTransactionWriteAmount = 4300
                , financeTransactionWriteOccurredAt = read "2026-02-01 09:00:00 UTC"
                , financeTransactionWriteOccurredAtSupplied = True
                }
              case (firstCreate, conflictingRetry) of
                (Right _, Left AlreadyExists) -> assertBool "Expected conflicting idempotent retry to return AlreadyExists" True
                results -> assertFailure ("Unexpected finance transaction idempotency results: " ++ show results)

pgFinanceTransactionRepoListWithFilters :: IO ()
pgFinanceTransactionRepoListWithFilters =
  withOptionalPostgresContext "Skipping Finance transaction Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runFinanceMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected finance migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          let accountRepo = postgresFinanceAccountRepository pool
              transactionRepo = postgresFinanceTransactionRepository pool
              userId = "finance-txn-list-user"
          primaryAccount <- runExceptT $ repoCreateFinanceAccount accountRepo userId "Primary"
          secondaryAccount <- runExceptT $ repoCreateFinanceAccount accountRepo userId "Secondary"
          case (primaryAccount, secondaryAccount) of
            (Right primary, Right secondary) -> do
              _ <- runExceptT $ repoCreateFinanceTransaction transactionRepo userId FinanceTransactionWriteRequest
                { financeTransactionWriteIdempotencyKey = "list-key-1"
                , financeTransactionWriteDirection = FinanceTransactionSent
                , financeTransactionWriteAccountId = financeAccountId primary
                , financeTransactionWriteAmount = 101
                , financeTransactionWriteOccurredAt = read "2026-04-01 10:00:00 UTC"
                , financeTransactionWriteOccurredAtSupplied = True
                }
              _ <- runExceptT $ repoCreateFinanceTransaction transactionRepo userId FinanceTransactionWriteRequest
                { financeTransactionWriteIdempotencyKey = "list-key-2"
                , financeTransactionWriteDirection = FinanceTransactionReceived
                , financeTransactionWriteAccountId = financeAccountId secondary
                , financeTransactionWriteAmount = 202
                , financeTransactionWriteOccurredAt = read "2026-04-03 10:00:00 UTC"
                , financeTransactionWriteOccurredAtSupplied = True
                }
              _ <- runExceptT $ repoCreateFinanceTransaction transactionRepo userId FinanceTransactionWriteRequest
                { financeTransactionWriteIdempotencyKey = "list-key-3"
                , financeTransactionWriteDirection = FinanceTransactionSent
                , financeTransactionWriteAccountId = financeAccountId primary
                , financeTransactionWriteAmount = 303
                , financeTransactionWriteOccurredAt = read "2026-04-02 10:00:00 UTC"
                , financeTransactionWriteOccurredAtSupplied = True
                }
              firstSameTime <- runExceptT $ repoCreateFinanceTransaction transactionRepo userId FinanceTransactionWriteRequest
                { financeTransactionWriteIdempotencyKey = "list-key-4"
                , financeTransactionWriteDirection = FinanceTransactionSent
                , financeTransactionWriteAccountId = financeAccountId primary
                , financeTransactionWriteAmount = 404
                , financeTransactionWriteOccurredAt = read "2026-04-02 10:00:00 UTC"
                , financeTransactionWriteOccurredAtSupplied = True
                }
              secondSameTime <- runExceptT $ repoCreateFinanceTransaction transactionRepo userId FinanceTransactionWriteRequest
                { financeTransactionWriteIdempotencyKey = "list-key-5"
                , financeTransactionWriteDirection = FinanceTransactionReceived
                , financeTransactionWriteAccountId = financeAccountId primary
                , financeTransactionWriteAmount = 505
                , financeTransactionWriteOccurredAt = read "2026-04-02 10:00:00 UTC"
                , financeTransactionWriteOccurredAtSupplied = True
                }
              allListed <- runExceptT $ repoListFinanceTransactions transactionRepo userId Nothing Nothing Nothing
              primaryListed <- runExceptT $ repoListFinanceTransactions transactionRepo userId (Just (financeAccountId primary)) Nothing Nothing
              fromListed <- runExceptT $ repoListFinanceTransactions transactionRepo userId Nothing (Just (read "2026-04-02 10:00:00 UTC")) Nothing
              toListed <- runExceptT $ repoListFinanceTransactions transactionRepo userId Nothing Nothing (Just (read "2026-04-02 10:00:00 UTC"))
              unknownAccountListed <- runExceptT $ repoListFinanceTransactions transactionRepo userId (Just "missing-account") Nothing Nothing
              case (firstSameTime, secondSameTime, allListed, primaryListed, fromListed, toListed, unknownAccountListed) of
                (Right sameA, Right sameB, Right allTransactions, Right primaryTransactions, Right fromTransactions, Right toTransactions, Right unknownTransactions) -> do
                  assertEqual "Expected all transactions to be ordered by occurredAt descending" [202, 303, 404, 505, 101] (map financeTransactionAmount allTransactions)
                  let sameTimeIds = map financeTransactionId (take 2 (drop 2 allTransactions))
                  assertEqual "Expected same-timestamp rows to be ordered by id ascending" (sort sameTimeIds) sameTimeIds
                  assertEqual "Expected same-timestamp rows to match created ids" (sort [financeTransactionId sameA, financeTransactionId sameB]) sameTimeIds
                  assertEqual "Expected account filter to keep only primary-account transactions" [303, 404, 505, 101] (map financeTransactionAmount primaryTransactions)
                  assertEqual "Expected from filter to include the boundary timestamp" [202, 303, 404, 505] (map financeTransactionAmount fromTransactions)
                  assertEqual "Expected to filter to exclude the boundary timestamp" [101] (map financeTransactionAmount toTransactions)
                  assertEqual "Expected unknown account filter to return an empty list" [] unknownTransactions
                results -> assertFailure ("Unexpected finance transaction list results: " ++ show results)
            results -> assertFailure ("Expected finance account setup success for list test, got " ++ show results)

pgNoteRepoRoundTripLifecycle :: IO ()
pgNoteRepoRoundTripLifecycle =
  withOptionalPostgresContext "Skipping Note Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runNoteMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected note migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          let repo = postgresNoteRepository pool
              createdContent = NoteContent { title = Just "pg note title", noteContent = "pg note body" }
              updatedContent = NoteContent { title = Just "pg note title updated", noteContent = "pg note body updated" }

          createResult <- runExceptT $ repoCreateItem repo createdContent
          case createResult of
            Left err -> assertFailure ("Expected note create success, got " ++ show err)
            Right createdStorageId -> do
              listedAfterCreate <- runExceptT $ repoListItems repo
              case listedAfterCreate of
                Left err -> assertFailure ("Expected note list success, got " ++ show err)
                Right [createdItem] -> do
                  assertEqual "Expected listed note id to match created id" createdStorageId (storageId createdItem)
                  assertEqual "Expected listed note content to match create payload" createdContent (content createdItem)
                Right listed -> assertFailure ("Expected one note item after create, got " ++ show (length listed))

              updateResult <- runExceptT $ repoUpdateItem repo (Identifiable createdStorageId updatedContent)
              case updateResult of
                Left err -> assertFailure ("Expected note update success, got " ++ show err)
                Right updatedStorageId -> do
                  assertEqual "Expected note id to stay stable after update" createdStorageId updatedStorageId
                  assertBool "Expected note version to change after update" (version createdStorageId /= version updatedStorageId)

                  listedAfterUpdate <- runExceptT $ repoListItems repo
                  case listedAfterUpdate of
                    Left err -> assertFailure ("Expected note list after update success, got " ++ show err)
                    Right [updatedItem] -> do
                      assertEqual "Expected listed updated note id to match created id" createdStorageId (storageId updatedItem)
                      assertEqual "Expected listed updated note content to match update payload" updatedContent (content updatedItem)
                    Right listed -> assertFailure ("Expected one note item after update, got " ++ show (length listed))

                  let StorageId {id = createdItemId} = createdStorageId
                  deleteResult <- runExceptT $ repoDeleteItemById repo createdItemId
                  case deleteResult of
                    Left err -> assertFailure ("Expected note delete success, got " ++ show err)
                    Right () -> do
                      listedAfterDelete <- runExceptT $ repoListItems repo
                      case listedAfterDelete of
                        Left err -> assertFailure ("Expected note list after delete success, got " ++ show err)
                        Right [] -> assertBool "Expected note list to be empty after delete" True
                        Right listed -> assertFailure ("Expected empty note list after delete, got " ++ show (length listed))

pgNoteRepoWrongVersionReturnsNotCurrentVersion :: IO ()
pgNoteRepoWrongVersionReturnsNotCurrentVersion =
  withOptionalPostgresContext "Skipping Note Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runNoteMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected note migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          let repo = postgresNoteRepository pool
              createdContent = NoteContent { title = Just "pg note stale", noteContent = "pg stale body" }
              staleContent = NoteContent { title = Just "pg note stale updated", noteContent = "pg stale body updated" }
          createResult <- runExceptT $ repoCreateItem repo createdContent
          case createResult of
            Left err -> assertFailure ("Expected note create success, got " ++ show err)
            Right createdStorageId -> do
              let staleStorageId = createdStorageId {version = version createdStorageId ++ "-stale"}
              staleUpdateResult <- runExceptT $ repoUpdateItem repo (Identifiable staleStorageId staleContent)
              case staleUpdateResult of
                Left (NotCurrentVersion actualStorageId) ->
                  assertEqual "Expected stale note update to report stale storage id" staleStorageId actualStorageId
                Left err -> assertFailure ("Expected NotCurrentVersion for stale note update, got " ++ show err)
                Right _ -> assertFailure "Expected stale note update to fail"

pgNoteRepoDeleteMissingIsIdempotent :: IO ()
pgNoteRepoDeleteMissingIsIdempotent =
  withOptionalPostgresContext "Skipping Note Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runNoteMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected note migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          let repo = postgresNoteRepository pool
          deleteResult <- runExceptT $ repoDeleteItemById repo "missing-note-id"
          case deleteResult of
            Left err -> assertFailure ("Expected missing note delete to succeed, got " ++ show err)
            Right () -> assertBool "Expected missing note delete to remain idempotent" True

pgChecklistRepoRoundTripLifecycle :: IO ()
pgChecklistRepoRoundTripLifecycle =
  withOptionalPostgresContext "Skipping Checklist Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runChecklistMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected checklist migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          let repo = postgresChecklistRepository pool
              createdContent = ChecklistContent { name = "pg checklist", items = [ChecklistItem {label = "first", checked = False}] }
              updatedContent = ChecklistContent { name = "pg checklist updated", items = [ChecklistItem {label = "first", checked = True}, ChecklistItem {label = "second", checked = False}] }

          createResult <- runExceptT $ repoCreateItem repo createdContent
          case createResult of
            Left err -> assertFailure ("Expected checklist create success, got " ++ show err)
            Right createdStorageId -> do
              listedAfterCreate <- runExceptT $ repoListItems repo
              case listedAfterCreate of
                Left err -> assertFailure ("Expected checklist list success, got " ++ show err)
                Right [createdItem] -> do
                  assertEqual "Expected listed checklist id to match created id" createdStorageId (storageId createdItem)
                  assertEqual "Expected listed checklist content to match create payload" createdContent (content createdItem)
                Right listed -> assertFailure ("Expected one checklist item after create, got " ++ show (length listed))

              updateResult <- runExceptT $ repoUpdateItem repo (Identifiable createdStorageId updatedContent)
              case updateResult of
                Left err -> assertFailure ("Expected checklist update success, got " ++ show err)
                Right updatedStorageId -> do
                  assertEqual "Expected checklist id to stay stable after update" createdStorageId updatedStorageId
                  assertBool "Expected checklist version to change after update" (version createdStorageId /= version updatedStorageId)

                  listedAfterUpdate <- runExceptT $ repoListItems repo
                  case listedAfterUpdate of
                    Left err -> assertFailure ("Expected checklist list after update success, got " ++ show err)
                    Right [updatedItem] -> do
                      assertEqual "Expected listed updated checklist id to match created id" createdStorageId (storageId updatedItem)
                      assertEqual "Expected listed updated checklist content to match update payload" updatedContent (content updatedItem)
                    Right listed -> assertFailure ("Expected one checklist item after update, got " ++ show (length listed))

                  let StorageId {id = createdItemId} = createdStorageId
                  deleteResult <- runExceptT $ repoDeleteItemById repo createdItemId
                  case deleteResult of
                    Left err -> assertFailure ("Expected checklist delete success, got " ++ show err)
                    Right () -> do
                      listedAfterDelete <- runExceptT $ repoListItems repo
                      case listedAfterDelete of
                        Left err -> assertFailure ("Expected checklist list after delete success, got " ++ show err)
                        Right [] -> assertBool "Expected checklist list to be empty after delete" True
                        Right listed -> assertFailure ("Expected empty checklist list after delete, got " ++ show (length listed))

pgChecklistRepoWrongVersionReturnsNotCurrentVersion :: IO ()
pgChecklistRepoWrongVersionReturnsNotCurrentVersion =
  withOptionalPostgresContext "Skipping Checklist Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runChecklistMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected checklist migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          let repo = postgresChecklistRepository pool
              createdContent = ChecklistContent { name = "pg checklist stale", items = [ChecklistItem {label = "first", checked = False}] }
              staleContent = ChecklistContent { name = "pg checklist stale updated", items = [ChecklistItem {label = "first", checked = True}] }
          createResult <- runExceptT $ repoCreateItem repo createdContent
          case createResult of
            Left err -> assertFailure ("Expected checklist create success, got " ++ show err)
            Right createdStorageId -> do
              let staleStorageId = createdStorageId {version = version createdStorageId ++ "-stale"}
              staleUpdateResult <- runExceptT $ repoUpdateItem repo (Identifiable staleStorageId staleContent)
              case staleUpdateResult of
                Left (NotCurrentVersion actualStorageId) ->
                  assertEqual "Expected stale checklist update to report stale storage id" staleStorageId actualStorageId
                Left err -> assertFailure ("Expected NotCurrentVersion for stale checklist update, got " ++ show err)
                Right _ -> assertFailure "Expected stale checklist update to fail"

pgChecklistRepoDeleteMissingIsIdempotent :: IO ()
pgChecklistRepoDeleteMissingIsIdempotent =
  withOptionalPostgresContext "Skipping Checklist Postgres repository test: set FOUCL_TEST_POSTGRES_URL and install psql" $ \ctx ->
    withIsolatedPostgresSchemaConn ctx $ \schemaConn -> do
      upResult <- runExceptT (runChecklistMigrationsAtPath "." schemaConn MigrateUp)
      case upResult of
        Left err -> assertFailure ("Expected checklist migration up success, got " ++ err)
        Right () -> do
          pool <- mkTestPostgresPool schemaConn
          let repo = postgresChecklistRepository pool
          deleteResult <- runExceptT $ repoDeleteItemById repo "missing-checklist-id"
          case deleteResult of
            Left err -> assertFailure ("Expected missing checklist delete to succeed, got " ++ show err)
            Right () -> assertBool "Expected missing checklist delete to remain idempotent" True

withTestPostgresPoolFromDbConfig :: DatabaseConfig -> (Pool Connection -> IO a) -> IO a
withTestPostgresPoolFromDbConfig dbCfg action =
  withTestPostgresPool (dbConfigToConnectionString dbCfg) action

withTestPostgresPool :: String -> (Pool Connection -> IO a) -> IO a
withTestPostgresPool connectionString action = do
  pool <- mkTestPostgresPool connectionString
  action pool `finally` destroyAllResources pool

mkTestPostgresPool :: String -> IO (Pool Connection)
mkTestPostgresPool connectionString =
  newPool (defaultPoolConfig (connectPostgreSQL (BS8.pack connectionString)) close 60 4)

dbConfigToConnectionString :: DatabaseConfig -> String
dbConfigToConnectionString dbCfg =
  unwords
    [ "host=" ++ databaseHost dbCfg
    , "port=" ++ show (databasePort dbCfg)
    , "dbname=" ++ databaseName dbCfg
    , "user=" ++ databaseUser dbCfg
    , "password=" ++ databasePassword dbCfg
    ]

withIsolatedPostgresSchemaConn :: PostgresTestContext -> (String -> IO ()) -> IO ()
withIsolatedPostgresSchemaConn ctx action = do
    createResult <- runSqlCommandCtx ctx ("CREATE SCHEMA " ++ ctxSchemaName ctx)
    case createResult of
      Left err -> assertFailure ("Failed creating schema: " ++ err)
      Right () -> do
        let schemaConnUrl = ctxConnUrl ctx ++ " options='-c search_path=" ++ ctxSchemaName ctx ++ ",public'"
        action schemaConnUrl `finally` do
          _ <- runSqlCommandCtx ctx ("DROP SCHEMA IF EXISTS " ++ ctxSchemaName ctx ++ " CASCADE")
          pure ()

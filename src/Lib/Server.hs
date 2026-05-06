{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib.Server
  ( apiController
  , homePage
  , serveStaticResource
  , log
  ) where

import Prelude hiding (log)
import Control.Concurrent.MVar (MVar, modifyMVar)
import Control.Monad (foldM, join, mplus, msum)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), decode, decode', encode, object, withObject, (.:), (.=))
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.ByteString.Char8 (unpack)
import Data.Char (toLower)
import Data.Int (Int64)
import Data.List (isPrefixOf, sortOn)
import Data.Maybe (Maybe(..), catMaybes, mapMaybe)
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Time.LocalTime (LocalTime)
import Happstack.Server (CookieLife(Expired, Session), FilterMonad, Method(DELETE, GET, POST, PUT), Response, RqBody, ServerPartT, addCookie, askRq, defaultBodyPolicy, dir, getHeaderM, guessContentTypeM, inputValue, internalServerError, look, method, mimeTypes, mkCookie, notFound, nullDir, ok, path, requestEntityTooLarge, rqInputsQuery, serveFileFrom, takeRequestBody, toResponse, unauthorized, unBody, uriRest, setResponseCode)
import qualified Happstack.Server as HServer
import Happstack.Server.Internal.Cookie (Cookie(..), SameSite(..))
import Happstack.Server.Internal.MessageWrap (BodyPolicy, bodyInput)
import qualified AgendaModel as Agenda
import Auth (AuthError(..), AuthRepository, AuthRequest(..), AuthRequestError(..), AuthenticatedProfile(..), approveUser, createUserWithBootstrapAdmin, deleteApprovedUser, deletePendingUser, isApprovedAdmin, listApprovedUsers, listPendingUsers, loadAuthenticatedProfile, signinUser, userExists)
import CalendarRepository (CalendarRepository(..))
import Crud
import CrudStorage (createItem, deleteItem, modifyItem)
import FinanceAccountRepository (FinanceAccount(..), FinanceAccountCreateRequest(..), FinanceAccountRepository(..), FinanceAccountStatus(..), FinanceAccountStatusFilter(..), normalizeFinanceAccountName, parseFinanceAccountStatusFilter)
import FinanceAccountRepository (FinanceAccountSnapshotCreateRequest(..))
import FinanceCategoryRepository
  ( FinanceCategoryRepository(..)
  , FinanceCategoryWriteRequest(..)
  , normalizeFinanceCategoryName
  )
import FinanceTransactionRepository
  ( FinanceTransactionCreateRequest(..)
  , FinanceTransactionCategorizeRequest(..)
  , FinanceReportDirection(..)
  , FinanceReportRequest(..)
  , FinanceTransactionDirection(..)
  , FinanceTransactionLinkRequest(..)
  , FinanceTransactionNoteCreateRequest(..)
  , FinanceTransactionNoteUpdateRequest(..)
  , FinanceTransactionSplitRequest(..)
  , FinanceTransaction
  , FinanceTransactionRepository(..)
  , FinanceTransactionWriteRequest(..)
  )
import Lib.Config (AppConfig(..))
import Model (Content, Identifiable(..))
import NotesChecklistRepository (ChecklistRepository, NoteRepository, NotesChecklistRepository(..))
import Repository (RepositoryError(..))
import Session (SessionConfig(..), SessionPrincipal(..), SessionStore(..), signSessionId, verifyAndExtractSessionId)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import TripSharingRepository (TripSharingRepository(..))

newtype AppContext = AppContext
  { sessionPrincipal :: SessionPrincipal }

newtype TripPlace = TripPlace
  { tripPlaceName :: String
  }

newtype TripSharingUser = TripSharingUser
  { tripSharingUsername :: String
  }

newtype PendingSignupApproval = PendingSignupApproval
  { pendingSignupApprovalUsername :: String
  }

data PeriodTripsUser = PeriodTripsUser
  { periodTripsUsername :: !String
  , periodTripsItems :: ![Agenda.CalendarItem]
  }

data StoredTripItem = StoredTripItem
  { storedTripStart :: !LocalTime
  , storedTripCalendarItem :: !Agenda.CalendarItem
  }

instance ToJSON TripPlace where
  toJSON (TripPlace placeName) = object ["name" .= placeName]

instance ToJSON TripSharingUser where
  toJSON (TripSharingUser username) = object ["username" .= username]

instance ToJSON PendingSignupApproval where
  toJSON (PendingSignupApproval username) = object ["username" .= username]

instance ToJSON PeriodTripsUser where
  toJSON (PeriodTripsUser username trips) =
    object
      [ "username" .= username
      , "trips" .= trips
      ]

instance FromJSON TripSharingUser where
  parseJSON = withObject "TripSharingUser" $ \value -> TripSharingUser
    <$> value .: "username"

instance FromJSON PendingSignupApproval where
  parseJSON = withObject "PendingSignupApproval" $ \value -> PendingSignupApproval
    <$> value .: "username"

tripPlacesCatalog :: [TripPlace]
tripPlacesCatalog =
  [ TripPlace "Paris"
  , TripPlace "Le Mesnil"
  , TripPlace "St Clair"
  ]

data TripWriteValidation
  = TripWriteValid
  | TripWriteBadRequest !String
  | TripWriteNotFound
  | TripWriteTechnicalFailure

tripPlaceNames :: [String]
tripPlaceNames = map tripPlaceName tripPlacesCatalog

validateTripWrite :: CalendarRepository -> String -> Maybe String -> Agenda.CalendarItemContent -> IO TripWriteValidation
validateTripWrite calendarRepo principalUserId mCurrentItemId content =
  case content of
    Agenda.TripCalendarItemContent tripContent -> do
      existingItemsResult <- runExceptT (repoListCalendarItemsForUser calendarRepo principalUserId)
      pure $
        case existingItemsResult of
          Left _ -> TripWriteTechnicalFailure
          Right existingItems -> validateTripContent tripPlaceNames existingItems mCurrentItemId tripContent
    _ -> pure TripWriteValid

validateTripContent :: [String] -> [Agenda.CalendarItem] -> Maybe String -> Agenda.TripItemContent -> TripWriteValidation
validateTripContent validPlaceNames existingItems mCurrentItemId tripContent =
  case validateTripWritePreconditions existingItems mCurrentItemId of
    Just result -> result
    Nothing ->
      case tripInterval tripContent of
        Nothing -> TripWriteBadRequest "windowStart and windowEnd must be valid ISO date-time strings"
        Just interval
          | Agenda.departurePlaceId tripContent `notElem` validPlaceNames ->
              TripWriteBadRequest "departurePlaceId must reference an existing trip place"
          | Agenda.arrivalPlaceId tripContent `notElem` validPlaceNames ->
              TripWriteBadRequest "arrivalPlaceId must reference an existing trip place"
          | Agenda.departurePlaceId tripContent == Agenda.arrivalPlaceId tripContent ->
              TripWriteBadRequest "departurePlaceId and arrivalPlaceId must be different"
          | not (tripIntervalHasPositiveDuration interval) ->
              TripWriteBadRequest "windowEnd must be strictly after windowStart"
          | otherwise ->
              case storedTripIntervals existingItems mCurrentItemId of
                Left () -> TripWriteTechnicalFailure
                Right intervals
                  | any (tripIntervalsOverlap interval) intervals ->
                      TripWriteBadRequest "trip time window overlaps another trip"
                  | otherwise -> TripWriteValid

validateTripWritePreconditions :: [Agenda.CalendarItem] -> Maybe String -> Maybe TripWriteValidation
validateTripWritePreconditions existingItems mCurrentItemId =
  case mCurrentItemId of
    Just currentItemId
      | currentItemId `notElem` mapMaybe calendarItemId existingItems -> Just TripWriteNotFound
    _ -> Nothing

calendarItemId :: Agenda.CalendarItem -> Maybe String
calendarItemId item =
  case item of
    Agenda.ServerCalendarItem { Agenda.itemId } -> Just itemId
    Agenda.NewCalendarItem {} -> Nothing

tripInterval :: Agenda.TripItemContent -> Maybe (LocalTime, LocalTime)
tripInterval tripContent = do
  start <- parseTripLocalTime (Agenda.tripWindowStart tripContent)
  end <- parseTripLocalTime (Agenda.tripWindowEnd tripContent)
  pure (start, end)

parseTripLocalTime :: String -> Maybe LocalTime
parseTripLocalTime raw =
  iso8601ParseM raw `mplus` parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M" raw

tripIntervalHasPositiveDuration :: (LocalTime, LocalTime) -> Bool
tripIntervalHasPositiveDuration (start, end) = end > start

-- Touching boundaries are allowed; only real interval intersection is rejected.
tripIntervalsOverlap :: (LocalTime, LocalTime) -> (LocalTime, LocalTime) -> Bool
tripIntervalsOverlap (startA, endA) (startB, endB) = startA < endB && startB < endA

storedTripIntervals :: [Agenda.CalendarItem] -> Maybe String -> Either () [(LocalTime, LocalTime)]
storedTripIntervals existingItems mCurrentItemId =
  mapM storedTripInterval (filter isOtherStoredTrip existingItems)
  where
    isOtherStoredTrip item =
      case item of
        Agenda.ServerCalendarItem { Agenda.content = Agenda.TripCalendarItemContent {}, Agenda.itemId } ->
          Just itemId /= mCurrentItemId
        _ -> False

    storedTripInterval item =
      case item of
        Agenda.ServerCalendarItem { Agenda.content = Agenda.TripCalendarItemContent tripContent } ->
          maybe (Left ()) Right (tripInterval tripContent)
        _ -> Left ()

parsePeriodTripBounds :: Maybe String -> Maybe String -> Either String (LocalTime, LocalTime)
parsePeriodTripBounds Nothing _ = Left "start is required"
parsePeriodTripBounds _ Nothing = Left "end is required"
parsePeriodTripBounds (Just rawStart) (Just rawEnd) =
  case (parseTripLocalTime rawStart, parseTripLocalTime rawEnd) of
    (Nothing, _) -> Left "start must be a valid ISO date-time string"
    (_, Nothing) -> Left "end must be a valid ISO date-time string"
    (Just start, Just end)
      | end <= start -> Left "end must be strictly after start"
      | otherwise -> Right (start, end)

resolveVisiblePeriodTripUsers :: TripSharingRepository -> String -> IO (Either () [String])
resolveVisiblePeriodTripUsers tripSharingRepo principalUserId = do
  subscribedUsersResult <- runExceptT (repoListSubscribedUsers tripSharingRepo principalUserId)
  case subscribedUsersResult of
    Left _ -> pure (Left ())
    Right subscribedUsers -> do
      visibilityResults <- mapM isVisibleToPrincipal subscribedUsers
      pure $ case sequence visibilityResults of
        Left _ -> Left ()
        Right visibleUsers -> Right [username | (username, True) <- visibleUsers]
  where
    isVisibleToPrincipal username = do
      sharedUsersResult <- runExceptT (repoListSharedUsers tripSharingRepo username)
      pure $ case sharedUsersResult of
        Left _ -> Left ()
        Right sharedUsers -> Right (username, principalUserId `elem` sharedUsers)

loadPeriodTripsForUsers :: CalendarRepository -> [String] -> LocalTime -> LocalTime -> IO (Either () [PeriodTripsUser])
loadPeriodTripsForUsers calendarRepo usernames periodStart periodEnd = do
  groups <- mapM buildUserGroup usernames
  pure $ fmap catMaybes (sequence groups)
  where
    buildUserGroup username = do
      itemsResult <- runExceptT (repoListCalendarItemsForUser calendarRepo username)
      pure $
        case itemsResult of
          Left _ -> Left ()
          Right items ->
            case selectPeriodTrips periodStart periodEnd items of
              Left () -> Left ()
              Right [] -> Right Nothing
              Right trips -> Right (Just (PeriodTripsUser username trips))

selectPeriodTrips :: LocalTime -> LocalTime -> [Agenda.CalendarItem] -> Either () [Agenda.CalendarItem]
selectPeriodTrips periodStart periodEnd items = do
  storedTrips <- storedTripItems items
  let orderedTrips = sortOn storedTripStart storedTrips
      seedTrip = case filter (\trip -> storedTripStart trip < periodStart) orderedTrips of
        [] -> Nothing
        earlierTrips -> Just (last earlierTrips)
      periodTrips =
        [ storedTripCalendarItem trip
        | trip <- orderedTrips
        , storedTripStart trip >= periodStart
        , storedTripStart trip < periodEnd
        ]
  pure $
    case seedTrip of
      Nothing -> periodTrips
      Just trip -> storedTripCalendarItem trip : periodTrips

storedTripItems :: [Agenda.CalendarItem] -> Either () [StoredTripItem]
storedTripItems items = catMaybes <$> mapM toStoredTripItem items
  where
    toStoredTripItem item =
      case item of
        Agenda.ServerCalendarItem { Agenda.content = Agenda.TripCalendarItemContent tripContent } ->
          case parseTripLocalTime (Agenda.tripWindowStart tripContent) of
            Nothing -> Left ()
            Just tripStart -> Right (Just (StoredTripItem tripStart item))
        _ -> Right Nothing

badRequest :: FilterMonad Response m => String -> m Response
badRequest = HServer.badRequest . jsonMessage

jsonResponse :: ToJSON a => a -> Response
jsonResponse = toResponse . encode

jsonMessage :: String -> Response
jsonMessage msg = jsonResponse $ object ["message" .= msg]

emptyResponse :: Response
emptyResponse = jsonResponse $ object []

authInternalError :: Response
authInternalError = jsonMessage "Unable to process authentication"

class ToServerResponse e where
  toServerResponse :: Monad m => e -> ServerPartT m Response

instance ToServerResponse AuthError where
  toServerResponse (BadRequest br) = badRequest errorStr
    where errorStr = case br of
                     EmptyUsername -> "Username cannot be empty"
                     UsernameDoesNotRespectPattern -> "Username has forbidden characters"
                     EmptyPassword -> "Password cannot be empty"
                     PasswordTooShort -> "Password is too short"
                     UsernameTooShort -> "Username is too short"
                     UsernameTooLong -> "Username is too long"
  toServerResponse UserAlreadyExists = badRequest "Unable to create user"
  toServerResponse InvalidCredentials = unauthorized $ jsonMessage "Invalid credentials"
  toServerResponse AccountPendingApproval = HServer.forbidden $ jsonMessage "Account pending approval"
  toServerResponse ResourceNotFound = notFound $ jsonMessage "Not found"
  toServerResponse (ResourceConflict message) = setResponseCode 409 >> pure (jsonMessage message)
  toServerResponse (TechnicalError _) = internalServerError $ jsonMessage "Unable to process authentication"

apiController :: AuthRepository -> CalendarRepository -> TripSharingRepository -> FinanceAccountRepository -> FinanceCategoryRepository -> FinanceTransactionRepository -> NoteRepository -> ChecklistRepository -> MVar [UTCTime] -> FilePath -> AppConfig -> SessionStore -> ServerPartT IO Response
apiController authRepo calendarRepo tripSharingRepo financeAccountRepo financeCategoryRepo financeTransactionRepo noteRepo checklistRepo signupRateLimitState tmpDir appConfig sessionStore =
  let sessionCfg = sessionConfig appConfig
      bootstrapAdmin = bootstrapAdminUsername appConfig
  in dir "api" $ msum [ signupController authRepo signupRateLimitState tmpDir bootstrapAdmin
                      , signinController authRepo sessionCfg sessionStore
                      , signoutController sessionCfg sessionStore
                      , dir "auth" $ requireAuth sessionCfg sessionStore (authController authRepo)
                      , dir "note" $ requireAuth sessionCfg sessionStore (noteController noteRepo)
                      , dir "checklist" $ requireAuth sessionCfg sessionStore (checklistController checklistRepo)
                      , dir "v1" $
                          msum
                            [ dir "trip-places" $ requireAuth sessionCfg sessionStore tripPlacesController
                            , dir "trip-sharing" $ requireAuth sessionCfg sessionStore (tripSharingController authRepo tripSharingRepo calendarRepo)
                            , dir "calendar-items" $ requireAuth sessionCfg sessionStore (agendaController calendarRepo)
                            , dir "finance" $ requireAuth sessionCfg sessionStore (financeController financeAccountRepo financeCategoryRepo financeTransactionRepo)
                            , dir "admin" $ requireAuth sessionCfg sessionStore (adminController authRepo bootstrapAdmin)
                            ]
                      ]

homePage :: ServerPartT IO Response
homePage = do
    nullDir
    cd <- liftIO getCurrentDirectory
    serveFileFrom (cd </> "static/") (guessContentTypeM mimeTypes) "index.html"

maxSignupBodyBytes :: Int64
maxSignupBodyBytes = 4096

signupBodyPolicy :: FilePath -> BodyPolicy
signupBodyPolicy tmpDir = defaultBodyPolicy tmpDir 0 maxSignupBodyBytes maxSignupBodyBytes

isTooLargeBodyError :: String -> Bool
isTooLargeBodyError err = "x-www-form-urlencoded content longer than BodyPolicy.maxRAM=" `isPrefixOf` err

signupController :: AuthRepository -> MVar [UTCTime] -> FilePath -> String -> ServerPartT IO Response
signupController authRepo signupRateLimitState tmpDir bootstrapAdmin = dir "signup" $ do
    nullDir
    method POST
    rq <- askRq
    (_, mBodyErr) <- liftIO $ bodyInput (signupBodyPolicy tmpDir) rq
    case mBodyErr of
      Just bodyErr | isTooLargeBodyError bodyErr -> requestEntityTooLarge $ jsonMessage "Body too large"
      Just _ -> badRequest "Unable to decode request body"
      Nothing -> do
        body <- askRq >>= takeRequestBody
        maybe (badRequest "Empty body")
              handleBody
              body
    where handleBody :: RqBody -> ServerPartT IO Response --AppM Response
          handleBody body =
            maybe (badRequest "Unable to decode the body as a SignupData")
                  doCreateUser
                  (decode $ unBody body)

          doCreateUser :: AuthRequest -> ServerPartT IO Response --AppM Response
          doCreateUser signupRequest = do
            allowed <- liftIO $ allowSignupRequest signupRateLimitState
            if not allowed
              then tooManyRequests "Too many signup attempts. Please retry later."
              else do
                res <- liftIO $ runExceptT $ createUserWithBootstrapAdmin authRepo (Just bootstrapAdmin) signupRequest
                either toServerResponse
                       (const $ ok emptyResponse)
                       res

signinController :: AuthRepository -> SessionConfig -> SessionStore -> ServerPartT IO Response
signinController authRepo sessionConfig sessionStore = dir "signin" $ do
  nullDir
  method POST
  withBusinessHandlingAndInput (signinUser authRepo) $ \profile -> do
    sid <- liftIO $ createSessionForUser sessionStore (authProfileUsername profile)
    let cookieValue = signSessionId (sessionSecret sessionConfig) sid
    addCookie Session (buildSessionCookie sessionConfig cookieValue)
    ok (jsonResponse profile)

signoutController :: SessionConfig -> SessionStore -> ServerPartT IO Response
signoutController sessionConfig sessionStore = dir "signout" $ do
  nullDir
  method POST
  revokeAll <- isSignoutAllRequested
  mToken <- getSessionCookieValue sessionConfig
  case mToken >>= verifyAndExtractSessionId (sessionSecret sessionConfig) of
    Nothing -> unauthorized $ jsonMessage "Not authenticated"
    Just sid -> do
      _ <- liftIO $ if revokeAll then revokeAllForSession sessionStore sid else revokeSession sessionStore sid
      addCookie Expired (buildSessionCookie sessionConfig "")
      ok emptyResponse

authController :: AuthRepository -> AppContext -> ServerPartT IO Response
authController authRepo AppContext { sessionPrincipal = SessionPrincipal { principalUserId } } =
  dir "profile" $ do
    nullDir
    method GET
    profileResult <- liftIO $ runExceptT $ loadAuthenticatedProfile authRepo principalUserId
    either toServerResponse
           (ok . jsonResponse)
           profileResult

isSignoutAllRequested :: ServerPartT IO Bool
isSignoutAllRequested = do
  mRaw <- (Just <$> look "all") `mplus` pure Nothing
  pure $ case fmap (map toLower) mRaw of
    Just "true" -> True
    Just "1" -> True
    Just "false" -> False
    Just "0" -> False
    _ -> False

tooManyRequests :: FilterMonad Response m => String -> m Response
tooManyRequests = HServer.badRequest . toResponse

allowSignupRequest :: MVar [UTCTime] -> IO Bool
allowSignupRequest state = do
  now <- getCurrentTime
  let windowStart = addUTCTime (-5) now
      maxRequests = 7
  modifyMVar state $ \timestamps -> do
    let recent = filter (> windowStart) timestamps
    pure $ if length recent >= maxRequests
              then (recent, False)
              else (now : recent, True)

withBusinessHandling :: (FromJSON a, ToServerResponse e) => (a -> ExceptT e IO r) -> ServerPartT IO Response
withBusinessHandling handle = do
    body <- askRq >>= takeRequestBody
    maybe (badRequest "Empty body")
          handleBody
          body
    where handleBody :: RqBody -> ServerPartT IO Response --AppM Response
          handleBody body = maybe (badRequest "Unable to decode the body as a SignupData")
                                  (processDecodedBody handle)
                                  (decode' $ unBody body)

          processDecodedBody :: ToServerResponse e => (a -> ExceptT e IO r) -> a -> ServerPartT IO Response
          processDecodedBody handle input = do
            res <- liftIO $ runExceptT $ handle input
            either toServerResponse
                   (const $ ok emptyResponse)
                   res


withBusinessHandlingAndInput :: (FromJSON a, ToServerResponse e) => (a -> ExceptT e IO r) -> (r -> ServerPartT IO Response) -> ServerPartT IO Response
withBusinessHandlingAndInput handle onSuccess = do
    body <- askRq >>= takeRequestBody
    maybe (badRequest "Empty body")
          handleBody
          body
    where
      handleBody :: RqBody -> ServerPartT IO Response
      handleBody body = maybe (badRequest "Unable to decode the body as a SignupData")
                             process
                             (decode' $ unBody body)
      process input = do
        res <- liftIO $ runExceptT $ handle input
        either toServerResponse
               onSuccess
               res

buildSessionCookie :: SessionConfig -> String -> Cookie
buildSessionCookie sessionConfig cookieValue =
  (mkCookie (sessionCookieName sessionConfig) cookieValue)
    { secure = sessionCookieSecure sessionConfig
    , httpOnly = True
    , sameSite = SameSiteLax
    }

getSessionCookieValue :: SessionConfig -> ServerPartT IO (Maybe String)
getSessionCookieValue sessionConfig = do
  mCookieHeader <- getHeaderM "cookie"
  pure $ mCookieHeader >>= extractCookie (sessionCookieName sessionConfig) . unpack

extractCookie :: String -> String -> Maybe String
extractCookie cookieName rawCookieHeader =
  let chunks = splitOn ';' rawCookieHeader
      normalized = map trim chunks
      targetPrefix = cookieName ++ "="
      matches = filter (isPrefixOf targetPrefix) normalized
  in case matches of
       [] -> Nothing
       (x:_) -> Just $ unquoteCookieValue $ drop (length targetPrefix) x

unquoteCookieValue :: String -> String
unquoteCookieValue value =
  case value of
    ('"':rest) | not (null rest) && last rest == '"' -> init rest
    _ -> value

splitOn :: Char -> String -> [String]
splitOn sep s =
  case break (== sep) s of
    (before, []) -> [before]
    (before, _:after) -> before : splitOn sep after

trim :: String -> String
trim = dropWhile (== ' ')

requireAuth :: SessionConfig -> SessionStore -> (AppContext -> ServerPartT IO Response) -> ServerPartT IO Response
requireAuth sessionConfig sessionStore handler = do
  mToken <- getSessionCookieValue sessionConfig
  let mSid = mToken >>= verifyAndExtractSessionId (sessionSecret sessionConfig)
  case mSid of
    Nothing -> unauthorized $ jsonMessage "Not authenticated"
    Just sid -> do
      mPrincipal <- liftIO $ resolveSession sessionStore sid
      case mPrincipal of
        Nothing -> unauthorized $ jsonMessage "Not authenticated"
        Just principal -> handler AppContext { sessionPrincipal = principal }

requireApprovedAdmin :: AuthRepository -> AppContext -> ServerPartT IO Response -> ServerPartT IO Response
requireApprovedAdmin authRepo AppContext { sessionPrincipal = SessionPrincipal { principalUserId } } handler = do
  adminCheck <- liftIO $ isApprovedAdmin authRepo principalUserId
  case adminCheck of
    Left _ -> internalServerError authInternalError
    Right False -> HServer.forbidden $ jsonMessage "Admin privileges required"
    Right True -> handler


noteController :: NoteRepository -> AppContext -> ServerPartT IO Response
noteController = notesChecklistHandlers "note"

checklistController :: ChecklistRepository -> AppContext -> ServerPartT IO Response
checklistController = notesChecklistHandlers "checklist"

notesChecklistHandlers :: Content a => String -> NotesChecklistRepository a -> AppContext -> ServerPartT IO Response
notesChecklistHandlers crudTypeName repo _ =
  msum
    [ notesChecklistGet crudTypeName repo
    , notesChecklistPost crudTypeName repo
    , notesChecklistDelete crudTypeName repo
    , notesChecklistPut crudTypeName repo
    ]

notesChecklistGet :: Content a => String -> NotesChecklistRepository a -> ServerPartT IO Response
notesChecklistGet crudTypeName repo = do
  nullDir
  method GET
  repoResult <- lift $ runExceptT (repoListItems repo)
  case repoResult of
    Left err -> genericInternalError ("Unexpected problem during retrieving all " ++ crudTypeName ++ "s:\n\t" ++ show err)
    Right items -> ok (jsonResponse items)

notesChecklistPost :: Content a => String -> NotesChecklistRepository a -> ServerPartT IO Response
notesChecklistPost crudTypeName repo = do
  nullDir
  method POST
  log ("crud POST on " ++ crudTypeName)
  body <- askRq >>= takeRequestBody
  let
    handleBody :: RqBody -> ServerPartT IO Response
    handleBody rqBody = do
      let bodyBS = unBody rqBody
          content = decode bodyBS
      log ("Getting body bytestrings: " ++ show bodyBS)
      log ("Getting deserialized content: " ++ show content)
      fmap (createNotesChecklistContent crudTypeName repo) content `orElse` genericInternalError "Unexpected problem during note creation"
  fmap handleBody body `orElse` ok emptyResponse

createNotesChecklistContent :: String -> NotesChecklistRepository a -> a -> ServerPartT IO Response
createNotesChecklistContent crudTypeName repo content = do
  recover (logThenGenericInternalErrorName crudTypeName) (ok . jsonResponse) $ repoCreateItem repo content

notesChecklistDelete :: String -> NotesChecklistRepository a -> ServerPartT IO Response
notesChecklistDelete crudTypeName repo = do
  method DELETE
  log ("crud DELETE on " ++ crudTypeName)
  path $ \pathId -> do
    nullDir
    recover (handleDeletionError pathId) (\() -> ok emptyResponse) $ repoDeleteItemById repo pathId

notesChecklistPut :: Content a => String -> NotesChecklistRepository a -> ServerPartT IO Response
notesChecklistPut crudTypeName repo = do
  nullDir
  method PUT
  log ("crud PUT on " ++ crudTypeName)
  body <- askRq >>= takeRequestBody
  let
    handleBody :: RqBody -> ServerPartT IO Response
    handleBody rqBody = do
      let bodyBS = unBody rqBody
          update = decode bodyBS
      log ("Getting body bytestrings: " ++ show bodyBS)
      log ("Getting deserialized content: " ++ show update)
      fmap (handleUpdateByRepository repo) update `orElse` genericInternalError "Unable to parse body as a NoteUpdate"
  fmap handleBody body `orElse` ok emptyResponse

handleUpdateByRepository :: Content a => NotesChecklistRepository a -> Identifiable a -> ServerPartT IO Response
handleUpdateByRepository repo update =
  recoverWith (const . notFound $ jsonMessage "Unable to find storage dir")
              (ok . jsonResponse <$> repoUpdateItem repo update)

tripPlacesController :: AppContext -> ServerPartT IO Response
tripPlacesController _ = do
  nullDir
  method GET
  ok (jsonResponse tripPlacesCatalog)

financeController :: FinanceAccountRepository -> FinanceCategoryRepository -> FinanceTransactionRepository -> AppContext -> ServerPartT IO Response
financeController financeAccountRepo financeCategoryRepo financeTransactionRepo AppContext { sessionPrincipal = SessionPrincipal { principalUserId } } =
  msum
    [ dir "accounts" $
        msum
          [ financeAccountsList
          , financeAccountsCreate
          , financeAccountsClose
          , financeAccountsSnapshots
          , financeAccountsReconciliation
          ]
    , dir "categories" $
        msum
          [ financeCategoriesList
          , financeCategoriesCreate
          , financeCategoriesUpdate
          , financeCategoriesDelete
          ]
    , dir "transactions" $
        msum
          [ financeTransactionsList
          , dir "sent" (financeTransactionsCreate FinanceTransactionSent)
          , dir "received" (financeTransactionsCreate FinanceTransactionReceived)
          , dir "link" financeTransactionsLink
          , financeTransactionsCategorize
          , financeTransactionsSplit
          , financeTransactionsNotes
          ]
    , dir "report" financeReport
    ]
  where
    financeAccountsList = do
      nullDir
      method GET
      mStatus <- (Just <$> look "status") `mplus` pure Nothing
      case parseFinanceAccountStatusFilter mStatus of
        Nothing -> badRequest "status must be one of: active, closed, all"
        Just statusFilter -> do
          result <- liftIO $ runExceptT (repoListFinanceAccounts financeAccountRepo principalUserId statusFilter)
          case result of
            Left _ -> internalServerError emptyResponse
            Right accounts -> ok (jsonResponse accounts)

    financeAccountsCreate = do
      nullDir
      method POST
      body <- askRq >>= takeRequestBody
      maybe (badRequest "Empty body") handleBody body

    handleBody rqBody =
      case decode' (unBody rqBody) :: Maybe FinanceAccountCreateRequest of
        Nothing -> badRequest "Unable to decode the body as a FinanceAccountCreateRequest"
        Just FinanceAccountCreateRequest { financeAccountCreateName } ->
          case normalizeFinanceAccountName financeAccountCreateName of
            Nothing -> badRequest "name must not be empty"
            Just normalizedName -> do
              result <- liftIO $ runExceptT (repoCreateFinanceAccount financeAccountRepo principalUserId normalizedName)
              case result of
                Left AlreadyExists -> setResponseCode 409 >> pure (jsonMessage "Account already exists")
                Left _ -> internalServerError emptyResponse
                Right account -> ok (jsonResponse account)

    financeAccountsClose = path $ \accountId -> do
      dir "close" $ do
        nullDir
        method POST
        result <- liftIO $ runExceptT (repoCloseFinanceAccount financeAccountRepo principalUserId accountId)
        case result of
          Left NotFound -> notFound (jsonMessage "Account not found")
          Left _ -> internalServerError emptyResponse
          Right account -> ok (jsonResponse account)

    financeAccountsSnapshots = path $ \accountId -> do
      dir "snapshots" $ do
        msum [financeAccountsSnapshotsCreate accountId, financeAccountsSnapshotsList accountId]

    financeAccountsSnapshotsCreate :: String -> ServerPartT IO Response
    financeAccountsSnapshotsCreate accountId = do
      nullDir
      method POST
      body <- askRq >>= takeRequestBody
      maybe (badRequest "Empty body") (handleSnapshotCreateBody accountId) body

    financeAccountsSnapshotsList :: String -> ServerPartT IO Response
    financeAccountsSnapshotsList accountId = do
      nullDir
      method GET
      result <- liftIO $ runExceptT (repoListFinanceAccountSnapshots financeAccountRepo principalUserId accountId)
      case result of
        Left NotFound -> notFound (jsonMessage "Account not found")
        Left _ -> internalServerError emptyResponse
        Right snapshots -> ok (jsonResponse snapshots)

    financeAccountsReconciliation = path $ \accountId -> do
      dir "reconciliation" $ do
        nullDir
        method GET
        mSnapshotId <- (Just <$> look "snapshotId") `mplus` pure Nothing
        result <- liftIO $ runExceptT $
          case mSnapshotId of
            Nothing -> repoGetFinanceAccountReconciliationLatest financeAccountRepo principalUserId accountId
            Just snapshotId -> repoGetFinanceAccountReconciliationBySnapshotId financeAccountRepo principalUserId accountId snapshotId
        case result of
          Left NotFound -> notFound (jsonMessage "Account or snapshot not found")
          Left _ -> internalServerError emptyResponse
          Right reconciliation -> ok (jsonResponse reconciliation)

    handleSnapshotCreateBody :: String -> RqBody -> ServerPartT IO Response
    handleSnapshotCreateBody accountId rqBody =
      case decode' (unBody rqBody) :: Maybe FinanceAccountSnapshotCreateRequest of
        Nothing -> badRequest "Unable to decode the body as a FinanceAccountSnapshotCreateRequest"
        Just FinanceAccountSnapshotCreateRequest
          { financeAccountSnapshotCreateBalance
          , financeAccountSnapshotCreateOccurredAt
          } ->
            case iso8601ParseM financeAccountSnapshotCreateOccurredAt of
              Nothing -> badRequest "occurredAt must be a valid ISO date-time string"
              Just occurredAt -> do
                result <- liftIO $ runExceptT (repoCreateFinanceAccountSnapshot financeAccountRepo principalUserId accountId financeAccountSnapshotCreateBalance occurredAt)
                case result of
                  Left NotFound -> notFound (jsonMessage "Account not found")
                  Left AlreadyExists -> setResponseCode 409 >> pure (jsonMessage "Snapshot already exists for that account and timestamp")
                  Left _ -> internalServerError emptyResponse
                  Right reconciliation -> ok (jsonResponse reconciliation)

    financeTransactionsCreate direction = do
      nullDir
      method POST
      mIdempotencyKey <- fmap unpack <$> getHeaderM "Idempotency-Key"
      case mIdempotencyKey of
        Nothing -> badRequest "Idempotency-Key header is required"
        Just idempotencyKey -> do
          body <- askRq >>= takeRequestBody
          maybe (badRequest "Empty body") (handleTransactionBody direction idempotencyKey) body

    handleTransactionBody :: FinanceTransactionDirection -> String -> RqBody -> ServerPartT IO Response
    handleTransactionBody direction idempotencyKey rqBody =
      case decode' (unBody rqBody) :: Maybe FinanceTransactionCreateRequest of
        Nothing -> badRequest "Unable to decode the body as a FinanceTransactionCreateRequest"
        Just FinanceTransactionCreateRequest
          { financeTransactionCreateAccountId
          , financeTransactionCreateAmount
          , financeTransactionCreateOccurredAt
          }
            | financeTransactionCreateAmount <= 0 ->
                badRequest "amount must be a positive integer"
            | otherwise -> do
                now <- liftIO getCurrentTime
                let parsedOccurredAt = case financeTransactionCreateOccurredAt of
                      Nothing -> Right (False, now)
                      Just rawOccurredAt ->
                        case iso8601ParseM rawOccurredAt of
                          Nothing -> Left "occurredAt must be a valid ISO date-time string"
                          Just occurredAt -> Right (True, occurredAt)
                case parsedOccurredAt of
                  Left message -> badRequest message
                  Right (occurredAtSupplied, occurredAt) -> do
                    accountResult <- liftIO $ runExceptT (repoGetFinanceAccountById financeAccountRepo principalUserId financeTransactionCreateAccountId)
                    case accountResult of
                      Left NotFound -> notFound (jsonMessage "Account not found")
                      Left _ -> internalServerError emptyResponse
                      Right FinanceAccount { financeAccountStatus = FinanceAccountClosed } ->
                        setResponseCode 409 >> pure (jsonMessage "Closed accounts cannot accept new transactions")
                      Right FinanceAccount {} -> do
                        let writeRequest = FinanceTransactionWriteRequest
                              { financeTransactionWriteIdempotencyKey = idempotencyKey
                              , financeTransactionWriteDirection = direction
                              , financeTransactionWriteAccountId = financeTransactionCreateAccountId
                              , financeTransactionWriteAmount = financeTransactionCreateAmount
                              , financeTransactionWriteOccurredAt = occurredAt
                              , financeTransactionWriteOccurredAtSupplied = occurredAtSupplied
                              }
                        createResult <- liftIO $ runExceptT (repoCreateFinanceTransaction financeTransactionRepo principalUserId writeRequest)
                        case createResult of
                          Left NotFound -> notFound (jsonMessage "Account not found")
                          Left AlreadyExists -> setResponseCode 409 >> pure (jsonMessage "Idempotency key already used for a different request")
                          Left _ -> internalServerError emptyResponse
                          Right transaction -> ok (jsonResponse transaction)

    financeTransactionsList = do
      nullDir
      method GET
      mAccountId <- (Just <$> look "accountId") `mplus` pure Nothing
      mFromRaw <- (Just <$> look "from") `mplus` pure Nothing
      mToRaw <- (Just <$> look "to") `mplus` pure Nothing
      case (parseQueryTime "from" mFromRaw, parseQueryTime "to" mToRaw) of
        (Left message, _) -> badRequest message
        (_, Left message) -> badRequest message
        (Right mFrom, Right mTo) ->
          case (mFrom, mTo) of
            (Just fromTs, Just toTs)
              | fromTs > toTs -> badRequest "from must be less than or equal to to"
              | fromTs == toTs -> ok (jsonResponse ([] :: [FinanceTransaction]))
            _ -> do
              result <- liftIO $ runExceptT (repoListFinanceTransactions financeTransactionRepo principalUserId mAccountId mFrom mTo)
              case result of
                Left _ -> internalServerError emptyResponse
                Right transactions -> ok (jsonResponse transactions)

    financeTransactionsCategorize = path $ \transactionId -> do
      dir "categorize" $ do
        nullDir
        method POST
        body <- askRq >>= takeRequestBody
        maybe (badRequest "Empty body") (handleTransactionCategorizeBody transactionId) body

    financeTransactionsSplit = path $ \transactionId -> do
      dir "split" $ do
        nullDir
        method POST
        body <- askRq >>= takeRequestBody
        maybe (badRequest "Empty body") (handleTransactionSplitBody transactionId) body

    financeTransactionsLink = do
      nullDir
      method POST
      body <- askRq >>= takeRequestBody
      maybe (badRequest "Empty body") handleTransactionLinkBody body

    financeTransactionsNotes = path $ \transactionId -> do
      dir "notes" $ do
        msum [financeTransactionNoteCreate transactionId, financeTransactionNoteUpdate transactionId, financeTransactionNoteDelete transactionId]

    financeReport = do
      nullDir
      method GET
      request <- askRq
      mFromRaw <- (Just <$> look "from") `mplus` pure Nothing
      mToRaw <- (Just <$> look "to") `mplus` pure Nothing
      let mDirectionRaw = queryParamValues "direction" request
          accountInValues = queryParamValues "accountIn" request
          accountNotInValues = queryParamValues "accountNotIn" request
          categoryInValues = queryParamValues "categoryIn" request
          categoryNotInValues = queryParamValues "categoryNotIn" request
      case (mFromRaw, mToRaw) of
        (Nothing, _) -> badRequest "from is required"
        (_, Nothing) -> badRequest "to is required"
        (Just _, Just _) ->
          case (parseQueryTime "from" mFromRaw, parseQueryTime "to" mToRaw, parseReportDirection mDirectionRaw) of
            (Left message, _, _) -> badRequest message
            (_, Left message, _) -> badRequest message
            (_, _, Left message) -> badRequest message
            (Right (Just fromTs), Right (Just toTs), Right direction)
              | fromTs >= toTs -> badRequest "from must be less than to"
              | hasOverlap accountInValues accountNotInValues -> badRequest "accountIn and accountNotIn must not overlap"
              | hasOverlap categoryInValues categoryNotInValues -> badRequest "categoryIn and categoryNotIn must not overlap"
              | otherwise -> do
                  let reportRequest = FinanceReportRequest
                        { financeReportFrom = fromTs
                        , financeReportTo = toTs
                        , financeReportDirection = direction
                        , financeReportAccountIn = accountInValues
                        , financeReportAccountNotIn = accountNotInValues
                        , financeReportCategoryIn = categoryInValues
                        , financeReportCategoryNotIn = categoryNotInValues
                        }
                  result <- liftIO $ runExceptT (repoGetFinanceReport financeTransactionRepo principalUserId reportRequest)
                  case result of
                    Left _ -> internalServerError emptyResponse
                    Right reportResult -> ok (jsonResponse reportResult)
            _ -> internalServerError emptyResponse

    financeTransactionNoteCreate :: String -> ServerPartT IO Response
    financeTransactionNoteCreate transactionId = do
      nullDir
      method POST
      body <- askRq >>= takeRequestBody
      maybe (badRequest "Empty body") (handleTransactionNoteCreateBody transactionId) body

    financeTransactionNoteUpdate :: String -> ServerPartT IO Response
    financeTransactionNoteUpdate transactionId = path $ \noteId -> do
      nullDir
      method PUT
      body <- askRq >>= takeRequestBody
      maybe (badRequest "Empty body") (handleTransactionNoteUpdateBody transactionId noteId) body

    financeTransactionNoteDelete :: String -> ServerPartT IO Response
    financeTransactionNoteDelete transactionId = path $ \noteId -> do
      nullDir
      method DELETE
      result <- liftIO $ runExceptT (repoDeleteFinanceTransactionNote financeTransactionRepo principalUserId transactionId noteId)
      case result of
        Left NotFound -> notFound (jsonMessage "Transaction or note not found")
        Left _ -> internalServerError emptyResponse
        Right transaction -> ok (jsonResponse transaction)

    handleTransactionCategorizeBody :: String -> RqBody -> ServerPartT IO Response
    handleTransactionCategorizeBody transactionId rqBody =
      case decode' (unBody rqBody) :: Maybe FinanceTransactionCategorizeRequest of
        Nothing -> badRequest "Unable to decode the body as a FinanceTransactionCategorizeRequest"
        Just FinanceTransactionCategorizeRequest { financeTransactionCategorizeCategory } -> do
          result <- liftIO $ runExceptT (repoCategorizeFinanceTransaction financeTransactionRepo principalUserId transactionId financeTransactionCategorizeCategory)
          case result of
            Left NotFound -> notFound (jsonMessage "Transaction or category not found")
            Left WriteFailure -> badRequest "category must reference a selectable category"
            Left AlreadyExists -> setResponseCode 409 >> pure (jsonMessage "Transaction already has an active split")
            Left _ -> internalServerError emptyResponse
            Right transaction -> ok (jsonResponse transaction)

    handleTransactionSplitBody :: String -> RqBody -> ServerPartT IO Response
    handleTransactionSplitBody transactionId rqBody =
      case decode' (unBody rqBody) :: Maybe FinanceTransactionSplitRequest of
        Nothing -> badRequest "Unable to decode the body as a FinanceTransactionSplitRequest"
        Just FinanceTransactionSplitRequest { financeTransactionSplitRows } -> do
          result <- liftIO $ runExceptT (repoSplitFinanceTransaction financeTransactionRepo principalUserId transactionId financeTransactionSplitRows)
          case result of
            Left NotFound -> notFound (jsonMessage "Transaction or category not found")
            Left WriteFailure -> badRequest "splits must contain at least two rows, sum to the transaction amount, and use selectable categories"
            Left _ -> internalServerError emptyResponse
            Right transaction -> ok (jsonResponse transaction)

    handleTransactionLinkBody :: RqBody -> ServerPartT IO Response
    handleTransactionLinkBody rqBody =
      case decode' (unBody rqBody) :: Maybe FinanceTransactionLinkRequest of
        Nothing -> badRequest "Unable to decode the body as a FinanceTransactionLinkRequest"
        Just FinanceTransactionLinkRequest
          { financeTransactionLinkSourceTransactionId
          , financeTransactionLinkTargetTransactionId
          , financeTransactionLinkType
          } ->
            if financeTransactionLinkType /= "transfer"
              then badRequest "linkType must be transfer"
              else do
                result <- liftIO $ runExceptT (repoLinkFinanceTransactions financeTransactionRepo principalUserId financeTransactionLinkSourceTransactionId financeTransactionLinkTargetTransactionId financeTransactionLinkType)
                case result of
                  Left NotFound -> notFound (jsonMessage "Transaction not found")
                  Left WriteFailure -> setResponseCode 409 >> pure (jsonMessage "Invalid transfer link request")
                  Left AlreadyExists -> setResponseCode 409 >> pure (jsonMessage "One or both transactions are already linked")
                  Left _ -> internalServerError emptyResponse
                  Right (sourceTransaction, targetTransaction) ->
                    ok (jsonResponse (object ["source" .= sourceTransaction, "target" .= targetTransaction]))

    handleTransactionNoteCreateBody :: String -> RqBody -> ServerPartT IO Response
    handleTransactionNoteCreateBody transactionId rqBody =
      case decode' (unBody rqBody) :: Maybe FinanceTransactionNoteCreateRequest of
        Nothing -> badRequest "Unable to decode the body as a FinanceTransactionNoteCreateRequest"
        Just FinanceTransactionNoteCreateRequest { financeTransactionNoteCreateText } -> do
          result <- liftIO $ runExceptT (repoAddFinanceTransactionNote financeTransactionRepo principalUserId transactionId financeTransactionNoteCreateText)
          case result of
            Left NotFound -> notFound (jsonMessage "Transaction not found")
            Left WriteFailure -> badRequest "text must not be blank and must not exceed 2000 characters"
            Left _ -> internalServerError emptyResponse
            Right transaction -> ok (jsonResponse transaction)

    handleTransactionNoteUpdateBody :: String -> String -> RqBody -> ServerPartT IO Response
    handleTransactionNoteUpdateBody transactionId noteId rqBody =
      case decode' (unBody rqBody) :: Maybe FinanceTransactionNoteUpdateRequest of
        Nothing -> badRequest "Unable to decode the body as a FinanceTransactionNoteUpdateRequest"
        Just FinanceTransactionNoteUpdateRequest { financeTransactionNoteUpdateText } -> do
          result <- liftIO $ runExceptT (repoUpdateFinanceTransactionNote financeTransactionRepo principalUserId transactionId noteId financeTransactionNoteUpdateText)
          case result of
            Left NotFound -> notFound (jsonMessage "Transaction or note not found")
            Left WriteFailure -> badRequest "text must not be blank and must not exceed 2000 characters"
            Left _ -> internalServerError emptyResponse
            Right transaction -> ok (jsonResponse transaction)

    financeCategoriesList = do
      nullDir
      method GET
      result <- liftIO $ runExceptT (repoListFinanceCategories financeCategoryRepo principalUserId)
      case result of
        Left _ -> internalServerError emptyResponse
        Right categories -> ok (jsonResponse categories)

    financeCategoriesCreate = do
      nullDir
      method POST
      body <- askRq >>= takeRequestBody
      maybe (badRequest "Empty body") handleCategoryCreateBody body

    financeCategoriesUpdate = path $ \categoryId -> do
      nullDir
      method POST
      body <- askRq >>= takeRequestBody
      maybe (badRequest "Empty body") (handleCategoryUpdateBody categoryId) body

    financeCategoriesDelete = path $ \categoryId -> do
      nullDir
      method DELETE
      result <- liftIO $ runExceptT (repoDeleteFinanceCategory financeCategoryRepo principalUserId categoryId)
      case result of
        Left NotFound -> notFound (jsonMessage "Category not found")
        Left AlreadyExists -> setResponseCode 409 >> pure (jsonMessage "Category cannot be deleted")
        Left _ -> internalServerError emptyResponse
        Right () -> ok emptyResponse

    handleCategoryCreateBody :: RqBody -> ServerPartT IO Response
    handleCategoryCreateBody rqBody =
      case decode' (unBody rqBody) :: Maybe FinanceCategoryWriteRequest of
        Nothing -> badRequest "Unable to decode the body as a FinanceCategoryWriteRequest"
        Just writeRequest@FinanceCategoryWriteRequest { financeCategoryWriteName } ->
          case normalizeFinanceCategoryName financeCategoryWriteName of
            Nothing -> badRequest "name must not be empty"
            Just normalizedName -> do
              let normalizedRequest = writeRequest { financeCategoryWriteName = normalizedName }
              result <- liftIO $ runExceptT (repoCreateFinanceCategory financeCategoryRepo principalUserId normalizedRequest)
              case result of
                Left WriteFailure -> badRequest "parentId must reference an accessible category and must not create a cycle"
                Left _ -> internalServerError emptyResponse
                Right category -> ok (jsonResponse category)

    handleCategoryUpdateBody :: String -> RqBody -> ServerPartT IO Response
    handleCategoryUpdateBody categoryId rqBody =
      case decode' (unBody rqBody) :: Maybe FinanceCategoryWriteRequest of
        Nothing -> badRequest "Unable to decode the body as a FinanceCategoryWriteRequest"
        Just writeRequest@FinanceCategoryWriteRequest { financeCategoryWriteName } ->
          case normalizeFinanceCategoryName financeCategoryWriteName of
            Nothing -> badRequest "name must not be empty"
            Just normalizedName -> do
              let normalizedRequest = writeRequest { financeCategoryWriteName = normalizedName }
              result <- liftIO $ runExceptT (repoUpdateFinanceCategory financeCategoryRepo principalUserId categoryId normalizedRequest)
              case result of
                Left NotFound -> notFound (jsonMessage "Category not found")
                Left AlreadyExists -> setResponseCode 409 >> pure (jsonMessage "Built-in categories are read-only")
                Left WriteFailure -> badRequest "parentId must reference an accessible category and must not create a cycle"
                Left _ -> internalServerError emptyResponse
                Right category -> ok (jsonResponse category)

    parseQueryTime :: String -> Maybe String -> Either String (Maybe UTCTime)
    parseQueryTime _ Nothing = Right Nothing
    parseQueryTime fieldName (Just rawValue) =
      case iso8601ParseM rawValue of
        Nothing -> Left (fieldName ++ " must be a valid ISO date-time string")
        Just parsed -> Right (Just parsed)

    queryParamValues key request =
      [ decodeInputValue input
      | (name, input) <- rqInputsQuery request
      , name == key
      ]

    decodeInputValue input =
      case inputValue input of
        Left pathValue -> pathValue
        Right bytes -> LBS8.unpack bytes

    parseReportDirection [] = Right FinanceReportAll
    parseReportDirection [raw] =
      case map toLower raw of
        "all" -> Right FinanceReportAll
        "sent" -> Right FinanceReportSent
        "received" -> Right FinanceReportReceived
        _ -> Left "direction must be one of: sent, received, all"
    parseReportDirection _ = Left "direction must be provided at most once"

    hasOverlap leftValues rightValues =
      let leftSet = Set.fromList leftValues
          rightSet = Set.fromList rightValues
       in not (Set.null (Set.intersection leftSet rightSet))

adminController :: AuthRepository -> String -> AppContext -> ServerPartT IO Response
adminController authRepo bootstrapAdminUsername appContext@AppContext { sessionPrincipal = SessionPrincipal { principalUserId } } =
  requireApprovedAdmin authRepo appContext $
    msum [ dir "pending-signups" $
             msum [ pendingSignupsList
                  , pendingSignupApprove
                  , pendingSignupDelete
                  ]
         , dir "users" $
             msum [ approvedUsersList
                  , approvedUserDelete
                  ]
         ]
  where
    pendingSignupsList = do
      nullDir
      method GET
      pendingUsersResult <- liftIO $ listPendingUsers authRepo
      case pendingUsersResult of
        Left _ -> internalServerError authInternalError
        Right pendingUsers -> ok (jsonResponse (map PendingSignupApproval pendingUsers))

    pendingSignupApprove = do
      dir "approve" $ do
        nullDir
        method POST
        body <- askRq >>= takeRequestBody
        maybe (badRequest "Empty body") handleBody body

    pendingSignupDelete = do
      path $ \username -> do
        nullDir
        method DELETE
        result <- liftIO $ runExceptT $ deletePendingUser authRepo username
        either toServerResponse
               (const $ ok emptyResponse)
               result

    approvedUsersList = do
      nullDir
      method GET
      approvedUsersResult <- liftIO $ listApprovedUsers authRepo
      case approvedUsersResult of
        Left _ -> internalServerError authInternalError
        Right approvedUsers -> ok (jsonResponse approvedUsers)

    approvedUserDelete = do
      path $ \username -> do
        nullDir
        method DELETE
        result <- liftIO $ runExceptT $ deleteApprovedUser authRepo bootstrapAdminUsername principalUserId username
        either toServerResponse
               (const $ ok emptyResponse)
               result

    handleBody :: RqBody -> ServerPartT IO Response
    handleBody rqBody =
      case decode' (unBody rqBody) :: Maybe PendingSignupApproval of
        Nothing -> badRequest "Unable to decode the body as a PendingSignupApproval"
        Just (PendingSignupApproval username)
          | null username -> badRequest "username is required"
          | otherwise -> do
              result <- liftIO $ runExceptT $ approveUser authRepo username
              either toServerResponse
                     (const $ ok emptyResponse)
                     result

tripSharingController :: AuthRepository -> TripSharingRepository -> CalendarRepository -> AppContext -> ServerPartT IO Response
tripSharingController authRepo tripSharingRepo calendarRepo AppContext { sessionPrincipal = SessionPrincipal { principalUserId } } =
  msum [ dir "shares" $ msum [ sharesList
                             , sharesAdd
                             , sharesDelete
                             ]
       , dir "subscriptions" $ msum [ subscriptionsList
                                    , subscriptionsAdd
                                    , subscriptionsDelete
                                    ]
       , dir "period-trips" periodTripsList
       ]
  where
    periodTripsList = do
      nullDir
      method GET
      mStart <- (Just <$> look "start") `mplus` pure Nothing
      mEnd <- (Just <$> look "end") `mplus` pure Nothing
      case parsePeriodTripBounds mStart mEnd of
        Left message -> badRequest message
        Right (periodStart, periodEnd) -> do
          visibleUsersResult <- liftIO $ resolveVisiblePeriodTripUsers tripSharingRepo principalUserId
          case visibleUsersResult of
            Left () -> internalServerError emptyResponse
            Right visibleUsers -> do
              groupsResult <- liftIO $ loadPeriodTripsForUsers calendarRepo visibleUsers periodStart periodEnd
              case groupsResult of
                Left () -> internalServerError emptyResponse
                Right groups -> ok (jsonResponse groups)

    sharesList = do
      nullDir
      method GET
      result <- liftIO $ runExceptT (repoListSharedUsers tripSharingRepo principalUserId)
      case result of
        Left _ -> internalServerError emptyResponse
        Right usernames -> ok (jsonResponse (map TripSharingUser usernames))

    sharesAdd = do
      nullDir
      method POST
      body <- askRq >>= takeRequestBody
      maybe (badRequest "Empty body") handleBody body
      where
        handleBody :: RqBody -> ServerPartT IO Response
        handleBody rqBody =
          case decode' (unBody rqBody) :: Maybe TripSharingUser of
            Nothing -> badRequest "Unable to decode the body as a TripSharingUser"
            Just (TripSharingUser username)
              | null username -> badRequest "username is required"
              | username == principalUserId -> badRequest "username must not be the authenticated user"
              | otherwise -> do
                  exists <- liftIO $ userExists authRepo username
                  if not exists
                    then badRequest "username must reference an existing user"
                    else do
                      result <- liftIO $ runExceptT (repoAddSharedUser tripSharingRepo principalUserId username)
                      case result of
                        Left _ -> internalServerError emptyResponse
                        Right () -> ok emptyResponse

    sharesDelete = do
      method DELETE
      path $ \username -> do
        nullDir
        result <- liftIO $ runExceptT (repoDeleteSharedUser tripSharingRepo principalUserId username)
        case result of
          Left _ -> internalServerError emptyResponse
          Right () -> ok emptyResponse

    subscriptionsList = do
      nullDir
      method GET
      result <- liftIO $ runExceptT (repoListSubscribedUsers tripSharingRepo principalUserId)
      case result of
        Left _ -> internalServerError emptyResponse
        Right usernames -> ok (jsonResponse (map TripSharingUser usernames))

    subscriptionsAdd = do
      nullDir
      method POST
      body <- askRq >>= takeRequestBody
      maybe (badRequest "Empty body") handleBody body
      where
        handleBody :: RqBody -> ServerPartT IO Response
        handleBody rqBody =
          case decode' (unBody rqBody) :: Maybe TripSharingUser of
            Nothing -> badRequest "Unable to decode the body as a TripSharingUser"
            Just (TripSharingUser username)
              | null username -> badRequest "username is required"
              | username == principalUserId -> badRequest "username must not be the authenticated user"
              | otherwise -> do
                  exists <- liftIO $ userExists authRepo username
                  if not exists
                    then badRequest "username must reference an existing user"
                    else do
                      result <- liftIO $ runExceptT (repoAddSubscribedUser tripSharingRepo principalUserId username)
                      case result of
                        Left _ -> internalServerError emptyResponse
                        Right () -> ok emptyResponse

    subscriptionsDelete = do
      method DELETE
      path $ \username -> do
        nullDir
        result <- liftIO $ runExceptT (repoDeleteSubscribedUser tripSharingRepo principalUserId username)
        case result of
          Left _ -> internalServerError emptyResponse
          Right () -> ok emptyResponse

agendaController :: CalendarRepository -> AppContext -> ServerPartT IO Response
agendaController calendarRepo AppContext { sessionPrincipal = SessionPrincipal { principalUserId } } =
  msum [ agendaList
       , agendaCreate
       , agendaDelete
       ]
  where
    agendaList = do
      nullDir
      method GET
      result <- liftIO $ runExceptT (repoListCalendarItemsForUser calendarRepo principalUserId)
      case result of
        Left _ -> internalServerError emptyResponse
        Right items -> ok (jsonResponse items)

    agendaCreate = do
      nullDir
      method POST
      body <- askRq >>= takeRequestBody
      maybe (badRequest "Empty body")
            handleBody
            body
      where
        handleBody :: RqBody -> ServerPartT IO Response
        handleBody rqBody =
          case decode' (unBody rqBody) :: Maybe Agenda.CalendarItem of
            Just (Agenda.NewCalendarItem {Agenda.content}) -> do
              validation <- liftIO $ validateTripWrite calendarRepo principalUserId Nothing content
              case validation of
                TripWriteValid -> do
                  result <- liftIO $ runExceptT (repoCreateCalendarItem calendarRepo principalUserId content)
                  case result of
                    Left _ -> internalServerError emptyResponse
                    Right created -> ok (jsonResponse created)
                TripWriteBadRequest message -> badRequest message
                TripWriteNotFound -> notFound emptyResponse
                TripWriteTechnicalFailure -> internalServerError emptyResponse
            Just (Agenda.ServerCalendarItem {Agenda.content, Agenda.itemId}) -> do
              validation <- liftIO $ validateTripWrite calendarRepo principalUserId (Just itemId) content
              case validation of
                TripWriteValid -> do
                  result <- liftIO $ runExceptT (repoUpdateCalendarItem calendarRepo principalUserId itemId content)
                  case result of
                    Left NotFound -> notFound emptyResponse
                    Left _ -> internalServerError emptyResponse
                    Right updated -> ok (jsonResponse updated)
                TripWriteBadRequest message -> badRequest message
                TripWriteNotFound -> notFound emptyResponse
                TripWriteTechnicalFailure -> internalServerError emptyResponse
            Nothing ->
              case decode' (unBody rqBody) :: Maybe Agenda.ValidateRequest of
                Nothing -> badRequest "Unable to decode the body as a CalendarItem or ValidateRequest"
                Just (Agenda.ValidateRequest itemId minutes) -> do
                  result <- liftIO $ runExceptT (repoUpdateCalendarItemDuration calendarRepo principalUserId itemId minutes)
                  case result of
                    Left NotFound -> notFound emptyResponse
                    Left _ -> internalServerError emptyResponse
                    Right _ -> ok emptyResponse

    agendaDelete = do
      method DELETE
      path $ \itemId -> do
        nullDir
        result <- liftIO $ runExceptT (repoDeleteCalendarItemById calendarRepo principalUserId itemId)
        case result of
          Left NotFound -> notFound emptyResponse
          Left _ -> internalServerError emptyResponse
          Right () -> ok emptyResponse

crudGet ::CRUDEngine crudType a => crudType -> ServerPartT IO Response
crudGet crudConfig = do
    nullDir
    method GET
    log ("crud GET on " ++ crudTypeDenomination crudConfig)
    recover (\err -> genericInternalError $ "Unexpected problem during retrieving all " ++ crudTypeDenomination crudConfig ++ "s:\n\t" ++ show err) (successResponse . handlePotentialParsingErrors) $ getItems crudConfig

successResponse :: ToJSON a => IO a -> ServerPartT IO Response
successResponse action = do
    a <- liftIO action
    (ok . jsonResponse) a

handlePotentialParsingErrors :: [ExceptT CrudReadException IO (Identifiable a)] -> IO [Identifiable a]
handlePotentialParsingErrors = foldM accumulateSuccessOrLogError []

accumulateSuccessOrLogError :: [Identifiable a] -> ExceptT CrudReadException IO (Identifiable a) -> IO [Identifiable a]
accumulateSuccessOrLogError acc parsingResult = do
    parsingTry <- runExceptT parsingResult
    case parsingTry of
        Left e -> do
            log ("Unexpected parsing exception: " ++ show e)
            return acc
        Right succ -> return (succ:acc)

crudPost ::CRUDEngine crudType a => crudType -> ServerPartT IO Response
crudPost crudConfig = do
    nullDir
    method POST
    log ("crud POST on " ++ crudTypeDenomination crudConfig)
    body <- askRq >>= takeRequestBody
    let
        handleBody :: RqBody -> ServerPartT IO Response
        handleBody rqBody = do
            let bodyBS = unBody rqBody
                noteContent = decode bodyBS :: Content a => Maybe a
            log ("Getting body bytestrings: " ++ show bodyBS)
            log ("Getting deserialized content: " ++ show noteContent)
            fmap (createNoteContent crudConfig) noteContent `orElse` genericInternalError "Unexpected problem during note creation"
    fmap handleBody body `orElse` ok emptyResponse

createNoteContent :: CRUDEngine crudType a => crudType -> a -> ServerPartT IO Response
createNoteContent crudConfig noteContent = do
    recover (logThenGenericInternalError crudConfig) (ok . jsonResponse) $ createItem crudConfig noteContent

logThenGenericInternalError :: (Show e, CRUDEngine crudType a) => crudType -> e -> ServerPartT IO Response
logThenGenericInternalError crudConfig e = do
    log ("Unexpected error during creation of " ++ crudTypeDenomination crudConfig ++ ": " ++ show e)
    emptyInternalError

logThenGenericInternalErrorName :: Show e => String -> e -> ServerPartT IO Response
logThenGenericInternalErrorName crudTypeName e = do
    log ("Unexpected error during creation of " ++ crudTypeName ++ ": " ++ show e)
    emptyInternalError

crudDelete :: CRUDEngine crudType a => crudType -> ServerPartT IO Response
crudDelete crudConfig = do
    method DELETE
    log ("crud DELETE on " ++ crudTypeDenomination crudConfig)
    path (\pathId -> do
        nullDir
        recover (handleDeletionError pathId) (\() -> ok emptyResponse) $ deleteItem crudConfig pathId)

handleDeletionError :: String -> CrudWriteException -> ServerPartT IO Response
handleDeletionError pathId err = do
    logDeletionError pathId err
    notFound emptyResponse

logDeletionError pathId s = log ("Error while deleting item " ++ pathId ++ ": " ++ show s)

crudPut :: CRUDEngine crudType a => crudType -> ServerPartT IO Response
crudPut crudConfig = do
    nullDir
    method PUT
    log ("crud PUT on " ++ crudTypeDenomination crudConfig)
    body <- askRq >>= takeRequestBody
    let
        handleBody :: RqBody -> ServerPartT IO Response
        handleBody rqBody = do
            let bodyBS = unBody rqBody
            let noteUpdate = decode bodyBS
            log ("Getting body bytestrings: " ++ show bodyBS)
            log ("Getting deserialized content: " ++ show noteUpdate)
            fmap (handleUpdate crudConfig) noteUpdate `orElse` genericInternalError "Unable to parse body as a NoteUpdate"
    fmap handleBody body `orElse` ok emptyResponse -- do not send back ok when there is no body

handleUpdate :: CRUDEngine crudType a => crudType -> Identifiable a -> ServerPartT IO Response
handleUpdate crudConfig update =
    recoverWith (const . notFound $ jsonMessage "Unable to find storage dir")
                (ok.jsonResponse <$> modifyItem crudConfig update)

serveStaticResource :: ServerPartT IO Response
serveStaticResource = do
    method GET
    log "Serving static resource"
    dir "static" $ uriRest (\rest -> do
        cd <- liftIO getCurrentDirectory -- replace with configuration data directory
        case rest of
          [] -> badRequest "toto"
          [a] -> badRequest "toto"
          (_:withoutFrontSlash) -> serveFileFrom (cd </> "static/") (guessContentTypeM mimeTypes) withoutFrontSlash) -- check serveFileFrom for filesystem attacks with ..

orElse :: Maybe a -> a -> a
(Just a) `orElse` _ = a
_        `orElse` b = b

recoverIO :: (MonadIO m, Monad m) => ExceptT e IO (m a) -> (e -> m a) -> m a
recoverIO exceptT f = join $ liftIO $ fmap (either f id) (runExceptT exceptT)

recoverWith :: (MonadIO m, Monad m) => (e -> m a) -> ExceptT e IO (m a) -> m a
recoverWith = flip recoverIO

orElseIO :: (MonadIO m, Monad m) => MaybeT IO (m a) -> m a -> m a
orElseIO maybe alt = do
    tmp <- liftIO $ runMaybeT maybe
    tmp `orElse` alt

withDefaultIO :: (MonadIO m, Monad m) => m a -> MaybeT IO (m a) -> m a
withDefaultIO = flip orElseIO

recover :: (e -> ServerPartT IO Response) -> (b -> ServerPartT IO Response) -> ExceptT e IO b -> ServerPartT IO Response
recover errorHandler successHandler errorMonad = do
    errorOrNot <- lift $ runExceptT errorMonad
    either errorHandler successHandler errorOrNot

genericInternalError :: String -> ServerPartT IO Response
genericInternalError s = do
    log ("Internal error: \n\t" ++ s)
    emptyInternalError

emptyInternalError :: ServerPartT IO Response
emptyInternalError = internalServerError emptyResponse

log :: (Show s, MonadIO m) => s -> m ()
log s = do
  liftIO $ print s >> hFlush stdout

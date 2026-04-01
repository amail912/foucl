{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module IntegrationTests (runIntegrationTests) where

import Prelude hiding (id)
import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import           Data.ByteString       (ByteString)
import           Data.ByteString.UTF8
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy (fromStrict)
import           Data.Functor.Identity
import           Data.CaseInsensitive (original)
import           GHC.Exts
import           GHC.Generics          (Generic)
import           Network.HTTP.Simple
import           Test.Hspec
import           Test.HUnit
import           Control.Exception (bracket_)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.List (isInfixOf, sortOn)
import           Data.Char (toLower)
import           System.Environment (lookupEnv)
import           System.Directory (getCurrentDirectory, setCurrentDirectory)
import           Data.Text (pack)
import AgendaModel (ItemStatus(..), ItemType(..))
import qualified AgendaModel as Agenda (CalendarItem(..), CalendarItemContent(..), TripItemContent(..))
import Auth (AuthRequest(..), AuthError(..), createUser)
import Model

-- ===================== Constants ==============================

noteEndpoint = "/note"
checklistEndpoint = "/checklist"

runIntegrationTests :: IO ()
runIntegrationTests = do
  let baseUsername = "integration-base-user"
      otherUsername = "integration-other-user"
      thirdUsername = "integration-third-user"
      basePassword = "averystrongpass" :: String
  _ <- signupAndSignin baseUsername basePassword
  ensureSandboxUser otherUsername basePassword
  ensureSandboxUser thirdUsername basePassword
  expectedCookieSecure <- resolveCookieSecureExpectation
  hspec $ do
    describe "Integration Tests" $ do
      it "should satisfy the basics, in one session, of the Very First User's needs, note-wise" $ do
        cookie <- signinOnly baseUsername basePassword
        let
          firstNoteContent = NoteContent { title = Just "First note", noteContent = "First note content" }
          firstNoteNewContent = NoteContent { title = Just "First note new title", noteContent = "This is a new content for the first note" }
        runCrudLifecycle cookie NoteEndpoint firstNoteContent firstNoteNewContent

      it "should satisfy the basics, in one session, of the Very First User's needs, checklist-wise" $ do
        cookie <- signinOnly baseUsername basePassword
        runCrudLifecycle cookie ChecklistEndpoint firstChecklistContent firstChecklistNewContent

      it "should authenticate signin using stored signup password hash" $ do
        signinResponse <- performSigninNoBody baseUsername basePassword
        assertStatusCode "Signin should succeed with valid credentials" 200 signinResponse

        invalidSigninResponse <- performSignin baseUsername "wrongpasswordbad"
        assertStatusCode "Signin should reject invalid password" 401 invalidSigninResponse

      it "should set session cookie attributes on signin" $ do
        signinResponse <- performSignin baseUsername basePassword
        assertStatusCode "Signin should succeed" 200 signinResponse
        let setCookie = BS.unpack <$> getFirstSetCookie signinResponse
        case setCookie of
          Nothing -> assertFailure "Expected Set-Cookie header"
          Just cookieHeader -> do
            assertBool "Cookie should be HttpOnly" ("HttpOnly" `isInfixOf` cookieHeader)
            if expectedCookieSecure
              then assertBool "Cookie should be Secure" ("Secure" `isInfixOf` cookieHeader)
              else assertBool "Cookie should not be Secure" (not ("Secure" `isInfixOf` cookieHeader))
            assertBool "Cookie should set SameSite=Lax" ("SameSite=Lax" `isInfixOf` cookieHeader)

      it "should expire cookie on signout and support all=true revocation" $ do
        cookie1 <- signinOnly baseUsername basePassword
        cookie2 <- signinOnly baseUsername basePassword

        signoutReq <- parseRequest "POST http://localhost:8081/api/signout?all=true"
        signoutResponse <- httpBS $ setRequestMethod "POST"
                                  $ setRequestHeader "Cookie" [BS.pack cookie1]
                                  signoutReq
        assertStatusCode "Signout should succeed" 200 signoutResponse
        let signoutSetCookie = BS.unpack <$> getFirstSetCookie signoutResponse
        case signoutSetCookie of
          Nothing -> assertFailure "Expected Set-Cookie header on signout"
          Just cookieHeader -> do
            assertBool "Expired cookie should set Max-Age=0" ("Max-Age=0" `isInfixOf` cookieHeader)

        protectedReq <- parseRequest "GET http://localhost:8081/api/note"
        protectedResponse <- httpBS $ setRequestMethod "GET"
                                 $ setRequestHeader "Cookie" [BS.pack cookie2]
                                 protectedReq
        assertStatusCode "all=true should revoke sibling sessions" 401 protectedResponse

      it "should enforce signup body size" $ do
        let oversizedPayload = BS.replicate 5000 'a'
        oversizedReq <- parseRequest "POST http://localhost:8081/api/signup"
        oversizedResponse <- httpBS $ setRequestMethod "POST" $ setRequestBodyLBS (fromStrict oversizedPayload) oversizedReq
        assertStatusCode "Oversized signup body should be rejected" 413 oversizedResponse

      it "should enforce signup rate limiting" $ do
        uniquenessSuffix <- round <$> getPOSIXTime

        mapM_ (\i -> do
            let signupPayload = object [ "username" .= ("ratelimit-user-" ++ show uniquenessSuffix ++ "-" ++ show i)
                                       , "password" .= ("averystrongpass" :: String)
                                       ]
            signupReq <- parseRequest "POST http://localhost:8081/api/signup"
            signupResponse <- httpNoBody $ setRequestMethod "POST"
                                      $ setRequestHeader "Content-Type" ["application/json"]
                                      $ setRequestBodyJSON signupPayload signupReq
            assertStatusCode "Signup should be allowed before rate-limit threshold" 200 signupResponse
          ) [1..4]

        blockedReq <- parseRequest "POST http://localhost:8081/api/signup"
        blockedResponse <- httpBS $ setRequestMethod "POST"
                                $ setRequestHeader "Content-Type" ["application/json"]
                                $ setRequestBodyJSON (object [ "username" .= ("ratelimit-user-blocked-" ++ show uniquenessSuffix)
                                                             , "password" .= ("averystrongpass" :: String)
                                                             ])
                                blockedReq
        assertStatusCode "Signup should be blocked when rate limit is reached" 400 blockedResponse

      it "should accept quoted session cookie values for auth" $ do
        rawCookie <- signinOnlyRawCookie baseUsername basePassword
        protectedReq <- parseRequest "GET http://localhost:8081/api/note"
        protectedResponse <- httpBS $ setRequestMethod "GET"
                                 $ setRequestHeader "Cookie" [BS.pack rawCookie]
                                 protectedReq
        assertStatusCode "Quoted cookie should authenticate" 200 protectedResponse

      it "should require auth for agenda endpoints" $ do
        unauthReq <- parseRequest "GET http://localhost:8081/api/v1/calendar-items"
        unauthResponse <- httpBS $ setRequestMethod "GET" unauthReq
        assertStatusCode "Agenda should require auth" 401 unauthResponse

      it "should require auth for trip places endpoint" $ do
        unauthReq <- parseRequest "GET http://localhost:8081/api/v1/trip-places"
        unauthResponse <- httpBS $ setRequestMethod "GET" unauthReq
        assertStatusCode "Trip places should require auth" 401 unauthResponse

      it "should require auth for trip-sharing shares endpoint" $ do
        unauthReq <- parseRequest "GET http://localhost:8081/api/v1/trip-sharing/shares"
        unauthResponse <- httpBS $ setRequestMethod "GET" unauthReq
        assertStatusCode "Trip-sharing shares should require auth" 401 unauthResponse

      it "should expose the fixed trip places catalog" $ do
        cookie <- signinOnly baseUsername basePassword
        places <- getTripPlaces cookie
        repeatedPlaces <- getTripPlaces cookie
        assertEqual "Trip places should match the fixed catalog" expectedTripPlaces places
        assertEqual "Trip places should stay stable across requests" expectedTripPlaces repeatedPlaces

      it "should support share-list add, list, and delete lifecycle" $ do
        cookie <- signinOnly baseUsername basePassword
        clearSharedUsers cookie [otherUsername, thirdUsername]
        assertNoSharedUsers cookie
        addSharedUser cookie otherUsername
        sharedUsers <- getSharedUsersList cookie
        assertEqual "Expected shared user to be listed" [shareUserValue otherUsername] sharedUsers
        deleteSharedUser cookie otherUsername
        assertNoSharedUsers cookie

      it "should keep share additions idempotent" $ do
        cookie <- signinOnly baseUsername basePassword
        clearSharedUsers cookie [otherUsername]
        addSharedUser cookie otherUsername
        addSharedUser cookie otherUsername
        sharedUsers <- getSharedUsersList cookie
        assertEqual "Expected duplicate share add to be ignored" [shareUserValue otherUsername] sharedUsers
        deleteSharedUser cookie otherUsername
        assertNoSharedUsers cookie

      it "should keep deleting a missing shared user idempotent" $ do
        cookie <- signinOnly baseUsername basePassword
        clearSharedUsers cookie [otherUsername]
        deleteSharedUser cookie otherUsername
        assertNoSharedUsers cookie

      it "should reject adding an unknown shared user" $ do
        cookie <- signinOnly baseUsername basePassword
        clearSharedUsers cookie [otherUsername]
        addSharedUserExpectMessage cookie "non-existent-shared-user" "username must reference an existing user"
        assertNoSharedUsers cookie

      it "should reject adding the authenticated user to their own share list" $ do
        cookie <- signinOnly baseUsername basePassword
        clearSharedUsers cookie [otherUsername]
        addSharedUserExpectMessage cookie baseUsername "username must not be the authenticated user"
        assertNoSharedUsers cookie

      it "should reject malformed share add payloads" $ do
        cookie <- signinOnly baseUsername basePassword
        req <- parseRequest "POST http://localhost:8081/api/v1/trip-sharing/shares"
        resp <- httpJSON $ setRequestMethod "POST"
                        $ setRequestHeader "Cookie" [BS.pack cookie]
                        $ setRequestHeader "Content-Type" ["application/json"]
                        $ setRequestBodyJSON (object []) req
        assertStatusCode "Malformed share add payload should return 400" 400 resp
        assertMessageResponse "Unable to decode the body as a TripSharingUser" resp

      it "should isolate share lists by authenticated user" $ do
        ownerCookie <- signinOnly baseUsername basePassword
        otherCookie <- signinOnly otherUsername basePassword
        clearSharedUsers ownerCookie [thirdUsername]
        clearSharedUsers otherCookie [thirdUsername]
        addSharedUser ownerCookie thirdUsername
        deleteSharedUser otherCookie thirdUsername
        ownerSharedUsers <- getSharedUsersList ownerCookie
        otherSharedUsers <- getSharedUsersList otherCookie
        assertEqual "Owner shares should remain visible to the owner only" [shareUserValue thirdUsername] ownerSharedUsers
        assertEqual "Other user should not see owner shares" [] otherSharedUsers
        deleteSharedUser ownerCookie thirdUsername
        assertNoSharedUsers ownerCookie
        assertNoSharedUsers otherCookie

      it "should return shared users in stable order" $ do
        cookie <- signinOnly baseUsername basePassword
        clearSharedUsers cookie [otherUsername, thirdUsername]
        addSharedUser cookie thirdUsername
        addSharedUser cookie otherUsername
        firstRead <- getSharedUsersList cookie
        secondRead <- getSharedUsersList cookie
        let expected = [shareUserValue otherUsername, shareUserValue thirdUsername]
        assertEqual "Expected first read to be sorted" expected firstRead
        assertEqual "Expected repeated reads to stay stable" expected secondRead
        clearSharedUsers cookie [otherUsername, thirdUsername]
        assertNoSharedUsers cookie

      it "should support agenda create/list/update/validate/delete lifecycle" $ do
        cookie <- signinOnly baseUsername basePassword
        assertNoAgendaItems cookie
        created <- createAgendaItem cookie agendaItemContent
        case created of
          Agenda.ServerCalendarItem {} -> do
            let sid = Agenda.itemId created
            assertBool "Created agenda item should have id" (not (null sid))
          _ -> assertFailure "Expected ServerCalendarItem response"
        items <- getAgendaItems cookie
        assertEqual "Agenda list should contain created item" [created] items
        case created of
          Agenda.ServerCalendarItem {} -> do
            let sid = Agenda.itemId created
            let updatedContent = agendaItemContent
                  { Agenda.title = "Updated agenda item"
                  , Agenda.status = EnCours
                  , Agenda.category = Just "updated-category"
                  }
            updateAgendaItem cookie (Agenda.ServerCalendarItem { Agenda.content = updatedContent, Agenda.itemId = sid })
            updatedItemsAfterUpdate <- getAgendaItems cookie
            let expectedUpdated = Agenda.ServerCalendarItem { Agenda.content = updatedContent, Agenda.itemId = sid }
            assertEqual "Agenda item should be updated" [expectedUpdated] updatedItemsAfterUpdate
            validateAgendaItem cookie sid 42
            updatedItems <- getAgendaItems cookie
            let expected = applyDuration 42 (Agenda.ServerCalendarItem { Agenda.content = updatedContent, Agenda.itemId = sid })
            assertEqual "Agenda item should be updated with duration" [expected] updatedItems
            deleteAgendaItem cookie sid
            assertNoAgendaItems cookie
          _ -> assertFailure "Expected ServerCalendarItem response"

      it "should support trip item create/list/update lifecycle" $ do
        cookie <- signinOnly baseUsername basePassword
        assertNoAgendaItems cookie
        created <- createAgendaItem cookie tripItemContent
        case created of
          Agenda.ServerCalendarItem {} -> do
            let sid = Agenda.itemId created
            assertBool "Created trip item should have id" (not (null sid))
          _ -> assertFailure "Expected ServerCalendarItem response"
        items <- getAgendaItems cookie
        assertEqual "Agenda list should contain created trip item" [created] items
        case created of
          Agenda.ServerCalendarItem {} -> do
            let sid = Agenda.itemId created
            let updatedContent = Agenda.TripCalendarItemContent Agenda.TripItemContent
                  { Agenda.tripWindowStart = "2025-02-01T09:00"
                  , Agenda.tripWindowEnd = "2025-02-01T12:30"
                  , Agenda.departurePlaceId = "Le Mesnil"
                  , Agenda.arrivalPlaceId = "St Clair"
                  }
            let updatedItem = Agenda.ServerCalendarItem { Agenda.content = updatedContent, Agenda.itemId = sid }
            updateAgendaItem cookie updatedItem
            updatedItems <- getAgendaItems cookie
            assertEqual "Trip item should be updated" [updatedItem] updatedItems
            deleteAgendaItem cookie sid
            assertNoAgendaItems cookie
          _ -> assertFailure "Expected ServerCalendarItem response"

      it "should return legacy and trip items together from agenda list" $ do
        cookie <- signinOnly baseUsername basePassword
        assertNoAgendaItems cookie
        legacyCreated <- createAgendaItem cookie agendaItemContent
        tripCreated <- createAgendaItem cookie tripItemContent
        items <- getAgendaItems cookie
        assertEqual "Agenda list should contain legacy and trip items"
          (sortAgendaItems [legacyCreated, tripCreated])
          (sortAgendaItems items)
        case (legacyCreated, tripCreated) of
          (Agenda.ServerCalendarItem {}, Agenda.ServerCalendarItem {}) -> do
            deleteAgendaItem cookie (Agenda.itemId legacyCreated)
            deleteAgendaItem cookie (Agenda.itemId tripCreated)
            assertNoAgendaItems cookie
          _ -> assertFailure "Expected stored agenda items with ids"

      it "should reject malformed trip item payloads" $ do
        cookie <- signinOnly baseUsername basePassword
        req <- parseRequest "POST http://localhost:8081/api/v1/calendar-items"
        let body = object
              [ "type" .= ("trip" :: String)
              , "windowStart" .= ("2025-02-01T09:00" :: String)
              , "windowEnd" .= ("2025-02-01T11:00" :: String)
              , "departurePlaceId" .= ("Paris" :: String)
              ]
        resp <- httpNoBody $ setRequestMethod "POST"
                         $ setRequestHeader "Cookie" [BS.pack cookie]
                         $ setRequestHeader "Content-Type" ["application/json"]
                         $ setRequestBodyJSON body req
        assertStatusCode "Malformed trip payload should be rejected" 400 resp

      it "should reject trip create when departure place is unknown" $ do
        cookie <- signinOnly baseUsername basePassword
        assertTripCreateValidationError cookie
          (mkTripContent "2025-02-03T09:00" "2025-02-03T11:00" "Nowhere" "Paris")
          "departurePlaceId must reference an existing trip place"

      it "should reject trip create when arrival place is unknown" $ do
        cookie <- signinOnly baseUsername basePassword
        assertTripCreateValidationError cookie
          (mkTripContent "2025-02-03T09:00" "2025-02-03T11:00" "Paris" "Nowhere")
          "arrivalPlaceId must reference an existing trip place"

      it "should reject trip create when departure and arrival are identical" $ do
        cookie <- signinOnly baseUsername basePassword
        assertTripCreateValidationError cookie
          (mkTripContent "2025-02-03T09:00" "2025-02-03T11:00" "Paris" "Paris")
          "departurePlaceId and arrivalPlaceId must be different"

      it "should reject trip create when windowEnd is not strictly after windowStart" $ do
        cookie <- signinOnly baseUsername basePassword
        assertTripCreateValidationError cookie
          (mkTripContent "2025-02-03T11:00" "2025-02-03T11:00" "Paris" "Le Mesnil")
          "windowEnd must be strictly after windowStart"

      it "should reject overlapping trips for the same user" $ do
        cookie <- signinOnly baseUsername basePassword
        created <- createAgendaItem cookie tripItemContent
        assertTripCreateValidationError cookie
          (mkTripContent "2025-02-01T10:30" "2025-02-01T12:00" "Le Mesnil" "St Clair")
          "trip time window overlaps another trip"
        case created of
          Agenda.ServerCalendarItem {} -> deleteAgendaItem cookie (Agenda.itemId created)
          _ -> assertFailure "Expected stored trip item"
        assertNoAgendaItems cookie

      it "should allow back-to-back trips for the same user" $ do
        cookie <- signinOnly baseUsername basePassword
        firstTrip <- createAgendaItem cookie tripItemContent
        secondTrip <- createAgendaItem cookie (mkTripContent "2025-02-01T11:00" "2025-02-01T13:00" "Le Mesnil" "St Clair")
        items <- getAgendaItems cookie
        assertEqual "Touching trips should both be stored"
          (sortAgendaItems [firstTrip, secondTrip])
          (sortAgendaItems items)
        case (firstTrip, secondTrip) of
          (Agenda.ServerCalendarItem {}, Agenda.ServerCalendarItem {}) -> do
            deleteAgendaItem cookie (Agenda.itemId firstTrip)
            deleteAgendaItem cookie (Agenda.itemId secondTrip)
            assertNoAgendaItems cookie
          _ -> assertFailure "Expected stored trip items"

      it "should reject trip update when it overlaps another trip owned by the same user" $ do
        cookie <- signinOnly baseUsername basePassword
        firstTrip <- createAgendaItem cookie tripItemContent
        secondTrip <- createAgendaItem cookie (mkTripContent "2025-02-01T12:00" "2025-02-01T13:00" "Le Mesnil" "St Clair")
        case (firstTrip, secondTrip) of
          (Agenda.ServerCalendarItem {}, Agenda.ServerCalendarItem {}) -> do
            assertTripUpdateValidationError cookie
              (Agenda.ServerCalendarItem
                { Agenda.itemId = Agenda.itemId secondTrip
                , Agenda.content = mkTripContent "2025-02-01T10:45" "2025-02-01T13:00" "Le Mesnil" "St Clair"
                })
              "trip time window overlaps another trip"
            deleteAgendaItem cookie (Agenda.itemId firstTrip)
            deleteAgendaItem cookie (Agenda.itemId secondTrip)
            assertNoAgendaItems cookie
          _ -> assertFailure "Expected stored trip items"

      it "should not apply one user's trip overlap checks to another user" $ do
        ownerCookie <- signinOnly baseUsername basePassword
        otherCookie <- signinOnly otherUsername basePassword
        ownerTrip <- createAgendaItem ownerCookie tripItemContent
        otherTrip <- createAgendaItem otherCookie (mkTripContent "2025-02-01T10:30" "2025-02-01T12:00" "Paris" "St Clair")
        ownerItems <- getAgendaItems ownerCookie
        otherItems <- getAgendaItems otherCookie
        assertEqual "Owner should keep their trip" [ownerTrip] ownerItems
        assertEqual "Other user should be allowed the same time window" [otherTrip] otherItems
        case (ownerTrip, otherTrip) of
          (Agenda.ServerCalendarItem {}, Agenda.ServerCalendarItem {}) -> do
            deleteAgendaItem ownerCookie (Agenda.itemId ownerTrip)
            deleteAgendaItem otherCookie (Agenda.itemId otherTrip)
            assertNoAgendaItems ownerCookie
            assertNoAgendaItems otherCookie
          _ -> assertFailure "Expected stored trip items"

      it "should isolate agenda items by authenticated user" $ do
        otherCookie <- signinOnly otherUsername basePassword
        ownerCookie <- signinOnly baseUsername basePassword
        assertNoAgendaItems ownerCookie
        assertNoAgendaItems otherCookie
        created <- createAgendaItem ownerCookie agendaItemContent
        ownerItems <- getAgendaItems ownerCookie
        otherItems <- getAgendaItems otherCookie
        assertEqual "Owner should see created agenda item" [created] ownerItems
        assertEqual "Other user should not see owner's agenda item" [] otherItems
        case created of
          Agenda.ServerCalendarItem {} -> do
            let sid = Agenda.itemId created
            let updatedContent = agendaItemContent
                  { Agenda.title = "Cross user update attempt"
                  , Agenda.status = EnCours
                  }
            updateAgendaItemExpectStatus otherCookie (Agenda.ServerCalendarItem { Agenda.content = updatedContent, Agenda.itemId = sid }) 404
            validateAgendaItemExpectStatus otherCookie sid 24 404
            deleteAgendaItemExpectStatus otherCookie sid 404
            deleteAgendaItem ownerCookie sid
            assertNoAgendaItems ownerCookie
            assertNoAgendaItems otherCookie
          _ -> assertFailure "Expected ServerCalendarItem response"
  where
    firstChecklistContent    = ChecklistContent { name = "First checklist"
                                                 , items = [ ChecklistItem { label = "First item label unchecked", checked = False }
                                                           , ChecklistItem { label = "Second item label checked", checked = True }
                                                           ]
                                                 }
    firstChecklistNewContent = ChecklistContent { name = "new checklist"
                                                , items = [ ChecklistItem { label = "Third item label checked", checked = True }
                                                          , ChecklistItem { label = "Fourth item label checked", checked = True }
                                                          ]
                                                }
    agendaItemContent = Agenda.CalendarItemContent
      { Agenda.itemType = Intention
      , Agenda.title = "Test agenda item"
      , Agenda.windowStart = "2025-01-01T09:00"
      , Agenda.windowEnd = "2025-01-01T10:00"
      , Agenda.status = Todo
      , Agenda.sourceItemId = Nothing
      , Agenda.actualDurationMinutes = Nothing
      , Agenda.category = Nothing
      , Agenda.recurrenceRule = Nothing
      , Agenda.recurrenceExceptionDates = []
      }
    tripItemContent = Agenda.TripCalendarItemContent Agenda.TripItemContent
      { Agenda.tripWindowStart = "2025-02-01T09:00"
      , Agenda.tripWindowEnd = "2025-02-01T11:00"
      , Agenda.departurePlaceId = "Paris"
      , Agenda.arrivalPlaceId = "Le Mesnil"
      }
    mkTripContent start end departure arrival =
      Agenda.TripCalendarItemContent Agenda.TripItemContent
        { Agenda.tripWindowStart = start
        , Agenda.tripWindowEnd = end
        , Agenda.departurePlaceId = departure
        , Agenda.arrivalPlaceId = arrival
        }
    applyDuration minutes item =
      case item of
        Agenda.ServerCalendarItem {} ->
          let storedContent = Agenda.content item
              storedId = Agenda.itemId item
          in Agenda.ServerCalendarItem { Agenda.content = storedContent { Agenda.actualDurationMinutes = Just minutes }, Agenda.itemId = storedId }
        Agenda.NewCalendarItem {} ->
          let storedContent = Agenda.content item
          in Agenda.NewCalendarItem { Agenda.content = storedContent { Agenda.actualDurationMinutes = Just minutes } }
    expectedTripPlaces =
      [ object ["name" .= ("Paris" :: String)]
      , object ["name" .= ("Le Mesnil" :: String)]
      , object ["name" .= ("St Clair" :: String)]
      ]
    sortAgendaItems = sortOn agendaItemSortKey
    agendaItemSortKey item =
      case item of
        Agenda.ServerCalendarItem {} -> Agenda.itemId item
        Agenda.NewCalendarItem {} -> "new"

resolveCookieSecureExpectation :: IO Bool
resolveCookieSecureExpectation = do
  mRaw <- lookupEnv "FOUCL_SESSION_COOKIE_SECURE"
  pure $ case fmap (map toLower) mRaw of
    Just "false" -> False
    Just "0" -> False
    _ -> True



getFirstSetCookie :: Response a -> Maybe ByteString
getFirstSetCookie response =
  case [v | (k, v) <- getResponseHeaders response, BS.map toLower (original k) == "set-cookie"] of
    [] -> Nothing
    (x:_) -> Just x

extractCookiePair :: ByteString -> String
extractCookiePair setCookieHeader =
  BS.unpack (cookieName <> "=" <> unquotedCookieValue)
  where
    cookiePair = BS.takeWhile (/= ';') setCookieHeader
    (cookieName, valueWithEq) = BS.break (== '=') cookiePair
    cookieValue = BS.drop 1 valueWithEq
    unquotedCookieValue
      | BS.length cookieValue >= 2 && BS.head cookieValue == '"' && BS.last cookieValue == '"' = BS.init (BS.tail cookieValue)
      | otherwise = cookieValue

signupAndSignin :: String -> String -> IO String
signupAndSignin username password = do
  signupReq <- parseRequest "POST http://localhost:8081/api/signup"
  _ <- httpNoBody $ setRequestMethod "POST"
                $ setRequestHeader "Content-Type" ["application/json"]
                $ setRequestBodyJSON (authPayload username password) signupReq
  signinOnly username password

signinOnly :: String -> String -> IO String
signinOnly username password = do
  signinResponse <- performSignin username password
  assertStatusCode "Signin should succeed" 200 signinResponse
  case getFirstSetCookie signinResponse of
    Nothing -> assertFailure "Expected Set-Cookie header" >> pure ""
    Just header -> pure (extractCookiePair header)

signinOnlyRawCookie :: String -> String -> IO String
signinOnlyRawCookie username password = do
  signinResponse <- performSignin username password
  assertStatusCode "Signin should succeed" 200 signinResponse
  case getFirstSetCookie signinResponse of
    Nothing -> assertFailure "Expected Set-Cookie header" >> pure ""
    Just header -> pure (BS.unpack (BS.takeWhile (/= ';') header))

ensureSandboxUser :: String -> String -> IO ()
ensureSandboxUser username password = do
  cwd <- getCurrentDirectory
  let sandboxDir = cwd ++ "/dist-newstyle/sandbox/foucl"
  bracket_ (setCurrentDirectory sandboxDir) (setCurrentDirectory cwd) $ do
    result <- runExceptT $ createUser $ AuthRequest { username = username, password = pack password }
    case result of
      Right () -> pure ()
      Left UserAlreadyExists -> pure ()
      Left _ -> assertFailure "Expected sandbox user creation to succeed"

authPayload :: String -> String -> Value
authPayload username password =
  object [ "username" .= username
         , "password" .= password
         ]

performSigninNoBody :: String -> String -> IO (Response ())
performSigninNoBody = performSigninWith httpNoBody

performSignin :: String -> String -> IO (Response ByteString)
performSignin = performSigninWith httpBS

performSigninWith :: (Request -> IO (Response a)) -> String -> String -> IO (Response a)
performSigninWith send username password = do
  signinReq <- parseRequest "POST http://localhost:8081/api/signin"
  send $ setRequestMethod "POST"
      $ setRequestHeader "Content-Type" ["application/json"]
      $ setRequestBodyJSON (authPayload username password) signinReq

runCrudLifecycle
  :: ( Content contentType
     , RequestType GET endpointType () [Identifiable contentType]
     , RequestType POST endpointType contentType StorageId
     , RequestType PUT endpointType (Identifiable contentType) StorageId
     )
  => String
  -> endpointType
  -> contentType
  -> contentType
  -> Expectation
runCrudLifecycle cookie endpoint initialContent updatedContent = do
  assertNoItemAtEndpoint cookie endpoint
  createNewContent cookie endpoint initialContent
  [createdItem] <- assertGetWithContent cookie endpoint initialContent
  modifyItem cookie endpoint $ Identifiable (storageId createdItem) updatedContent
  [updatedItem] <- assertGetWithContent cookie endpoint updatedContent
  deleteItem cookie endpoint $ (id . storageId) updatedItem
  assertNoItemAtEndpoint cookie endpoint

assertNoItemAtEndpoint :: (Content contentType, RequestType GET endpointType () [Identifiable contentType]) => String -> endpointType -> Expectation
assertNoItemAtEndpoint cookie endpoint = do
  getResponse :: Response [Identifiable contentType] <- sendRequestWithJSONBodyImplWithCookie (Just cookie) GET endpoint ()
  assertNoNoteInResponse "Failed to start with an empty server" getResponse

createNewContent :: (Content contentType, RequestType POST endpointType contentType StorageId) => String -> endpointType -> contentType -> Expectation
createNewContent cookie endpoint content = do
  postResponse :: Response StorageId <- sendRequestWithJSONBodyImplWithCookie (Just cookie) POST endpoint content
  assertStatusCode200 ("Failed to create item" ++ show content) postResponse

assertGetWithContent :: (Content contentType, RequestType GET endpointType () [Identifiable contentType]) => String -> endpointType -> contentType -> IO [Identifiable contentType]
assertGetWithContent cookie endpoint expectedContent = do
  getResponse <- sendRequestWithJSONBodyImplWithCookie (Just cookie) GET endpoint ()
  assertWithFoundContent ("Failed to retrieve created content " ++ show expectedContent) [expectedContent] getResponse

modifyItem :: (Content contentType, RequestType PUT endpointType (Identifiable contentType) StorageId) => String -> endpointType -> Identifiable contentType -> Expectation
modifyItem cookie endpoint update = do
  putResponse :: Response StorageId <- sendRequestWithJSONBodyImplWithCookie (Just cookie) PUT endpoint update
  assertStatusCode200 ("Failed to apply modification " ++ show update) putResponse

deleteItem :: Endpoint a => String -> a -> String -> Expectation
deleteItem cookie endpoint idToDelete = do
  req <- parseRequest ("DELETE http://localhost:8081" ++ getEndpoint endpoint ++ "/" ++ idToDelete)
  let deleteReq = setRequestMethod "DELETE"
                $ setRequestHeader "Cookie" [BS.pack cookie] req
  deleteResponse <- httpBS deleteReq
  assertStatusCode200 ("Failed to delete item" ++ show idToDelete) deleteResponse

assertWithFoundContent :: Content a => String -> [a] -> Response [Identifiable a] -> IO [Identifiable a]
assertWithFoundContent errorPrefix expectedContents response = do
  assertStatusCode200 errorPrefix response
  assertEqual (errorPrefix ++ "\n\tExpected notes with contents:\n\t" ++ show expectedContents) expectedContents  (map content responseItems)
  return responseItems
  where
    responseItems = getResponseBody response

sendRequestWithJSONBodyImpl :: (RequestType methodType endpointType requestType responseType) =>
  methodType -> endpointType -> requestType -> IO (Response responseType)
sendRequestWithJSONBodyImpl =
    sendRequestWithJSONBodyImplWithCookie Nothing

sendRequestWithJSONBodyImplWithCookie :: (RequestType methodType endpointType requestType responseType) =>
  Maybe String -> methodType -> endpointType -> requestType -> IO (Response responseType)
sendRequestWithJSONBodyImplWithCookie mCookie method endpoint body = do
    req <- parseRequest ("http://localhost:8081" ++ getEndpoint endpoint)
    let withCookie :: Request -> Request
        withCookie = maybe (\req' -> req') (\cookie -> setRequestHeader "Cookie" [BS.pack cookie]) mCookie
    httpJSON $ (setRequestMethod (getMethod method) . withCookie . setRequestHeader "Content-Type" ["application/json"] . setRequestBodyJSON body) req


-- sendRequestWithJSONBodyImpl endpoint method body = httpJSON <$>
--   setRequestBodyJSON body <$>
--     setRequestHeader "Content-Type" ["application/json"] <$>
--       setRequestMethod method <$> parseRequest ("http://localhost:8081" ++ endpoint) 
-- 
-- sendRequestSimple :: (ToJSON requestType) => ByteString -> requestType -> IO (Response ByteString)
-- sendRequestSimple method body = httpBS $
--   setRequestBodyJSON body $
--     setRequestHeader "Content-Type" ["application/json"] $
--       setRequestMethod method "http://localhost:8081/note"

assertNoNoteInResponse errorPrefix response = do
  assertStatusCode200 errorPrefix response
  assertEqual (errorPrefix ++ "Expected no note in response"     ) []  (toList $ getResponseBody response)

assertStatusCode200 :: String -> Response a -> Assertion
assertStatusCode200 errorPrefix response = assertEqual (errorPrefix ++ "Expected 200 response status code") 200 (getResponseStatusCode response)

assertStatusCode :: String -> Int -> Response a -> Assertion
assertStatusCode errorPrefix expectedStatus response = assertEqual (errorPrefix ++ "Expected status code") expectedStatus (getResponseStatusCode response)

modifyNote :: Content a => a -> Identifiable a -> Identifiable a
modifyNote newContent previousNote = Identifiable (storageId previousNote) newContent

class Method a where
    getMethod :: a -> ByteString

data GET = GET
data POST = POST
data PUT = PUT
data DELETE = DELETE

instance Method GET where
    getMethod GET = "GET"

instance Method POST where
    getMethod POST = "POST"

instance Method DELETE where
    getMethod DELETE = "DELETE"

instance Method PUT where
    getMethod PUT = "PUT"

class Endpoint a where
    getEndpoint :: a -> String

data NoteEndpoint = NoteEndpoint
instance Endpoint NoteEndpoint where
    getEndpoint NoteEndpoint = "/api/note"

data ChecklistEndpoint = ChecklistEndpoint
instance Endpoint ChecklistEndpoint where
    getEndpoint ChecklistEndpoint = "/api/checklist"

data CalendarItemsEndpoint = CalendarItemsEndpoint
instance Endpoint CalendarItemsEndpoint where
    getEndpoint CalendarItemsEndpoint = "/api/v1/calendar-items"

data TripPlacesEndpoint = TripPlacesEndpoint
instance Endpoint TripPlacesEndpoint where
    getEndpoint TripPlacesEndpoint = "/api/v1/trip-places"

data TripSharingSharesEndpoint = TripSharingSharesEndpoint
instance Endpoint TripSharingSharesEndpoint where
    getEndpoint TripSharingSharesEndpoint = "/api/v1/trip-sharing/shares"

class (ToJSON requestType, FromJSON responseType, Endpoint endpoint, Method methodType) => RequestType methodType endpoint requestType responseType | endpoint methodType -> requestType, endpoint methodType requestType -> responseType where
    sendRequestWithJSONBody :: endpoint -> methodType -> requestType -> IO (Response responseType)

instance RequestType GET NoteEndpoint () [Identifiable NoteContent] where
    sendRequestWithJSONBody endpoint _ = sendRequestWithJSONBodyImpl GET endpoint

instance RequestType POST NoteEndpoint NoteContent StorageId where
    sendRequestWithJSONBody endpoint _ = sendRequestWithJSONBodyImpl POST endpoint

instance RequestType PUT NoteEndpoint (Identifiable NoteContent) StorageId where
    sendRequestWithJSONBody endpoint _ = sendRequestWithJSONBodyImpl PUT endpoint

instance RequestType GET ChecklistEndpoint () [Identifiable ChecklistContent] where
    sendRequestWithJSONBody endpoint _ = sendRequestWithJSONBodyImpl GET endpoint

instance RequestType POST ChecklistEndpoint ChecklistContent StorageId where
    sendRequestWithJSONBody endpoint _ = sendRequestWithJSONBodyImpl POST endpoint

instance RequestType PUT ChecklistEndpoint (Identifiable ChecklistContent) StorageId where
    sendRequestWithJSONBody endpoint _ = sendRequestWithJSONBodyImpl PUT endpoint

instance RequestType GET CalendarItemsEndpoint () [Agenda.CalendarItem] where
    sendRequestWithJSONBody endpoint _ = sendRequestWithJSONBodyImpl GET endpoint

instance RequestType POST CalendarItemsEndpoint Agenda.CalendarItem Agenda.CalendarItem where
    sendRequestWithJSONBody endpoint _ = sendRequestWithJSONBodyImpl POST endpoint

instance RequestType GET TripPlacesEndpoint () [Value] where
    sendRequestWithJSONBody endpoint _ = sendRequestWithJSONBodyImpl GET endpoint

instance RequestType GET TripSharingSharesEndpoint () [Value] where
    sendRequestWithJSONBody endpoint _ = sendRequestWithJSONBodyImpl GET endpoint

assertNoAgendaItems :: String -> Expectation
assertNoAgendaItems cookie = do
  getResponse :: Response [Agenda.CalendarItem] <- sendRequestWithJSONBodyImplWithCookie (Just cookie) GET CalendarItemsEndpoint ()
  assertStatusCode200 "Agenda should start empty" getResponse
  assertEqual "Expected no agenda items" [] (getResponseBody getResponse)

getAgendaItems :: String -> IO [Agenda.CalendarItem]
getAgendaItems cookie = do
  getResponse :: Response [Agenda.CalendarItem] <- sendRequestWithJSONBodyImplWithCookie (Just cookie) GET CalendarItemsEndpoint ()
  assertStatusCode200 "Agenda list should succeed" getResponse
  pure (getResponseBody getResponse)

getTripPlaces :: String -> IO [Value]
getTripPlaces cookie = do
  getResponse :: Response [Value] <- sendRequestWithJSONBodyImplWithCookie (Just cookie) GET TripPlacesEndpoint ()
  assertStatusCode200 "Trip places list should succeed" getResponse
  pure (getResponseBody getResponse)

getSharedUsersList :: String -> IO [Value]
getSharedUsersList cookie = do
  getResponse :: Response [Value] <- sendRequestWithJSONBodyImplWithCookie (Just cookie) GET TripSharingSharesEndpoint ()
  assertStatusCode200 "Share list should succeed" getResponse
  pure (getResponseBody getResponse)

assertNoSharedUsers :: String -> IO ()
assertNoSharedUsers cookie = do
  sharedUsers <- getSharedUsersList cookie
  assertEqual "Expected no shared users" [] sharedUsers

addSharedUser :: String -> String -> IO ()
addSharedUser cookie username = do
  req <- parseRequest "POST http://localhost:8081/api/v1/trip-sharing/shares"
  resp <- httpNoBody $ setRequestMethod "POST"
                   $ setRequestHeader "Cookie" [BS.pack cookie]
                   $ setRequestHeader "Content-Type" ["application/json"]
                   $ setRequestBodyJSON (object ["username" .= username]) req
  assertStatusCode "Share add should succeed" 200 resp

addSharedUserExpectMessage :: String -> String -> String -> IO ()
addSharedUserExpectMessage cookie username expectedMessage = do
  req <- parseRequest "POST http://localhost:8081/api/v1/trip-sharing/shares"
  resp <- httpJSON $ setRequestMethod "POST"
                  $ setRequestHeader "Cookie" [BS.pack cookie]
                  $ setRequestHeader "Content-Type" ["application/json"]
                  $ setRequestBodyJSON (object ["username" .= username]) req
  assertStatusCode "Share add validation should return 400" 400 resp
  assertMessageResponse expectedMessage resp

deleteSharedUser :: String -> String -> IO ()
deleteSharedUser cookie username = do
  req <- parseRequest ("DELETE http://localhost:8081/api/v1/trip-sharing/shares/" ++ username)
  resp <- httpNoBody $ setRequestMethod "DELETE"
                   $ setRequestHeader "Cookie" [BS.pack cookie] req
  assertStatusCode "Share delete should succeed" 200 resp

clearSharedUsers :: String -> [String] -> IO ()
clearSharedUsers cookie = mapM_ (deleteSharedUser cookie)

shareUserValue :: String -> Value
shareUserValue username = object ["username" .= username]

createAgendaItem :: String -> Agenda.CalendarItemContent -> IO Agenda.CalendarItem
createAgendaItem cookie content = do
  postResponse :: Response Agenda.CalendarItem <- sendRequestWithJSONBodyImplWithCookie (Just cookie) POST CalendarItemsEndpoint (Agenda.NewCalendarItem { Agenda.content = content })
  assertStatusCode200 "Agenda create should succeed" postResponse
  pure (getResponseBody postResponse)

updateAgendaItem :: String -> Agenda.CalendarItem -> IO ()
updateAgendaItem cookie item = do
  postResponse :: Response Agenda.CalendarItem <- sendRequestWithJSONBodyImplWithCookie (Just cookie) POST CalendarItemsEndpoint item
  assertStatusCode200 "Agenda update should succeed" postResponse

updateAgendaItemExpectStatus :: String -> Agenda.CalendarItem -> Int -> IO ()
updateAgendaItemExpectStatus cookie item expectedStatus = do
  req <- parseRequest "POST http://localhost:8081/api/v1/calendar-items"
  resp <- httpNoBody $ setRequestMethod "POST"
                   $ setRequestHeader "Cookie" [BS.pack cookie]
                   $ setRequestHeader "Content-Type" ["application/json"]
                   $ setRequestBodyJSON item req
  assertStatusCode "Agenda update should return expected status" expectedStatus resp

assertTripCreateValidationError :: String -> Agenda.CalendarItemContent -> String -> IO ()
assertTripCreateValidationError cookie content expectedMessage =
  assertAgendaPostValidationError cookie (Agenda.NewCalendarItem { Agenda.content = content }) expectedMessage

assertTripUpdateValidationError :: String -> Agenda.CalendarItem -> String -> IO ()
assertTripUpdateValidationError cookie item expectedMessage =
  assertAgendaPostValidationError cookie item expectedMessage

assertAgendaPostValidationError :: String -> Agenda.CalendarItem -> String -> IO ()
assertAgendaPostValidationError cookie item expectedMessage = do
  req <- parseRequest "POST http://localhost:8081/api/v1/calendar-items"
  resp <- httpJSON $ setRequestMethod "POST"
                  $ setRequestHeader "Cookie" [BS.pack cookie]
                  $ setRequestHeader "Content-Type" ["application/json"]
                  $ setRequestBodyJSON item req
  assertStatusCode "Agenda validation error should return 400" 400 resp
  assertMessageResponse expectedMessage resp

assertMessageResponse :: String -> Response Value -> Assertion
assertMessageResponse expectedMessage response =
  case getResponseBody response of
    Object value ->
      case parseMaybe (.: "message") value of
        Just actualMessage -> assertEqual "Expected validation error message" expectedMessage (actualMessage :: String)
        Nothing -> assertFailure "Expected response body to contain a message field"
    _ -> assertFailure "Expected JSON object error response"

validateAgendaItem :: String -> String -> Int -> IO ()
validateAgendaItem cookie itemId minutes = do
  req <- parseRequest "POST http://localhost:8081/api/v1/calendar-items"
  let body = object [ "id" .= itemId
                    , "duree_reelle_minutes" .= minutes
                    ]
  resp <- httpNoBody $ setRequestMethod "POST"
                   $ setRequestHeader "Cookie" [BS.pack cookie]
                   $ setRequestHeader "Content-Type" ["application/json"]
                   $ setRequestBodyJSON body req
  assertStatusCode "Agenda validate should succeed" 200 resp

validateAgendaItemExpectStatus :: String -> String -> Int -> Int -> IO ()
validateAgendaItemExpectStatus cookie itemId minutes expectedStatus = do
  req <- parseRequest "POST http://localhost:8081/api/v1/calendar-items"
  let body = object [ "id" .= itemId
                    , "duree_reelle_minutes" .= minutes
                    ]
  resp <- httpNoBody $ setRequestMethod "POST"
                   $ setRequestHeader "Cookie" [BS.pack cookie]
                   $ setRequestHeader "Content-Type" ["application/json"]
                   $ setRequestBodyJSON body req
  assertStatusCode "Agenda validate should return expected status" expectedStatus resp

deleteAgendaItem :: String -> String -> IO ()
deleteAgendaItem cookie itemId = deleteAgendaItemExpectStatus cookie itemId 200

deleteAgendaItemExpectStatus :: String -> String -> Int -> IO ()
deleteAgendaItemExpectStatus cookie itemId expectedStatus = do
  req <- parseRequest ("DELETE http://localhost:8081/api/v1/calendar-items/" ++ itemId)
  resp <- httpNoBody $ setRequestMethod "DELETE"
                   $ setRequestHeader "Cookie" [BS.pack cookie] req
  assertStatusCode "Agenda delete should return expected status" expectedStatus resp

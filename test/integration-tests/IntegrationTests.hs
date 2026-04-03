{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module IntegrationTests (runIntegrationTests) where

import Prelude hiding (id)
import qualified Prelude
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
import           Control.Monad (when)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.List (isInfixOf, sortOn)
import           Data.Char (toLower)
import           System.Environment (lookupEnv)
import           System.Directory (getCurrentDirectory, setCurrentDirectory, doesDirectoryExist, removeDirectoryRecursive)
import           Data.Text (pack)
import AgendaModel (ItemStatus(..), ItemType(..))
import qualified AgendaModel as Agenda (CalendarItem(..), CalendarItemContent(..), TripItemContent(..))
import Auth (AuthRequest(..), AuthError(..), createUserWithBootstrapAdmin, approveUser)
import Model

-- ===================== Constants ==============================

noteEndpoint = "/note"
checklistEndpoint = "/checklist"

runIntegrationTests :: IO ()
runIntegrationTests = do
  let baseUsername = "admin"
      otherUsername = "integration-other-user"
      thirdUsername = "integration-third-user"
      basePassword = "averystrongpass" :: String
  resetSandboxUser baseUsername
  _ <- signupAndSignin baseUsername basePassword
  ensureApprovedSandboxUser baseUsername baseUsername basePassword
  ensureApprovedSandboxUser baseUsername otherUsername basePassword
  ensureApprovedSandboxUser baseUsername thirdUsername basePassword
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
        signinResponse <- performSigninJSON baseUsername basePassword
        assertStatusCode "Signin should succeed with valid credentials" 200 signinResponse
        assertSigninProfileResponse baseUsername ["admin"] True signinResponse

        invalidSigninResponse <- performSignin baseUsername "wrongpasswordbad"
        assertStatusCode "Signin should reject invalid password" 401 invalidSigninResponse

      it "should return the authenticated profile for a signed-in user" $ do
        signinResponse <- performSigninJSON baseUsername basePassword
        assertStatusCode "Signin should succeed with valid credentials" 200 signinResponse
        cookie <- signinOnly baseUsername basePassword
        profileResponse <- getAuthProfile cookie
        assertStatusCode "Auth profile should succeed" 200 profileResponse
        assertEqual "Auth profile response should match signin profile response"
          (getResponseBody signinResponse)
          (getResponseBody profileResponse)

      it "should reject unauthenticated auth profile access" $ do
        req <- parseRequest "GET http://localhost:8081/api/auth/profile"
        resp <- httpJSON $ setRequestMethod "GET" req
        assertStatusCode "Auth profile should require auth" 401 resp
        assertMessageResponse "Not authenticated" resp

      it "should enforce signup rate limiting" $ do
        uniquenessSuffix <- round . (* 1000000) <$> getPOSIXTime

        mapM_ (\i -> do
            let signupPayload = object [ "username" .= ("ratelimit-" ++ show uniquenessSuffix ++ "-" ++ show i)
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
                                $ setRequestBodyJSON (object [ "username" .= ("rlblock-" ++ show uniquenessSuffix)
                                                             , "password" .= ("averystrongpass" :: String)
                                                             ])
                                blockedReq
        assertStatusCode "Signup should be blocked when rate limit is reached" 400 blockedResponse

      it "should create pending signups that cannot sign in before admin approval" $ do
        uniquenessSuffix <- round . (* 1000000) <$> getPOSIXTime
        let pendingUsername = "pending-" ++ show uniquenessSuffix
        ensurePendingSandboxUser baseUsername pendingUsername basePassword

        pendingSigninResponse <- performSigninJSON pendingUsername basePassword
        assertStatusCode "Pending user should be blocked from signin" 403 pendingSigninResponse
        assertMessageResponse "Account pending approval" pendingSigninResponse

      it "should allow an admin to list and approve pending signups" $ do
        uniquenessSuffix <- round . (* 1000000) <$> getPOSIXTime
        let pendingUsername = "approvable-" ++ show uniquenessSuffix
        ensurePendingSandboxUser baseUsername pendingUsername basePassword

        adminCookie <- signinOnly baseUsername basePassword
        pendingUsers <- getPendingSignups adminCookie
        assertBool "Pending signup should be visible to admin" (pendingSignupValue pendingUsername `elem` pendingUsers)

        approvePendingSignup adminCookie pendingUsername

        approvedSigninResponse <- performSigninNoBody pendingUsername basePassword
        assertStatusCode "Approved user should be able to sign in" 200 approvedSigninResponse

      it "should reject non-admin access to pending signup admin endpoints" $ do
        uniquenessSuffix <- round . (* 1000000) <$> getPOSIXTime
        let memberUsername = "member-" ++ show uniquenessSuffix
        ensureApprovedSandboxUser baseUsername memberUsername basePassword
        memberCookie <- signinOnly memberUsername basePassword

        pendingReq <- parseRequest "GET http://localhost:8081/api/v1/admin/pending-signups"
        pendingResp <- httpBS $ setRequestMethod "GET"
                            $ setRequestHeader "Cookie" [BS.pack memberCookie]
                            pendingReq
        assertStatusCode "Non-admin should be forbidden from listing pending signups" 403 pendingResp

        approveReq <- parseRequest "POST http://localhost:8081/api/v1/admin/pending-signups/approve"
        approveResp <- httpJSON $ setRequestMethod "POST"
                             $ setRequestHeader "Cookie" [BS.pack memberCookie]
                             $ setRequestHeader "Content-Type" ["application/json"]
                             $ setRequestBodyJSON (object ["username" .= baseUsername]) approveReq
        assertStatusCode "Non-admin should be forbidden from approving pending signups" 403 approveResp
        assertMessageResponse "Admin privileges required" approveResp

      it "should set session cookie attributes on signin" $ do
        signinResponse <- performSignin baseUsername basePassword
        assertStatusCode "Signin should succeed" 200 signinResponse
        let setCookie = BS.unpack <$> getFirstSetCookie signinResponse
        case setCookie of
          Nothing -> assertFailure "Expected Set-Cookie header"
          Just cookieHeader -> do
            let cookieHeaderLower = map toLower cookieHeader
            assertBool "Cookie should be HttpOnly" ("httponly" `isInfixOf` cookieHeaderLower)
            if expectedCookieSecure
              then assertBool "Cookie should be Secure" ("secure" `isInfixOf` cookieHeaderLower)
              else assertBool "Cookie should not be Secure" (not ("secure" `isInfixOf` cookieHeaderLower))
            assertBool "Cookie should set SameSite=Lax" ("samesite=lax" `isInfixOf` cookieHeaderLower)

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

      it "should require auth for trip-sharing subscriptions endpoint" $ do
        unauthReq <- parseRequest "GET http://localhost:8081/api/v1/trip-sharing/subscriptions"
        unauthResponse <- httpBS $ setRequestMethod "GET" unauthReq
        assertStatusCode "Trip-sharing subscriptions should require auth" 401 unauthResponse

      it "should require auth for trip-sharing period-trips endpoint" $ do
        unauthReq <- periodTripsRequest Nothing (Just "2025-03-10T00:00") (Just "2025-03-11T00:00")
        unauthResponse <- httpBS unauthReq
        assertStatusCode "Trip-sharing period-trips should require auth" 401 unauthResponse

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

      it "should support subscription-list add, list, and delete lifecycle" $ do
        cookie <- signinOnly baseUsername basePassword
        clearSubscribedUsers cookie [otherUsername, thirdUsername]
        assertNoSubscribedUsers cookie
        addSubscribedUser cookie otherUsername
        subscribedUsers <- getSubscribedUsersList cookie
        assertEqual "Expected subscribed user to be listed" [tripSharingUserValue otherUsername] subscribedUsers
        deleteSubscribedUser cookie otherUsername
        assertNoSubscribedUsers cookie

      it "should keep subscription additions idempotent" $ do
        cookie <- signinOnly baseUsername basePassword
        clearSubscribedUsers cookie [otherUsername]
        addSubscribedUser cookie otherUsername
        addSubscribedUser cookie otherUsername
        subscribedUsers <- getSubscribedUsersList cookie
        assertEqual "Expected duplicate subscription add to be ignored" [tripSharingUserValue otherUsername] subscribedUsers
        deleteSubscribedUser cookie otherUsername
        assertNoSubscribedUsers cookie

      it "should keep deleting a missing subscribed user idempotent" $ do
        cookie <- signinOnly baseUsername basePassword
        clearSubscribedUsers cookie [otherUsername]
        deleteSubscribedUser cookie otherUsername
        assertNoSubscribedUsers cookie

      it "should reject adding an unknown subscribed user" $ do
        cookie <- signinOnly baseUsername basePassword
        clearSubscribedUsers cookie [otherUsername]
        addSubscribedUserExpectMessage cookie "non-existent-subscribed-user" "username must reference an existing user"
        assertNoSubscribedUsers cookie

      it "should reject subscribing to the authenticated user" $ do
        cookie <- signinOnly baseUsername basePassword
        clearSubscribedUsers cookie [otherUsername]
        addSubscribedUserExpectMessage cookie baseUsername "username must not be the authenticated user"
        assertNoSubscribedUsers cookie

      it "should reject malformed subscription add payloads" $ do
        cookie <- signinOnly baseUsername basePassword
        req <- parseRequest "POST http://localhost:8081/api/v1/trip-sharing/subscriptions"
        resp <- httpJSON $ setRequestMethod "POST"
                        $ setRequestHeader "Cookie" [BS.pack cookie]
                        $ setRequestHeader "Content-Type" ["application/json"]
                        $ setRequestBodyJSON (object []) req
        assertStatusCode "Malformed subscription add payload should return 400" 400 resp
        assertMessageResponse "Unable to decode the body as a TripSharingUser" resp

      it "should isolate subscription lists by authenticated user" $ do
        ownerCookie <- signinOnly baseUsername basePassword
        otherCookie <- signinOnly otherUsername basePassword
        clearSubscribedUsers ownerCookie [thirdUsername]
        clearSubscribedUsers otherCookie [thirdUsername]
        addSubscribedUser ownerCookie thirdUsername
        deleteSubscribedUser otherCookie thirdUsername
        ownerSubscribedUsers <- getSubscribedUsersList ownerCookie
        otherSubscribedUsers <- getSubscribedUsersList otherCookie
        assertEqual "Owner subscriptions should remain visible to the owner only" [tripSharingUserValue thirdUsername] ownerSubscribedUsers
        assertEqual "Other user should not see owner subscriptions" [] otherSubscribedUsers
        deleteSubscribedUser ownerCookie thirdUsername
        assertNoSubscribedUsers ownerCookie
        assertNoSubscribedUsers otherCookie

      it "should return subscribed users in stable order" $ do
        cookie <- signinOnly baseUsername basePassword
        clearSubscribedUsers cookie [otherUsername, thirdUsername]
        addSubscribedUser cookie thirdUsername
        addSubscribedUser cookie otherUsername
        firstRead <- getSubscribedUsersList cookie
        secondRead <- getSubscribedUsersList cookie
        let expected = [tripSharingUserValue otherUsername, tripSharingUserValue thirdUsername]
        assertEqual "Expected first subscription read to be sorted" expected firstRead
        assertEqual "Expected repeated subscription reads to stay stable" expected secondRead
        clearSubscribedUsers cookie [otherUsername, thirdUsername]
        assertNoSubscribedUsers cookie

      it "should keep subscriptions independent from shares" $ do
        cookie <- signinOnly baseUsername basePassword
        clearSharedUsers cookie [otherUsername]
        clearSubscribedUsers cookie [otherUsername]
        addSharedUser cookie otherUsername
        addSubscribedUser cookie thirdUsername
        sharedUsers <- getSharedUsersList cookie
        subscribedUsers <- getSubscribedUsersList cookie
        assertEqual "Expected shares to stay unchanged by subscriptions" [tripSharingUserValue otherUsername] sharedUsers
        assertEqual "Expected subscriptions to stay independent from shares" [tripSharingUserValue thirdUsername] subscribedUsers
        clearSharedUsers cookie [otherUsername]
        clearSubscribedUsers cookie [otherUsername, thirdUsername]
        assertNoSharedUsers cookie
        assertNoSubscribedUsers cookie

      it "should reject invalid period-trip queries" $ do
        cookie <- signinOnly baseUsername basePassword
        assertPeriodTripsValidationError cookie Nothing (Just "2025-03-10T12:00") "start is required"
        assertPeriodTripsValidationError cookie (Just "not-a-date") (Just "2025-03-10T12:00") "start must be a valid ISO date-time string"
        assertPeriodTripsValidationError cookie (Just "2025-03-10T12:00") (Just "2025-03-10T12:00") "end must be strictly after start"

      it "should require both subscriptions and shares to expose period trips" $ do
        baseCookie <- signinOnly baseUsername basePassword
        otherCookie <- signinOnly otherUsername basePassword
        thirdCookie <- signinOnly thirdUsername basePassword
        clearAgendaItems baseCookie
        clearAgendaItems otherCookie
        clearAgendaItems thirdCookie
        clearSharedUsers baseCookie [otherUsername, thirdUsername]
        clearSharedUsers otherCookie [baseUsername, thirdUsername]
        clearSharedUsers thirdCookie [baseUsername, otherUsername]
        clearSubscribedUsers baseCookie [otherUsername, thirdUsername]
        clearSubscribedUsers otherCookie [baseUsername, thirdUsername]
        clearSubscribedUsers thirdCookie [baseUsername, otherUsername]
        otherTrip <- createAgendaItem otherCookie (mkTripContent "2025-03-10T09:00" "2025-03-10T10:00" "Paris" "Le Mesnil")
        thirdTrip <- createAgendaItem thirdCookie (mkTripContent "2025-03-10T11:00" "2025-03-10T12:00" "Le Mesnil" "St Clair")
        addSubscribedUser baseCookie otherUsername
        addSharedUser thirdCookie baseUsername
        invisibleTrips <- getPeriodTripsList baseCookie "2025-03-10T00:00" "2025-03-11T00:00"
        assertEqual "Trips should stay hidden without both relations" [] invisibleTrips
        addSharedUser otherCookie baseUsername
        addSubscribedUser baseCookie thirdUsername
        firstRead <- getPeriodTripsList baseCookie "2025-03-10T00:00" "2025-03-11T00:00"
        secondRead <- getPeriodTripsList baseCookie "2025-03-10T00:00" "2025-03-11T00:00"
        let expected =
              [ periodTripsUserValue otherUsername [otherTrip]
              , periodTripsUserValue thirdUsername [thirdTrip]
              ]
        assertEqual "Visible trip users should be returned in stable username order" expected firstRead
        assertEqual "Repeated period-trip reads should stay stable" expected secondRead
        clearAgendaItems baseCookie
        clearAgendaItems otherCookie
        clearAgendaItems thirdCookie
        clearSharedUsers baseCookie [otherUsername, thirdUsername]
        clearSharedUsers otherCookie [baseUsername, thirdUsername]
        clearSharedUsers thirdCookie [baseUsername, otherUsername]
        clearSubscribedUsers baseCookie [otherUsername, thirdUsername]
        clearSubscribedUsers otherCookie [baseUsername, thirdUsername]
        clearSubscribedUsers thirdCookie [baseUsername, otherUsername]

      it "should include the last seed trip before start and only in-period trips with start inclusive and end exclusive" $ do
        baseCookie <- signinOnly baseUsername basePassword
        otherCookie <- signinOnly otherUsername basePassword
        clearAgendaItems baseCookie
        clearAgendaItems otherCookie
        clearSharedUsers otherCookie [baseUsername]
        clearSubscribedUsers baseCookie [otherUsername]
        addSharedUser otherCookie baseUsername
        addSubscribedUser baseCookie otherUsername
        earlyTrip <- createAgendaItem otherCookie (mkTripContent "2025-03-20T07:00" "2025-03-20T08:00" "Paris" "Le Mesnil")
        seedTrip <- createAgendaItem otherCookie (mkTripContent "2025-03-20T09:00" "2025-03-20T09:30" "Le Mesnil" "Paris")
        startTrip <- createAgendaItem otherCookie (mkTripContent "2025-03-20T10:00" "2025-03-20T11:00" "Paris" "St Clair")
        middleTrip <- createAgendaItem otherCookie (mkTripContent "2025-03-20T12:00" "2025-03-20T13:00" "St Clair" "Le Mesnil")
        endTrip <- createAgendaItem otherCookie (mkTripContent "2025-03-20T15:00" "2025-03-20T16:00" "Le Mesnil" "Paris")
        trips <- getPeriodTripsList baseCookie "2025-03-20T10:00" "2025-03-20T15:00"
        let expected = [periodTripsUserValue otherUsername [seedTrip, startTrip, middleTrip]]
        assertEqual "Expected seed trip and in-period trips only" expected trips
        case (earlyTrip, endTrip) of
          (Agenda.ServerCalendarItem {}, Agenda.ServerCalendarItem {}) -> pure ()
          _ -> assertFailure "Expected stored trip items"
        clearAgendaItems baseCookie
        clearAgendaItems otherCookie
        clearSharedUsers otherCookie [baseUsername]
        clearSubscribedUsers baseCookie [otherUsername]

      it "should exclude users without qualifying trips and ignore legacy agenda items" $ do
        baseCookie <- signinOnly baseUsername basePassword
        otherCookie <- signinOnly otherUsername basePassword
        thirdCookie <- signinOnly thirdUsername basePassword
        clearAgendaItems baseCookie
        clearAgendaItems otherCookie
        clearAgendaItems thirdCookie
        clearSharedUsers otherCookie [baseUsername]
        clearSharedUsers thirdCookie [baseUsername]
        clearSubscribedUsers baseCookie [otherUsername, thirdUsername]
        addSharedUser otherCookie baseUsername
        addSharedUser thirdCookie baseUsername
        addSubscribedUser baseCookie otherUsername
        addSubscribedUser baseCookie thirdUsername
        _ <- createAgendaItem otherCookie agendaItemContent
        _ <- createAgendaItem thirdCookie (mkTripContent "2025-03-30T15:00" "2025-03-30T16:00" "Paris" "Le Mesnil")
        trips <- getPeriodTripsList baseCookie "2025-03-30T10:00" "2025-03-30T12:00"
        assertEqual "Visible users without a seed trip or in-period trip should be excluded" [] trips
        clearAgendaItems baseCookie
        clearAgendaItems otherCookie
        clearAgendaItems thirdCookie
        clearSharedUsers otherCookie [baseUsername]
        clearSharedUsers thirdCookie [baseUsername]
        clearSubscribedUsers baseCookie [otherUsername, thirdUsername]

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



resetSandboxUser :: String -> IO ()
resetSandboxUser username = do
  cwd <- getCurrentDirectory
  let userDir = cwd ++ "/dist-newstyle/sandbox/foucl/data/users/" ++ username
  exists <- doesDirectoryExist userDir
  when exists $ removeDirectoryRecursive userDir

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

ensureApprovedSandboxUser :: String -> String -> String -> IO ()
ensureApprovedSandboxUser bootstrapAdminUsername username password = do
  cwd <- getCurrentDirectory
  let sandboxDir = cwd ++ "/dist-newstyle/sandbox/foucl"
  bracket_ (setCurrentDirectory sandboxDir) (setCurrentDirectory cwd) $ do
    result <- runExceptT $ createUserWithBootstrapAdmin (Just bootstrapAdminUsername) $ AuthRequest { username = username, password = pack password }
    case result of
      Right () -> do
        approvalResult <- runExceptT $ approveUser username
        case approvalResult of
          Right () -> pure ()
          Left _ -> assertFailure "Expected sandbox user approval to succeed"
      Left UserAlreadyExists -> do
        approvalResult <- runExceptT $ approveUser username
        case approvalResult of
          Right () -> pure ()
          Left _ -> assertFailure "Expected sandbox user approval to succeed"
      Left _ -> assertFailure "Expected sandbox user creation to succeed"

ensurePendingSandboxUser :: String -> String -> String -> IO ()
ensurePendingSandboxUser bootstrapAdminUsername username password = do
  cwd <- getCurrentDirectory
  let sandboxDir = cwd ++ "/dist-newstyle/sandbox/foucl"
  bracket_ (setCurrentDirectory sandboxDir) (setCurrentDirectory cwd) $ do
    result <- runExceptT $ createUserWithBootstrapAdmin (Just bootstrapAdminUsername) $ AuthRequest { username = username, password = pack password }
    case result of
      Right () -> pure ()
      Left UserAlreadyExists -> pure ()
      Left _ -> assertFailure "Expected pending sandbox user creation to succeed"

authPayload :: String -> String -> Value
authPayload username password =
  object [ "username" .= username
         , "password" .= password
         ]

performSigninNoBody :: String -> String -> IO (Response ())
performSigninNoBody = performSigninWith httpNoBody

performSignin :: String -> String -> IO (Response ByteString)
performSignin = performSigninWith httpBS

performSigninJSON :: String -> String -> IO (Response Value)
performSigninJSON = performSigninWith httpJSON

performSigninWith :: (Request -> IO (Response a)) -> String -> String -> IO (Response a)
performSigninWith send username password = do
  signinReq <- parseRequest "POST http://localhost:8081/api/signin"
  send $ setRequestMethod "POST"
      $ setRequestHeader "Content-Type" ["application/json"]
      $ setRequestBodyJSON (authPayload username password) signinReq

getAuthProfile :: String -> IO (Response Value)
getAuthProfile cookie = do
  req <- parseRequest "GET http://localhost:8081/api/auth/profile"
  httpJSON $ setRequestMethod "GET"
           $ setRequestHeader "Cookie" [BS.pack cookie] req

getPendingSignups :: String -> IO [Value]
getPendingSignups cookie = do
  req <- parseRequest "GET http://localhost:8081/api/v1/admin/pending-signups"
  resp <- httpJSON $ setRequestMethod "GET"
                  $ setRequestHeader "Cookie" [BS.pack cookie]
                  req
  assertStatusCode "Pending signup list should succeed" 200 resp
  case getResponseBody resp of
    Array items -> pure (toList items)
    _ -> assertFailure "Expected pending signups array" >> pure []

approvePendingSignup :: String -> String -> IO ()
approvePendingSignup cookie username = do
  req <- parseRequest "POST http://localhost:8081/api/v1/admin/pending-signups/approve"
  resp <- httpNoBody $ setRequestMethod "POST"
                   $ setRequestHeader "Cookie" [BS.pack cookie]
                   $ setRequestHeader "Content-Type" ["application/json"]
                   $ setRequestBodyJSON (object ["username" .= username]) req
  assertStatusCode "Pending signup approval should succeed" 200 resp

pendingSignupValue :: String -> Value
pendingSignupValue username = object ["username" .= username]

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
  let updatedItemId =
        case storageId updatedItem of
          StorageId { id = itemId } -> itemId
  deleteItem cookie endpoint updatedItemId
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
        withCookie = maybe Prelude.id (\cookie -> setRequestHeader "Cookie" [BS.pack cookie]) mCookie
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

data TripSharingSubscriptionsEndpoint = TripSharingSubscriptionsEndpoint
instance Endpoint TripSharingSubscriptionsEndpoint where
    getEndpoint TripSharingSubscriptionsEndpoint = "/api/v1/trip-sharing/subscriptions"

data TripSharingPeriodTripsEndpoint = TripSharingPeriodTripsEndpoint
instance Endpoint TripSharingPeriodTripsEndpoint where
    getEndpoint TripSharingPeriodTripsEndpoint = "/api/v1/trip-sharing/period-trips"

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

instance RequestType GET TripSharingSubscriptionsEndpoint () [Value] where
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

getSubscribedUsersList :: String -> IO [Value]
getSubscribedUsersList cookie = do
  getResponse :: Response [Value] <- sendRequestWithJSONBodyImplWithCookie (Just cookie) GET TripSharingSubscriptionsEndpoint ()
  assertStatusCode200 "Subscription list should succeed" getResponse
  pure (getResponseBody getResponse)

assertNoSharedUsers :: String -> IO ()
assertNoSharedUsers cookie = do
  sharedUsers <- getSharedUsersList cookie
  assertEqual "Expected no shared users" [] sharedUsers

assertNoSubscribedUsers :: String -> IO ()
assertNoSubscribedUsers cookie = do
  subscribedUsers <- getSubscribedUsersList cookie
  assertEqual "Expected no subscribed users" [] subscribedUsers

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
shareUserValue = tripSharingUserValue

tripSharingUserValue :: String -> Value
tripSharingUserValue username = object ["username" .= username]

addSubscribedUser :: String -> String -> IO ()
addSubscribedUser cookie username = do
  req <- parseRequest "POST http://localhost:8081/api/v1/trip-sharing/subscriptions"
  resp <- httpNoBody $ setRequestMethod "POST"
                   $ setRequestHeader "Cookie" [BS.pack cookie]
                   $ setRequestHeader "Content-Type" ["application/json"]
                   $ setRequestBodyJSON (object ["username" .= username]) req
  assertStatusCode "Subscription add should succeed" 200 resp

addSubscribedUserExpectMessage :: String -> String -> String -> IO ()
addSubscribedUserExpectMessage cookie username expectedMessage = do
  req <- parseRequest "POST http://localhost:8081/api/v1/trip-sharing/subscriptions"
  resp <- httpJSON $ setRequestMethod "POST"
                  $ setRequestHeader "Cookie" [BS.pack cookie]
                  $ setRequestHeader "Content-Type" ["application/json"]
                  $ setRequestBodyJSON (object ["username" .= username]) req
  assertStatusCode "Subscription add validation should return 400" 400 resp
  assertMessageResponse expectedMessage resp

deleteSubscribedUser :: String -> String -> IO ()
deleteSubscribedUser cookie username = do
  req <- parseRequest ("DELETE http://localhost:8081/api/v1/trip-sharing/subscriptions/" ++ username)
  resp <- httpNoBody $ setRequestMethod "DELETE"
                   $ setRequestHeader "Cookie" [BS.pack cookie] req
  assertStatusCode "Subscription delete should succeed" 200 resp

clearSubscribedUsers :: String -> [String] -> IO ()
clearSubscribedUsers cookie = mapM_ (deleteSubscribedUser cookie)

periodTripsRequest :: Maybe String -> Maybe String -> Maybe String -> IO Request
periodTripsRequest mCookie mStart mEnd = do
  req <- parseRequest ("GET http://localhost:8081" ++ getEndpoint TripSharingPeriodTripsEndpoint)
  let withCookie :: Request -> Request
      withCookie =
        case mCookie of
          Nothing -> Prelude.id
          Just cookie -> setRequestHeader "Cookie" [BS.pack cookie]
      query =
        maybe [] (\start -> [("start", Just (BS.pack start))]) mStart ++
        maybe [] (\end -> [("end", Just (BS.pack end))]) mEnd
  pure $ withCookie $ setRequestMethod "GET" $ setRequestQueryString query req

getPeriodTripsList :: String -> String -> String -> IO [Value]
getPeriodTripsList cookie start end = do
  req <- periodTripsRequest (Just cookie) (Just start) (Just end)
  resp <- httpJSON req
  assertStatusCode200 "Period trips request should succeed" (resp :: Response [Value])
  pure (getResponseBody resp)

assertPeriodTripsValidationError :: String -> Maybe String -> Maybe String -> String -> IO ()
assertPeriodTripsValidationError cookie mStart mEnd expectedMessage = do
  req <- periodTripsRequest (Just cookie) mStart mEnd
  resp <- httpJSON req
  assertStatusCode "Period trips validation should return 400" 400 (resp :: Response Value)
  assertMessageResponse expectedMessage resp

periodTripsUserValue :: String -> [Agenda.CalendarItem] -> Value
periodTripsUserValue username trips = object ["username" .= username, "trips" .= trips]

clearAgendaItems :: String -> IO ()
clearAgendaItems cookie = do
  items <- getAgendaItems cookie
  mapM_ deleteStoredItem items
  where
    deleteStoredItem item =
      case item of
        Agenda.ServerCalendarItem {} -> deleteAgendaItem cookie (Agenda.itemId item)
        Agenda.NewCalendarItem {} -> pure ()

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
assertTripCreateValidationError cookie content =
  assertAgendaPostValidationError cookie (Agenda.NewCalendarItem { Agenda.content = content })

assertTripUpdateValidationError :: String -> Agenda.CalendarItem -> String -> IO ()
assertTripUpdateValidationError = assertAgendaPostValidationError

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

assertSigninProfileResponse :: String -> [String] -> Bool -> Response Value -> Assertion
assertSigninProfileResponse expectedUsername expectedRoles expectedApproved response =
  case getResponseBody response of
    Object value -> do
      case parseMaybe (.: "username") value of
        Just actualUsername -> assertEqual "Expected signin profile username" expectedUsername (actualUsername :: String)
        Nothing -> assertFailure "Expected signin profile username"
      case parseMaybe (.: "roles") value of
        Just actualRoles -> assertEqual "Expected signin profile roles" expectedRoles (actualRoles :: [String])
        Nothing -> assertFailure "Expected signin profile roles"
      case parseMaybe (.: "approved") value of
        Just actualApproved -> assertEqual "Expected signin profile approval flag" expectedApproved (actualApproved :: Bool)
        Nothing -> assertFailure "Expected signin profile approval flag"
    _ -> assertFailure "Expected signin profile JSON object"

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

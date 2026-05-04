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
import qualified Data.Aeson.Key as Key
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
import           Test.Hspec.Runner (configRandomize, defaultConfig, hspecWith)
import           Test.HUnit
import           Control.Monad (when)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.List (isInfixOf, sort, sortOn)
import           Data.Char (toLower)
import           System.Environment (lookupEnv)
import AgendaModel (ItemStatus(..), ItemType(..))
import qualified AgendaModel as Agenda (CalendarItem(..), CalendarItemContent(..), TripItemContent(..))
import Model

-- ===================== Constants ==============================

noteEndpoint = "/note"
checklistEndpoint = "/checklist"
financeAccountsEndpoint = "/api/v1/finance/accounts"
financeTransactionsEndpoint = "/api/v1/finance/transactions"
financeTransactionsSentEndpoint = "/api/v1/finance/transactions/sent"
financeTransactionsReceivedEndpoint = "/api/v1/finance/transactions/received"

runIntegrationTests :: IO ()
runIntegrationTests = do
  let baseUsername = "admin"
      otherUsername = "integration-other-user"
      thirdUsername = "integration-third-user"
      basePassword = "averystrongpass" :: String
  _ <- ensureBootstrapAdminSession baseUsername basePassword
  expectedCookieSecure <- resolveCookieSecureExpectation
  hspecWith defaultConfig { configRandomize = False } $ do
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

      it "should return not found on stale note update and missing note delete" $ do
        cookie <- signinOnly baseUsername basePassword
        assertCrudEndpointConflictAndMissingDelete
          cookie
          NoteEndpoint
          (NoteContent { title = Just "stale note", noteContent = "initial content" })
          (NoteContent { title = Just "updated note", noteContent = "updated content" })
          200

      it "should return not found on stale checklist update and missing checklist delete" $ do
        cookie <- signinOnly baseUsername basePassword
        assertCrudEndpointConflictAndMissingDelete
          cookie
          ChecklistEndpoint
          (ChecklistContent { name = "stale checklist", items = [ChecklistItem { label = "item-1", checked = False }] })
          (ChecklistContent { name = "updated checklist", items = [ChecklistItem { label = "item-2", checked = True }] })
          200

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

      it "should return not found for unknown unauthenticated api paths" $ do
        req <- parseRequest "GET http://localhost:8081/api/does-not-exist"
        resp <- httpBS $ setRequestMethod "GET" req
        assertStatusCode "Unknown api route should return not found" 404 resp

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

      it "should return not found when approving an unknown pending signup" $ do
        adminCookie <- signinOnly baseUsername basePassword
        approveResponse <- approvePendingSignupResponse adminCookie "missing-pending-user"
        assertStatusCode "Approving an unknown pending signup should return not found" 404 approveResponse
        assertMessageResponse "Not found" approveResponse

      it "should allow an admin to delete pending signups" $ do
        uniquenessSuffix <- round . (* 1000000) <$> getPOSIXTime
        let pendingUsername = "deletable-" ++ show uniquenessSuffix
        ensurePendingSandboxUser baseUsername pendingUsername basePassword

        adminCookie <- signinOnly baseUsername basePassword
        pendingUsersBeforeDelete <- getPendingSignups adminCookie
        assertBool "Pending signup should be visible before delete" (pendingSignupValue pendingUsername `elem` pendingUsersBeforeDelete)

        deletePendingSignup adminCookie pendingUsername

        pendingUsersAfterDelete <- getPendingSignups adminCookie
        assertBool "Deleted pending signup should no longer be visible" (pendingSignupValue pendingUsername `notElem` pendingUsersAfterDelete)

        deletedSigninResponse <- performSigninJSON pendingUsername basePassword
        assertStatusCode "Deleted pending user should no longer be able to sign in" 401 deletedSigninResponse
        assertMessageResponse "Invalid credentials" deletedSigninResponse

      it "should return not found when deleting an unknown pending signup" $ do
        adminCookie <- signinOnly baseUsername basePassword
        deleteResponse <- deletePendingSignupResponse adminCookie "missing-pending-user"
        assertStatusCode "Deleting an unknown pending signup should return not found" 404 deleteResponse
        assertMessageResponse "Not found" deleteResponse

      it "should return not found when deleting an approved user through the pending-signup route" $ do
        uniquenessSuffix <- round . (* 1000000) <$> getPOSIXTime
        let approvedUsername = "approved-delete-" ++ show uniquenessSuffix
        ensureApprovedSandboxUser baseUsername approvedUsername basePassword

        adminCookie <- signinOnly baseUsername basePassword
        deleteResponse <- deletePendingSignupResponse adminCookie approvedUsername
        assertStatusCode "Deleting an approved user through the pending-signup route should return not found" 404 deleteResponse
        assertMessageResponse "Not found" deleteResponse

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

        deleteResp <- deletePendingSignupResponse memberCookie baseUsername
        assertStatusCode "Non-admin should be forbidden from deleting pending signups" 403 deleteResp
        assertMessageResponse "Admin privileges required" deleteResp

      it "should reject unauthenticated delete access to pending signup admin endpoints" $ do
        deleteReq <- parseRequest "DELETE http://localhost:8081/api/v1/admin/pending-signups/admin"
        deleteResp <- httpJSON $ setRequestMethod "DELETE" deleteReq
        assertStatusCode "Pending signup delete should require auth" 401 deleteResp
        assertMessageResponse "Not authenticated" deleteResp

      it "should allow an admin to list approved users" $ do
        uniquenessSuffix <- round . (* 1000000) <$> getPOSIXTime
        let approvedUsername = "list-app-" ++ show uniquenessSuffix
            pendingUsername = "list-pend-" ++ show uniquenessSuffix
        ensureApprovedSandboxUser baseUsername approvedUsername basePassword
        ensurePendingSandboxUser baseUsername pendingUsername basePassword

        adminCookie <- signinOnly baseUsername basePassword
        approvedUsers <- getAdminUsers adminCookie
        assertBool "Bootstrap admin should be visible in approved users" (adminUserValue baseUsername ["admin"] True `elem` approvedUsers)
        assertBool "Approved member should be visible in approved users" (adminUserValue approvedUsername ["member"] True `elem` approvedUsers)
        assertBool "Pending user should not be visible in approved users" (adminUserValue pendingUsername ["member"] True `notElem` approvedUsers)

      it "should allow an admin to delete another approved user" $ do
        uniquenessSuffix <- round . (* 1000000) <$> getPOSIXTime
        let approvedUsername = "del-app-" ++ show uniquenessSuffix
        ensureApprovedSandboxUser baseUsername approvedUsername basePassword

        adminCookie <- signinOnly baseUsername basePassword
        approvedUsersBeforeDelete <- getAdminUsers adminCookie
        assertBool "Approved user should be visible before delete" (adminUserValue approvedUsername ["member"] True `elem` approvedUsersBeforeDelete)

        deleteAdminUser adminCookie approvedUsername

        approvedUsersAfterDelete <- getAdminUsers adminCookie
        assertBool "Deleted approved user should no longer be visible" (adminUserValue approvedUsername ["member"] True `notElem` approvedUsersAfterDelete)

        deletedSigninResponse <- performSigninJSON approvedUsername basePassword
        assertStatusCode "Deleted approved user should no longer be able to sign in" 401 deletedSigninResponse
        assertMessageResponse "Invalid credentials" deletedSigninResponse

      it "should return not found when deleting an unknown approved user" $ do
        adminCookie <- signinOnly baseUsername basePassword
        deleteResponse <- deleteAdminUserResponse adminCookie "missing-approved-user"
        assertStatusCode "Deleting an unknown approved user should return not found" 404 deleteResponse
        assertMessageResponse "Not found" deleteResponse

      it "should return not found when deleting a pending user through the approved-user route" $ do
        uniquenessSuffix <- round . (* 1000000) <$> getPOSIXTime
        let pendingUsername = "pend-users-" ++ show uniquenessSuffix
        ensurePendingSandboxUser baseUsername pendingUsername basePassword

        adminCookie <- signinOnly baseUsername basePassword
        deleteResponse <- deleteAdminUserResponse adminCookie pendingUsername
        assertStatusCode "Deleting a pending user through the approved-user route should return not found" 404 deleteResponse
        assertMessageResponse "Not found" deleteResponse

      it "should reject non-admin access to approved-user admin endpoints" $ do
        uniquenessSuffix <- round . (* 1000000) <$> getPOSIXTime
        let memberUsername = "approved-member-" ++ show uniquenessSuffix
        ensureApprovedSandboxUser baseUsername memberUsername basePassword
        memberCookie <- signinOnly memberUsername basePassword

        usersReq <- parseRequest "GET http://localhost:8081/api/v1/admin/users"
        usersResp <- httpJSON $ setRequestMethod "GET"
                           $ setRequestHeader "Cookie" [BS.pack memberCookie]
                           usersReq
        assertStatusCode "Non-admin should be forbidden from listing approved users" 403 usersResp
        assertMessageResponse "Admin privileges required" usersResp

        deleteResp <- deleteAdminUserResponse memberCookie baseUsername
        assertStatusCode "Non-admin should be forbidden from deleting approved users" 403 deleteResp
        assertMessageResponse "Admin privileges required" deleteResp

      it "should reject unauthenticated approved-user admin access" $ do
        usersReq <- parseRequest "GET http://localhost:8081/api/v1/admin/users"
        usersResp <- httpJSON $ setRequestMethod "GET" usersReq
        assertStatusCode "Approved users list should require auth" 401 usersResp
        assertMessageResponse "Not authenticated" usersResp

        deleteReq <- parseRequest "DELETE http://localhost:8081/api/v1/admin/users/admin"
        deleteResp <- httpJSON $ setRequestMethod "DELETE" deleteReq
        assertStatusCode "Approved user delete should require auth" 401 deleteResp
        assertMessageResponse "Not authenticated" deleteResp

      it "should reject deleting the bootstrap admin through the approved-user route" $ do
        adminCookie <- signinOnly baseUsername basePassword
        deleteResponse <- deleteAdminUserResponse adminCookie baseUsername
        assertStatusCode "Deleting the bootstrap admin should return conflict" 409 deleteResponse
        assertMessageResponse "Cannot delete bootstrap admin" deleteResponse

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

      it "should require auth for agenda delete" $ do
        unauthReq <- parseRequest "DELETE http://localhost:8081/api/v1/calendar-items/missing-item"
        unauthResponse <- httpBS $ setRequestMethod "DELETE" unauthReq
        assertStatusCode "Agenda delete should require auth" 401 unauthResponse

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

      it "should require auth for finance account list and create endpoints" $ do
        listReq <- parseRequest "GET http://localhost:8081/api/v1/finance/accounts"
        listResp <- httpJSON $ setRequestMethod "GET" listReq
        assertStatusCode "Finance account list should require auth" 401 listResp
        assertMessageResponse "Not authenticated" listResp

        createReq <- parseRequest "POST http://localhost:8081/api/v1/finance/accounts"
        createResp <- httpJSON $ setRequestMethod "POST"
                                $ setRequestHeader "Content-Type" ["application/json"]
                                $ setRequestBodyJSON (object ["name" .= ("Wallet" :: String)]) createReq
        assertStatusCode "Finance account create should require auth" 401 createResp
        assertMessageResponse "Not authenticated" createResp

        closeReq <- parseRequest "POST http://localhost:8081/api/v1/finance/accounts/missing/close"
        closeResp <- httpJSON $ setRequestMethod "POST" closeReq
        assertStatusCode "Finance account close should require auth" 401 closeResp
        assertMessageResponse "Not authenticated" closeResp

        sentReq <- parseRequest "POST http://localhost:8081/api/v1/finance/transactions/sent"
        sentResp <- httpJSON $ setRequestMethod "POST"
                              $ setRequestHeader "Content-Type" ["application/json"]
                              $ setRequestBodyJSON (object ["accountId" .= ("missing" :: String), "amount" .= (100 :: Int)]) sentReq
        assertStatusCode "Finance sent transaction create should require auth" 401 sentResp
        assertMessageResponse "Not authenticated" sentResp

        receivedReq <- parseRequest "POST http://localhost:8081/api/v1/finance/transactions/received"
        receivedResp <- httpJSON $ setRequestMethod "POST"
                                  $ setRequestHeader "Content-Type" ["application/json"]
                                  $ setRequestBodyJSON (object ["accountId" .= ("missing" :: String), "amount" .= (100 :: Int)]) receivedReq
        assertStatusCode "Finance received transaction create should require auth" 401 receivedResp
        assertMessageResponse "Not authenticated" receivedResp

        listTransactionsReq <- parseRequest "GET http://localhost:8081/api/v1/finance/transactions"
        listTransactionsResp <- httpJSON $ setRequestMethod "GET" listTransactionsReq
        assertStatusCode "Finance transaction list should require auth" 401 listTransactionsResp
        assertMessageResponse "Not authenticated" listTransactionsResp

      it "should create and list finance accounts for the authenticated user" $ do
        cookie <- signinOnly baseUsername basePassword
        created <- createFinanceAccount cookie "  Cash Wallet  "
        assertFinanceAccountNameAndStatus "Cash Wallet" "active" created

        secondCreated <- createFinanceAccount cookie "Savings"
        assertFinanceAccountNameAndStatus "Savings" "active" secondCreated

        accounts <- getFinanceAccounts cookie Nothing
        assertEqual "Expected default finance account list to return active accounts only" ["Cash Wallet", "Savings"] (map financeAccountNameValue accounts)

        allAccounts <- getFinanceAccounts cookie (Just "all")
        assertEqual "Expected all finance account list to include both active accounts" ["Cash Wallet", "Savings"] (map financeAccountNameValue allAccounts)

      it "should reject blank, duplicate, and invalid finance account requests" $ do
        cookie <- signinOnly baseUsername basePassword
        blankResp <- createFinanceAccountExpectValue cookie "   "
        assertStatusCode "Blank finance account name should return 400" 400 blankResp
        assertMessageResponse "name must not be empty" blankResp

        _ <- createFinanceAccount cookie "Primary"
        duplicateResp <- createFinanceAccountExpectValue cookie " primary "
        assertStatusCode "Duplicate finance account name should return 409" 409 duplicateResp
        assertMessageResponse "Account already exists" duplicateResp

        invalidStatusResp <- getFinanceAccountsExpectValue cookie "archived"
        assertStatusCode "Invalid finance account status should return 400" 400 invalidStatusResp
        assertMessageResponse "status must be one of: active, closed, all" invalidStatusResp

      it "should close finance accounts idempotently and expose them through closed/all filters" $ do
        cookie <- signinOnly baseUsername basePassword
        created <- createFinanceAccount cookie "Archive Me"
        accountId <- requireObjectStringField "id" created

        firstClose <- closeFinanceAccount cookie accountId
        assertFinanceAccountNameAndStatus "Archive Me" "closed" firstClose

        secondClose <- closeFinanceAccount cookie accountId
        assertFinanceAccountNameAndStatus "Archive Me" "closed" secondClose

        activeAccounts <- getFinanceAccounts cookie Nothing
        assertBool "Expected closed account to disappear from the default active list" (all ((/= "Archive Me") . financeAccountNameValue) activeAccounts)

        closedAccounts <- getFinanceAccounts cookie (Just "closed")
        assertEqual "Expected closed filter to return the closed finance account" ["Archive Me"] (map financeAccountNameValue closedAccounts)

        allAccounts <- getFinanceAccounts cookie (Just "all")
        assertBool "Expected all filter to include the closed finance account" ("Archive Me" `elem` map financeAccountNameValue allAccounts)

      it "should reject closing unknown finance accounts" $ do
        cookie <- signinOnly baseUsername basePassword
        closeResp <- closeFinanceAccountExpectValue cookie "missing-account"
        assertStatusCode "Closing an unknown finance account should return 404" 404 closeResp
        assertMessageResponse "Account not found" closeResp

      it "should create sent and received finance transactions and enforce closed-account and idempotency rules" $ do
        cookie <- signinOnly baseUsername basePassword
        account <- createFinanceAccount cookie "Daily Checking"
        accountId <- requireObjectStringField "id" account

        sentResp <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "sent-key-1" (object ["accountId" .= accountId, "amount" .= (2500 :: Int), "occurredAt" .= ("2026-03-01T09:00:00Z" :: String)])
        assertStatusCode "Sent transaction create should succeed" 200 sentResp
        assertFinanceTransactionDirectionAndAmount "sent" 2500 (getResponseBody sentResp)

        sentRetryResp <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "sent-key-1" (object ["accountId" .= accountId, "amount" .= (2500 :: Int), "occurredAt" .= ("2026-03-01T09:00:00Z" :: String)])
        assertStatusCode "Idempotent sent transaction retry should succeed" 200 sentRetryResp
        assertEqual "Expected idempotent sent transaction retry to return the original row" (getResponseBody sentResp) (getResponseBody sentRetryResp)

        receivedResp <- createFinanceTransactionExpectValue cookie financeTransactionsReceivedEndpoint "received-key-1" (object ["accountId" .= accountId, "amount" .= (4200 :: Int)])
        assertStatusCode "Received transaction create should succeed" 200 receivedResp
        assertFinanceTransactionDirectionAndAmount "received" 4200 (getResponseBody receivedResp)

        missingKeyResp <- createFinanceTransactionWithoutIdempotencyHeader cookie financeTransactionsSentEndpoint (object ["accountId" .= accountId, "amount" .= (120 :: Int)])
        assertStatusCode "Missing idempotency key should return 400" 400 missingKeyResp
        assertMessageResponse "Idempotency-Key header is required" missingKeyResp

        invalidAmountResp <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "sent-key-invalid-amount" (object ["accountId" .= accountId, "amount" .= (0 :: Int)])
        assertStatusCode "Non-positive transaction amount should return 400" 400 invalidAmountResp
        assertMessageResponse "amount must be a positive integer" invalidAmountResp

        invalidOccurredAtResp <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "sent-key-invalid-occurred" (object ["accountId" .= accountId, "amount" .= (120 :: Int), "occurredAt" .= ("not-a-time" :: String)])
        assertStatusCode "Invalid occurredAt should return 400" 400 invalidOccurredAtResp
        assertMessageResponse "occurredAt must be a valid ISO date-time string" invalidOccurredAtResp

        malformedResp <- createFinanceTransactionMalformed cookie financeTransactionsSentEndpoint "sent-key-malformed"
        assertStatusCode "Malformed transaction payload should return 400" 400 malformedResp
        assertMessageResponse "Unable to decode the body as a FinanceTransactionCreateRequest" malformedResp

        unknownAccountResp <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "sent-key-missing-account" (object ["accountId" .= ("missing-account" :: String), "amount" .= (100 :: Int)])
        assertStatusCode "Unknown transaction account should return 404" 404 unknownAccountResp
        assertMessageResponse "Account not found" unknownAccountResp

        conflictingIdempotencyResp <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "sent-key-1" (object ["accountId" .= accountId, "amount" .= (2600 :: Int), "occurredAt" .= ("2026-03-01T09:00:00Z" :: String)])
        assertStatusCode "Reusing an idempotency key with a different request should return 409" 409 conflictingIdempotencyResp
        assertMessageResponse "Idempotency key already used for a different request" conflictingIdempotencyResp

        _ <- closeFinanceAccount cookie accountId
        closedSentResp <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "sent-key-closed" (object ["accountId" .= accountId, "amount" .= (200 :: Int)])
        assertStatusCode "Closed account sent transaction create should return 409" 409 closedSentResp
        assertMessageResponse "Closed accounts cannot accept new transactions" closedSentResp

        closedReceivedResp <- createFinanceTransactionExpectValue cookie financeTransactionsReceivedEndpoint "received-key-closed" (object ["accountId" .= accountId, "amount" .= (200 :: Int)])
        assertStatusCode "Closed account received transaction create should return 409" 409 closedReceivedResp
        assertMessageResponse "Closed accounts cannot accept new transactions" closedReceivedResp

      it "should list finance transactions with deterministic ordering and half-open filters" $ do
        uniquenessSuffix <- round . (* 1000000) <$> getPOSIXTime
        let ledgerUsername = "fin-ledger-" ++ show uniquenessSuffix
        ensureApprovedSandboxUser baseUsername ledgerUsername basePassword
        cookie <- signinOnly ledgerUsername basePassword
        firstAccount <- createFinanceAccount cookie "Ledger Primary"
        secondAccount <- createFinanceAccount cookie "Ledger Secondary"
        firstAccountId <- requireObjectStringField "id" firstAccount
        secondAccountId <- requireObjectStringField "id" secondAccount

        _ <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "ledger-key-1" (object ["accountId" .= firstAccountId, "amount" .= (101 :: Int), "occurredAt" .= ("2026-04-01T10:00:00Z" :: String)])
        _ <- createFinanceTransactionExpectValue cookie financeTransactionsReceivedEndpoint "ledger-key-2" (object ["accountId" .= secondAccountId, "amount" .= (202 :: Int), "occurredAt" .= ("2026-04-03T10:00:00Z" :: String)])
        _ <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "ledger-key-3" (object ["accountId" .= firstAccountId, "amount" .= (303 :: Int), "occurredAt" .= ("2026-04-02T10:00:00Z" :: String)])
        sameTimeAResp <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "ledger-key-4" (object ["accountId" .= firstAccountId, "amount" .= (404 :: Int), "occurredAt" .= ("2026-04-02T10:00:00Z" :: String)])
        sameTimeBResp <- createFinanceTransactionExpectValue cookie financeTransactionsReceivedEndpoint "ledger-key-5" (object ["accountId" .= firstAccountId, "amount" .= (505 :: Int), "occurredAt" .= ("2026-04-02T10:00:00Z" :: String)])

        allTransactions <- getFinanceTransactions cookie []
        assertEqual "Expected newest transaction amount first" 202 (financeTransactionAmountValue (head allTransactions))
        assertEqual "Expected oldest transaction amount last" 101 (financeTransactionAmountValue (last allTransactions))
        let sameTimestampTransactions = Prelude.take 3 (Prelude.drop 1 allTransactions)
            sameTimestampIds = map financeTransactionIdValue sameTimestampTransactions
            sameTimestampAmounts = map financeTransactionAmountValue sameTimestampTransactions
        assertEqual "Expected same-timestamp transaction ids to be ordered ascending as deterministic tie-breaker" (sort sameTimestampIds) sameTimestampIds
        assertEqual "Expected same-timestamp group to contain the expected amounts" [303, 404, 505] (sort sameTimestampAmounts)
        assertBool "Expected created same-timestamp transaction ids to appear in the sorted tie-break segment"
          (all (`elem` sameTimestampIds) [financeTransactionIdValue (getResponseBody sameTimeAResp), financeTransactionIdValue (getResponseBody sameTimeBResp)])

        firstAccountTransactions <- getFinanceTransactions cookie [("accountId", firstAccountId)]
        assertEqual "Expected accountId filter to return only matching account transactions" [101, 303, 404, 505] (sort (map financeTransactionAmountValue firstAccountTransactions))

        fromTransactions <- getFinanceTransactions cookie [("from", "2026-04-02T10:00:00Z")]
        assertEqual "Expected from filter to include the boundary timestamp" [202, 303, 404, 505] (sort (map financeTransactionAmountValue fromTransactions))

        toTransactions <- getFinanceTransactions cookie [("to", "2026-04-02T10:00:00Z")]
        assertEqual "Expected to filter to exclude the boundary timestamp" [101] (map financeTransactionAmountValue toTransactions)

        emptyBoundaryTransactions <- getFinanceTransactions cookie [("from", "2026-04-02T10:00:00Z"), ("to", "2026-04-02T10:00:00Z")]
        assertEqual "Expected equal from/to boundary to return an empty ledger page" [] emptyBoundaryTransactions

      it "should validate finance transaction list filters and treat unknown account filters as empty results" $ do
        uniquenessSuffix <- round . (* 1000000) <$> getPOSIXTime
        let ledgerValidationUsername = "fin-val-" ++ show uniquenessSuffix
        ensureApprovedSandboxUser baseUsername ledgerValidationUsername basePassword
        cookie <- signinOnly ledgerValidationUsername basePassword
        account <- createFinanceAccount cookie "Ledger Validation"
        accountId <- requireObjectStringField "id" account
        _ <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "ledger-validation-key" (object ["accountId" .= accountId, "amount" .= (123 :: Int), "occurredAt" .= ("2026-04-05T10:00:00Z" :: String)])

        unknownAccountTransactions <- getFinanceTransactions cookie [("accountId", "missing-account")]
        assertEqual "Expected unknown account filter to return an empty result set" [] unknownAccountTransactions

        invalidFromResp <- getFinanceTransactionsExpectValue cookie [("from", "not-a-time")]
        assertStatusCode "Invalid from timestamp should return 400" 400 invalidFromResp
        assertMessageResponse "from must be a valid ISO date-time string" invalidFromResp

        invalidToResp <- getFinanceTransactionsExpectValue cookie [("to", "not-a-time")]
        assertStatusCode "Invalid to timestamp should return 400" 400 invalidToResp
        assertMessageResponse "to must be a valid ISO date-time string" invalidToResp

        invertedRangeResp <- getFinanceTransactionsExpectValue cookie [("from", "2026-04-06T10:00:00Z"), ("to", "2026-04-05T10:00:00Z")]
        assertStatusCode "Inverted transaction list range should return 400" 400 invertedRangeResp
        assertMessageResponse "from must be less than or equal to to" invertedRangeResp

      it "should keep finance account lists isolated per authenticated user" $ do
        uniquenessSuffix <- round . (* 1000000) <$> getPOSIXTime
        let otherFinanceUsername = "finance-user-" ++ show uniquenessSuffix
        ensureApprovedSandboxUser baseUsername otherFinanceUsername basePassword

        baseCookie <- signinOnly baseUsername basePassword
        otherCookie <- signinOnly otherFinanceUsername basePassword

        _ <- createFinanceAccount baseCookie "Household"
        otherAccounts <- getFinanceAccounts otherCookie Nothing
        assertEqual "Expected other user finance account list to remain empty" [] otherAccounts

      it "should expose the fixed trip places catalog" $ do
        cookie <- signinOnly baseUsername basePassword
        places <- getTripPlaces cookie
        repeatedPlaces <- getTripPlaces cookie
        assertEqual "Trip places should match the fixed catalog" expectedTripPlaces places
        assertEqual "Trip places should stay stable across requests" expectedTripPlaces repeatedPlaces

      it "should support share-list add, list, and delete lifecycle" $ do
        ensureApprovedSandboxUser baseUsername otherUsername basePassword
        ensureApprovedSandboxUser baseUsername thirdUsername basePassword
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

      it "should return not found when deleting an unknown agenda item id" $ do
        cookie <- signinOnly baseUsername basePassword
        deleteAgendaItemExpectStatus cookie "missing-agenda-item" 404

      it "should enforce signup rate limiting" $ do
        uniquenessSuffix <- round . (* 1000000) <$> getPOSIXTime
        signupStatuses <- mapM
          (\i -> do
              let username = "ratelimit-" ++ show uniquenessSuffix ++ "-" ++ show i
              signupResponse <- performSignupNoBody username ("averystrongpass" :: String)
              pure (getResponseStatusCode signupResponse)
          )
          [1..8]
        assertBool "Expected at least one signup allowed before rate-limit saturation" (200 `elem` signupStatuses)
        assertBool "Expected signup rate-limiter to block at saturation" (400 `elem` signupStatuses)
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

ensureBootstrapAdminSession :: String -> String -> IO String
ensureBootstrapAdminSession username password = do
  signupResponse <- performSignupNoBody username password
  let statusCode = getResponseStatusCode signupResponse
  when (statusCode /= 200 && statusCode /= 400) $
    assertFailure ("Expected bootstrap signup to return 200 or 400, got " ++ show statusCode)
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
  signupResponse <- performSignupNoBody username password
  let signupStatus = getResponseStatusCode signupResponse
  when (signupStatus /= 200 && signupStatus /= 400) $
    assertFailure ("Expected sandbox user creation to return 200 or 400, got " ++ show signupStatus)
  adminCookie <- signinOnly bootstrapAdminUsername password
  approveResponse <- approvePendingSignupResponseNoBody adminCookie username
  let approveStatus = getResponseStatusCode approveResponse
  when (approveStatus /= 200 && approveStatus /= 404) $
    assertFailure ("Expected sandbox user approval to return 200 or 404, got " ++ show approveStatus)

ensurePendingSandboxUser :: String -> String -> String -> IO ()
ensurePendingSandboxUser bootstrapAdminUsername username password = do
  _ <- bootstrapAdminUsername `seq` pure ()
  signupResponse <- performSignupNoBody username password
  let statusCode = getResponseStatusCode signupResponse
  when (statusCode /= 200 && statusCode /= 400) $
    assertFailure ("Expected pending sandbox user creation to return 200 or 400, got " ++ show statusCode)

authPayload :: String -> String -> Value
authPayload username password =
  object [ "username" .= username
         , "password" .= password
         ]

performSigninNoBody :: String -> String -> IO (Response ())
performSigninNoBody = performSigninWith httpNoBody

performSignupNoBody :: String -> String -> IO (Response ())
performSignupNoBody username password = do
  signupReq <- parseRequest "POST http://localhost:8081/api/signup"
  httpNoBody $ setRequestMethod "POST"
             $ setRequestHeader "Content-Type" ["application/json"]
             $ setRequestBodyJSON (authPayload username password) signupReq

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

getAdminUsers :: String -> IO [Value]
getAdminUsers cookie = do
  req <- parseRequest "GET http://localhost:8081/api/v1/admin/users"
  resp <- httpJSON $ setRequestMethod "GET"
                  $ setRequestHeader "Cookie" [BS.pack cookie]
                  req
  assertStatusCode "Approved users list should succeed" 200 resp
  case getResponseBody resp of
    Array items -> pure (toList items)
    _ -> assertFailure "Expected approved users array" >> pure []

approvePendingSignup :: String -> String -> IO ()
approvePendingSignup cookie username = do
  resp <- approvePendingSignupResponseNoBody cookie username
  assertStatusCode "Pending signup approval should succeed" 200 resp

approvePendingSignupResponse :: String -> String -> IO (Response Value)
approvePendingSignupResponse cookie username = do
  req <- parseRequest "POST http://localhost:8081/api/v1/admin/pending-signups/approve"
  httpJSON $ setRequestMethod "POST"
           $ setRequestHeader "Cookie" [BS.pack cookie]
           $ setRequestHeader "Content-Type" ["application/json"]
           $ setRequestBodyJSON (object ["username" .= username]) req

approvePendingSignupResponseNoBody :: String -> String -> IO (Response ())
approvePendingSignupResponseNoBody cookie username = do
  req <- parseRequest "POST http://localhost:8081/api/v1/admin/pending-signups/approve"
  httpNoBody $ setRequestMethod "POST"
           $ setRequestHeader "Cookie" [BS.pack cookie]
           $ setRequestHeader "Content-Type" ["application/json"]
           $ setRequestBodyJSON (object ["username" .= username]) req

deletePendingSignup :: String -> String -> IO ()
deletePendingSignup cookie username = do
  resp <- deletePendingSignupResponse cookie username
  assertStatusCode "Pending signup delete should succeed" 200 resp

deletePendingSignupResponse :: String -> String -> IO (Response Value)
deletePendingSignupResponse cookie username = do
  req <- parseRequest ("DELETE http://localhost:8081/api/v1/admin/pending-signups/" ++ username)
  httpJSON $ setRequestMethod "DELETE"
           $ setRequestHeader "Cookie" [BS.pack cookie] req

deleteAdminUser :: String -> String -> IO ()
deleteAdminUser cookie username = do
  resp <- deleteAdminUserResponse cookie username
  assertStatusCode "Approved user delete should succeed" 200 resp

deleteAdminUserResponse :: String -> String -> IO (Response Value)
deleteAdminUserResponse cookie username = do
  req <- parseRequest ("DELETE http://localhost:8081/api/v1/admin/users/" ++ username)
  httpJSON $ setRequestMethod "DELETE"
           $ setRequestHeader "Cookie" [BS.pack cookie] req

pendingSignupValue :: String -> Value
pendingSignupValue username = object ["username" .= username]

adminUserValue :: String -> [String] -> Bool -> Value
adminUserValue username roles approved =
  object [ "username" .= username
         , "roles" .= roles
         , "approved" .= approved
         ]

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

assertCrudEndpointConflictAndMissingDelete
  :: ( Content contentType
     , Endpoint endpointType
     , RequestType GET endpointType () [Identifiable contentType]
     , RequestType POST endpointType contentType StorageId
     , RequestType PUT endpointType (Identifiable contentType) StorageId
     )
  => String
  -> endpointType
  -> contentType
  -> contentType
  -> Int
  -> Expectation
assertCrudEndpointConflictAndMissingDelete cookie endpoint initialContent updatedContent expectedDeleteStatus = do
  assertNoItemAtEndpoint cookie endpoint
  createNewContent cookie endpoint initialContent
  [createdItem] <- assertGetWithContent cookie endpoint initialContent
  let staleStorageId =
        case storageId createdItem of
          StorageId { id = itemId, version = itemVersion } ->
            StorageId { id = itemId, version = itemVersion ++ "-stale" }
      staleUpdate = modifyNote updatedContent createdItem { storageId = staleStorageId }
      missingId =
        case storageId createdItem of
          StorageId { id = itemId } -> itemId ++ "-missing"
  updateItemExpectMessage cookie endpoint staleUpdate 404 "Unable to find storage dir"
  deleteItemExpectEmptyResponse cookie endpoint missingId expectedDeleteStatus

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

deleteItemExpectEmptyResponse :: Endpoint a => String -> a -> String -> Int -> Expectation
deleteItemExpectEmptyResponse cookie endpoint idToDelete expectedStatus = do
  req <- parseRequest ("DELETE http://localhost:8081" ++ getEndpoint endpoint ++ "/" ++ idToDelete)
  resp <- httpJSON $ setRequestMethod "DELETE"
                  $ setRequestHeader "Cookie" [BS.pack cookie] req
  assertStatusCode "Delete should return expected status" expectedStatus (resp :: Response Value)
  assertEqual "Expected empty JSON response body" (object []) (getResponseBody resp)

updateItemExpectMessage
  :: ( Endpoint endpointType
     , ToJSON contentType
     )
  => String
  -> endpointType
  -> contentType
  -> Int
  -> String
  -> Expectation
updateItemExpectMessage cookie endpoint updateContent expectedStatus expectedMessage = do
  req <- parseRequest ("PUT http://localhost:8081" ++ getEndpoint endpoint)
  resp <- httpJSON $ setRequestMethod "PUT"
                  $ setRequestHeader "Cookie" [BS.pack cookie]
                  $ setRequestHeader "Content-Type" ["application/json"]
                  $ setRequestBodyJSON updateContent req
  assertStatusCode "Update should return expected status" expectedStatus (resp :: Response Value)
  assertMessageResponse expectedMessage resp

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

createFinanceAccount :: String -> String -> IO Value
createFinanceAccount cookie name = do
  resp <- createFinanceAccountExpectValue cookie name
  assertStatusCode "Finance account create should succeed" 200 resp
  pure (getResponseBody resp)

createFinanceAccountExpectValue :: String -> String -> IO (Response Value)
createFinanceAccountExpectValue cookie name = do
  req <- parseRequest ("POST http://localhost:8081" ++ financeAccountsEndpoint)
  httpJSON $ setRequestMethod "POST"
         $ setRequestHeader "Cookie" [BS.pack cookie]
         $ setRequestHeader "Content-Type" ["application/json"]
         $ setRequestBodyJSON (object ["name" .= name]) req

getFinanceAccounts :: String -> Maybe String -> IO [Value]
getFinanceAccounts cookie mStatus = do
  req <- financeAccountsRequest cookie mStatus
  resp <- httpJSON req
  assertStatusCode "Finance account list should succeed" 200 (resp :: Response [Value])
  pure (getResponseBody resp)

getFinanceAccountsExpectValue :: String -> String -> IO (Response Value)
getFinanceAccountsExpectValue cookie statusValue = do
  req <- financeAccountsRequest cookie (Just statusValue)
  httpJSON req

financeAccountsRequest :: String -> Maybe String -> IO Request
financeAccountsRequest cookie mStatus = do
  req <- parseRequest ("GET http://localhost:8081" ++ financeAccountsEndpoint)
  let query = maybe [] (\statusValue -> [("status", Just (BS.pack statusValue))]) mStatus
  pure $ setRequestHeader "Cookie" [BS.pack cookie] $ setRequestMethod "GET" $ setRequestQueryString query req

closeFinanceAccount :: String -> String -> IO Value
closeFinanceAccount cookie accountId = do
  resp <- closeFinanceAccountExpectValue cookie accountId
  assertStatusCode "Finance account close should succeed" 200 resp
  pure (getResponseBody resp)

closeFinanceAccountExpectValue :: String -> String -> IO (Response Value)
closeFinanceAccountExpectValue cookie accountId = do
  req <- parseRequest ("POST http://localhost:8081" ++ financeAccountsEndpoint ++ "/" ++ accountId ++ "/close")
  httpJSON $ setRequestMethod "POST"
         $ setRequestHeader "Cookie" [BS.pack cookie]
         req

createFinanceTransactionExpectValue :: String -> String -> String -> Value -> IO (Response Value)
createFinanceTransactionExpectValue cookie endpoint idempotencyKey body = do
  req <- parseRequest ("POST http://localhost:8081" ++ endpoint)
  httpJSON $ setRequestMethod "POST"
         $ setRequestHeader "Cookie" [BS.pack cookie]
         $ setRequestHeader "Idempotency-Key" [BS.pack idempotencyKey]
         $ setRequestHeader "Content-Type" ["application/json"]
         $ setRequestBodyJSON body req

createFinanceTransactionWithoutIdempotencyHeader :: String -> String -> Value -> IO (Response Value)
createFinanceTransactionWithoutIdempotencyHeader cookie endpoint body = do
  req <- parseRequest ("POST http://localhost:8081" ++ endpoint)
  httpJSON $ setRequestMethod "POST"
         $ setRequestHeader "Cookie" [BS.pack cookie]
         $ setRequestHeader "Content-Type" ["application/json"]
         $ setRequestBodyJSON body req

createFinanceTransactionMalformed :: String -> String -> String -> IO (Response Value)
createFinanceTransactionMalformed cookie endpoint idempotencyKey = do
  req <- parseRequest ("POST http://localhost:8081" ++ endpoint)
  httpJSON $ setRequestMethod "POST"
         $ setRequestHeader "Cookie" [BS.pack cookie]
         $ setRequestHeader "Idempotency-Key" [BS.pack idempotencyKey]
         $ setRequestHeader "Content-Type" ["application/json"]
         $ setRequestBodyJSON (object []) req

getFinanceTransactions :: String -> [(String, String)] -> IO [Value]
getFinanceTransactions cookie queryParams = do
  req <- financeTransactionsRequest cookie queryParams
  resp <- httpJSON req
  assertStatusCode "Finance transaction list should succeed" 200 (resp :: Response [Value])
  pure (getResponseBody resp)

getFinanceTransactionsExpectValue :: String -> [(String, String)] -> IO (Response Value)
getFinanceTransactionsExpectValue cookie queryParams = do
  req <- financeTransactionsRequest cookie queryParams
  httpJSON req

financeTransactionsRequest :: String -> [(String, String)] -> IO Request
financeTransactionsRequest cookie queryParams = do
  req <- parseRequest ("GET http://localhost:8081" ++ financeTransactionsEndpoint)
  let encodedQuery = map (\(k, v) -> (BS.pack k, Just (BS.pack v))) queryParams
  pure $ setRequestHeader "Cookie" [BS.pack cookie] $ setRequestMethod "GET" $ setRequestQueryString encodedQuery req

assertFinanceAccountNameAndStatus :: String -> String -> Value -> Assertion
assertFinanceAccountNameAndStatus expectedName expectedStatus responseBody =
  case responseBody of
    Object value -> do
      case parseMaybe (.: "name") value of
        Just actualName -> assertEqual "Expected finance account name" expectedName (actualName :: String)
        Nothing -> assertFailure "Expected finance account name"
      case parseMaybe (.: "status") value of
        Just actualStatus -> assertEqual "Expected finance account status" expectedStatus (actualStatus :: String)
        Nothing -> assertFailure "Expected finance account status"
      case parseMaybe (.: "id") value of
        Just actualId -> assertBool "Expected finance account id to be non-empty" (not (null (actualId :: String)))
        Nothing -> assertFailure "Expected finance account id"
    _ -> assertFailure "Expected finance account response object"

assertFinanceTransactionDirectionAndAmount :: String -> Int -> Value -> Assertion
assertFinanceTransactionDirectionAndAmount expectedDirection expectedAmount responseBody =
  case responseBody of
    Object value -> do
      case parseMaybe (.: "direction") value of
        Just actualDirection -> assertEqual "Expected finance transaction direction" expectedDirection (actualDirection :: String)
        Nothing -> assertFailure "Expected finance transaction direction"
      case parseMaybe (.: "amount") value of
        Just actualAmount -> assertEqual "Expected finance transaction amount" expectedAmount (actualAmount :: Int)
        Nothing -> assertFailure "Expected finance transaction amount"
      case parseMaybe (.: "accountId") value of
        Just actualAccountId -> assertBool "Expected finance transaction account id to be non-empty" (not (null (actualAccountId :: String)))
        Nothing -> assertFailure "Expected finance transaction account id"
      case parseMaybe (.: "id") value of
        Just actualId -> assertBool "Expected finance transaction id to be non-empty" (not (null (actualId :: String)))
        Nothing -> assertFailure "Expected finance transaction id"
    _ -> assertFailure "Expected finance transaction response object"

requireObjectStringField :: String -> Value -> IO String
requireObjectStringField fieldName responseBody =
  case responseBody of
    Object value ->
      case parseMaybe (.: Key.fromString fieldName) value of
        Just actualValue -> pure actualValue
        Nothing -> assertFailure ("Expected field " ++ fieldName) >> pure ""
    _ -> assertFailure "Expected response object" >> pure ""

financeAccountNameValue :: Value -> String
financeAccountNameValue responseBody =
  case responseBody of
    Object value ->
      case parseMaybe (.: "name") value of
        Just actualName -> actualName
        Nothing -> error "Expected finance account name field"
    _ -> error "Expected finance account response object"

financeTransactionAmountValue :: Value -> Int
financeTransactionAmountValue responseBody =
  case responseBody of
    Object value ->
      case parseMaybe (.: "amount") value of
        Just actualAmount -> actualAmount
        Nothing -> error "Expected finance transaction amount field"
    _ -> error "Expected finance transaction response object"

financeTransactionIdValue :: Value -> String
financeTransactionIdValue responseBody =
  case responseBody of
    Object value ->
      case parseMaybe (.: "id") value of
        Just actualId -> actualId
        Nothing -> error "Expected finance transaction id field"
    _ -> error "Expected finance transaction response object"

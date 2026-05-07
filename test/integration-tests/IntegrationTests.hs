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
import           Control.Concurrent (threadDelay)
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
financeCategoriesEndpoint = "/api/v1/finance/categories"
financeTransactionsEndpoint = "/api/v1/finance/transactions"
financeTransactionsSentEndpoint = "/api/v1/finance/transactions/sent"
financeTransactionsReceivedEndpoint = "/api/v1/finance/transactions/received"
financeReportEndpoint = "/api/v1/finance/report"
financeExportEndpoint = "/api/v1/finance/export"

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

        snapshotsCreateReq <- parseRequest "POST http://localhost:8081/api/v1/finance/accounts/missing/snapshots"
        snapshotsCreateResp <- httpJSON $ setRequestMethod "POST"
                                       $ setRequestHeader "Content-Type" ["application/json"]
                                       $ setRequestBodyJSON (object ["balance" .= (1000 :: Int), "occurredAt" .= ("2026-04-01T10:00:00Z" :: String)]) snapshotsCreateReq
        assertStatusCode "Finance account snapshot create should require auth" 401 snapshotsCreateResp
        assertMessageResponse "Not authenticated" snapshotsCreateResp

        snapshotsListReq <- parseRequest "GET http://localhost:8081/api/v1/finance/accounts/missing/snapshots"
        snapshotsListResp <- httpJSON $ setRequestMethod "GET" snapshotsListReq
        assertStatusCode "Finance account snapshot list should require auth" 401 snapshotsListResp
        assertMessageResponse "Not authenticated" snapshotsListResp

        setSnapshotStatusReq <- parseRequest "PUT http://localhost:8081/api/v1/finance/accounts/missing/snapshots/missing/reconciliation-status"
        setSnapshotStatusResp <- httpJSON $ setRequestMethod "PUT"
                                         $ setRequestHeader "Content-Type" ["application/json"]
                                         $ setRequestBodyJSON (object ["status" .= ("reconciled" :: String)]) setSnapshotStatusReq
        assertStatusCode "Finance account snapshot status update should require auth" 401 setSnapshotStatusResp
        assertMessageResponse "Not authenticated" setSnapshotStatusResp

        reconciliationReq <- parseRequest "GET http://localhost:8081/api/v1/finance/accounts/missing/reconciliation"
        reconciliationResp <- httpJSON $ setRequestMethod "GET" reconciliationReq
        assertStatusCode "Finance account reconciliation should require auth" 401 reconciliationResp
        assertMessageResponse "Not authenticated" reconciliationResp

        listCategoriesReq <- parseRequest "GET http://localhost:8081/api/v1/finance/categories"
        listCategoriesResp <- httpJSON $ setRequestMethod "GET" listCategoriesReq
        assertStatusCode "Finance category list should require auth" 401 listCategoriesResp
        assertMessageResponse "Not authenticated" listCategoriesResp

        createCategoryReq <- parseRequest "POST http://localhost:8081/api/v1/finance/categories"
        createCategoryResp <- httpJSON $ setRequestMethod "POST"
                                        $ setRequestHeader "Content-Type" ["application/json"]
                                        $ setRequestBodyJSON (object ["name" .= ("Wallet" :: String)]) createCategoryReq
        assertStatusCode "Finance category create should require auth" 401 createCategoryResp
        assertMessageResponse "Not authenticated" createCategoryResp

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

        reportReq <- parseRequest "GET http://localhost:8081/api/v1/finance/report?from=2026-04-01T00:00:00Z&to=2026-04-30T00:00:00Z"
        reportResp <- httpJSON $ setRequestMethod "GET" reportReq
        assertStatusCode "Finance report should require auth" 401 reportResp
        assertMessageResponse "Not authenticated" reportResp

        exportReq <- parseRequest "GET http://localhost:8081/api/v1/finance/export"
        exportResp <- httpJSON $ setRequestMethod "GET" exportReq
        assertStatusCode "Finance export should require auth" 401 exportResp
        assertMessageResponse "Not authenticated" exportResp

        categorizeReq <- parseRequest "POST http://localhost:8081/api/v1/finance/transactions/missing/categorize"
        categorizeResp <- httpJSON $ setRequestMethod "POST"
                                    $ setRequestHeader "Content-Type" ["application/json"]
                                    $ setRequestBodyJSON (object ["category" .= ("pets.food" :: String)]) categorizeReq
        assertStatusCode "Finance transaction categorize should require auth" 401 categorizeResp
        assertMessageResponse "Not authenticated" categorizeResp

        splitReq <- parseRequest "POST http://localhost:8081/api/v1/finance/transactions/missing/split"
        splitResp <- httpJSON $ setRequestMethod "POST"
                              $ setRequestHeader "Content-Type" ["application/json"]
                              $ setRequestBodyJSON (object ["splits" .= [object ["amount" .= (10 :: Int), "category" .= ("pets.food" :: String)], object ["amount" .= (10 :: Int), "category" .= ("personal.clothing" :: String)]]]) splitReq
        assertStatusCode "Finance transaction split should require auth" 401 splitResp
        assertMessageResponse "Not authenticated" splitResp

        linkReq <- parseRequest "POST http://localhost:8081/api/v1/finance/transactions/link"
        linkResp <- httpJSON $ setRequestMethod "POST"
                             $ setRequestHeader "Content-Type" ["application/json"]
                             $ setRequestBodyJSON (object
                               [ "sourceTransactionId" .= ("source" :: String)
                               , "targetTransactionId" .= ("target" :: String)
                               , "linkType" .= ("transfer" :: String)
                               ]) linkReq
        assertStatusCode "Finance transaction link should require auth" 401 linkResp
        assertMessageResponse "Not authenticated" linkResp

        notesReq <- parseRequest "POST http://localhost:8081/api/v1/finance/transactions/missing/notes"
        notesResp <- httpJSON $ setRequestMethod "POST"
                               $ setRequestHeader "Content-Type" ["application/json"]
                               $ setRequestBodyJSON (object ["text" .= ("hello" :: String)]) notesReq
        assertStatusCode "Finance transaction note append should require auth" 401 notesResp
        assertMessageResponse "Not authenticated" notesResp

        notesUpdateReq <- parseRequest "PUT http://localhost:8081/api/v1/finance/transactions/missing/notes/missing"
        notesUpdateResp <- httpJSON $ setRequestMethod "PUT"
                                     $ setRequestHeader "Content-Type" ["application/json"]
                                     $ setRequestBodyJSON (object ["text" .= ("hello" :: String)]) notesUpdateReq
        assertStatusCode "Finance transaction note update should require auth" 401 notesUpdateResp
        assertMessageResponse "Not authenticated" notesUpdateResp

        notesDeleteReq <- parseRequest "DELETE http://localhost:8081/api/v1/finance/transactions/missing/notes/missing"
        notesDeleteResp <- httpJSON $ setRequestMethod "DELETE" notesDeleteReq
        assertStatusCode "Finance transaction note delete should require auth" 401 notesDeleteResp
        assertMessageResponse "Not authenticated" notesDeleteResp

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

      it "should record account snapshots and compute reconciliation views" $ do
        uniquenessSuffix <- round . (* 1000000) <$> getPOSIXTime
        let snapshotUsername = "fin-snap-" ++ show uniquenessSuffix
        ensureApprovedSandboxUser baseUsername snapshotUsername basePassword
        cookie <- signinOnly snapshotUsername basePassword
        account <- createFinanceAccount cookie "Snapshot Account"
        accountId <- requireObjectStringField "id" account

        _ <- createFinanceTransactionExpectValue cookie financeTransactionsReceivedEndpoint "snapshot-key-1" (object ["accountId" .= accountId, "amount" .= (5000 :: Int), "occurredAt" .= ("2026-04-01T10:00:00Z" :: String)])
        _ <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "snapshot-key-2" (object ["accountId" .= accountId, "amount" .= (1200 :: Int), "occurredAt" .= ("2026-04-02T10:00:00Z" :: String)])

        snapshotCreateResp <- createFinanceAccountSnapshotExpectValue cookie accountId 3500 "2026-04-02T10:00:00Z"
        assertStatusCode "Snapshot create should succeed" 200 snapshotCreateResp
        assertFinanceReconciliationValues 3800 (-300) (getResponseBody snapshotCreateResp)
        assertEqual "Expected snapshot create to default to unreconciled status" "unreconciled" (financeReconciliationStatusValue (getResponseBody snapshotCreateResp))
        assertEqual "Expected snapshot create to have no reconciliation basis" Nothing (financeReconciliationBasisSnapshotIdValue (getResponseBody snapshotCreateResp))
        firstSnapshotId <- requireObjectStringField "snapshotId" (getResponseBody snapshotCreateResp)

        reconciledFirstResp <- setFinanceAccountSnapshotReconciliationStatusExpectValue cookie accountId firstSnapshotId "reconciled"
        assertStatusCode "Marking a snapshot reconciled should succeed" 200 reconciledFirstResp
        assertEqual "Expected reconciled snapshot status to update" "reconciled" (financeReconciliationStatusValue (getResponseBody reconciledFirstResp))
        assertEqual "Expected reconciled snapshot to use itself as basis" (Just firstSnapshotId) (financeReconciliationBasisSnapshotIdValue (getResponseBody reconciledFirstResp))
        assertEqual "Expected reconciled snapshot basis timestamp to match the snapshot" (Just "2026-04-02T10:00:00Z") (financeReconciliationBasisSnapshotOccurredAtValue (getResponseBody reconciledFirstResp))
        assertFinanceReconciliationValues 3500 0 (getResponseBody reconciledFirstResp)

        duplicateSnapshotResp <- createFinanceAccountSnapshotExpectValue cookie accountId 3550 "2026-04-02T10:00:00Z"
        assertStatusCode "Duplicate account+timestamp snapshot should return 409" 409 duplicateSnapshotResp
        assertMessageResponse "Snapshot already exists for that account and timestamp" duplicateSnapshotResp

        secondSnapshotResp <- createFinanceAccountSnapshotExpectValue cookie accountId 3000 "2026-04-03T10:00:00Z"
        assertStatusCode "Second snapshot create should succeed" 200 secondSnapshotResp
        secondSnapshotId <- requireObjectStringField "snapshotId" (getResponseBody secondSnapshotResp)
        assertEqual "Expected second snapshot create to default to unreconciled status" "unreconciled" (financeReconciliationStatusValue (getResponseBody secondSnapshotResp))
        assertEqual "Expected second snapshot create to use the first reconciled snapshot as basis" (Just firstSnapshotId) (financeReconciliationBasisSnapshotIdValue (getResponseBody secondSnapshotResp))
        assertEqual "Expected second snapshot basis timestamp to come from the first snapshot" (Just "2026-04-02T10:00:00Z") (financeReconciliationBasisSnapshotOccurredAtValue (getResponseBody secondSnapshotResp))
        assertFinanceReconciliationValues 3500 (-500) (getResponseBody secondSnapshotResp)

        _ <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "snapshot-key-3" (object ["accountId" .= accountId, "amount" .= (500 :: Int), "occurredAt" .= ("2026-04-04T10:00:00Z" :: String)])

        snapshots <- getFinanceAccountSnapshots cookie accountId
        assertEqual "Expected two snapshots listed" 2 (Prelude.length snapshots)
        assertEqual "Expected latest snapshot first in list order" secondSnapshotId (financeSnapshotIdValue (head snapshots))
        assertEqual "Expected latest snapshot to be unreconciled in the list view" "unreconciled" (financeSnapshotReconciliationStatusValue (head snapshots))
        assertEqual "Expected older snapshot to be reconciled in the list view" "reconciled" (financeSnapshotReconciliationStatusValue (snapshots !! 1))

        latestReconciliationResp <- getFinanceAccountReconciliationExpectValue cookie accountId Nothing
        assertStatusCode "Latest reconciliation should succeed" 200 latestReconciliationResp
        assertEqual "Expected latest reconciliation to use latest snapshot id" secondSnapshotId (financeReconciliationSnapshotIdValue (getResponseBody latestReconciliationResp))
        assertEqual "Expected latest reconciliation to report the first snapshot as the basis" (Just firstSnapshotId) (financeReconciliationBasisSnapshotIdValue (getResponseBody latestReconciliationResp))
        assertEqual "Expected latest reconciliation basis timestamp to match the first snapshot" (Just "2026-04-02T10:00:00Z") (financeReconciliationBasisSnapshotOccurredAtValue (getResponseBody latestReconciliationResp))
        assertFinanceReconciliationValues 3500 (-500) (getResponseBody latestReconciliationResp)

        firstReconciliationResp <- getFinanceAccountReconciliationExpectValue cookie accountId (Just firstSnapshotId)
        assertStatusCode "Reconciliation by snapshot id should succeed" 200 firstReconciliationResp
        assertFinanceReconciliationValues 3500 0 (getResponseBody firstReconciliationResp)

        unreconciledFirstResp <- setFinanceAccountSnapshotReconciliationStatusExpectValue cookie accountId firstSnapshotId "unreconciled"
        assertStatusCode "Unmarking a snapshot reconciled should succeed" 200 unreconciledFirstResp
        assertEqual "Expected snapshot to return to unreconciled status" "unreconciled" (financeReconciliationStatusValue (getResponseBody unreconciledFirstResp))
        assertEqual "Expected unreconciled snapshot to lose its basis" Nothing (financeReconciliationBasisSnapshotIdValue (getResponseBody unreconciledFirstResp))
        assertFinanceReconciliationValues 3800 (-300) (getResponseBody unreconciledFirstResp)

        fallbackSecondResp <- getFinanceAccountReconciliationExpectValue cookie accountId (Just secondSnapshotId)
        assertStatusCode "Reconciliation by snapshot id should still succeed after basis removal" 200 fallbackSecondResp
        assertEqual "Expected second snapshot to fall back to transaction-derived baseline after unmarking the basis" Nothing (financeReconciliationBasisSnapshotIdValue (getResponseBody fallbackSecondResp))
        assertFinanceReconciliationValues 3800 (-800) (getResponseBody fallbackSecondResp)

        missingSnapshotResp <- getFinanceAccountReconciliationExpectValue cookie accountId (Just "missing-snapshot")
        assertStatusCode "Unknown snapshot reconciliation should return 404" 404 missingSnapshotResp
        assertMessageResponse "Account or snapshot not found" missingSnapshotResp

        missingAccountSnapshotResp <- createFinanceAccountSnapshotExpectValue cookie "missing-account" 1000 "2026-04-02T10:00:00Z"
        assertStatusCode "Snapshot create on unknown account should return 404" 404 missingAccountSnapshotResp
        assertMessageResponse "Account not found" missingAccountSnapshotResp

        _ <- closeFinanceAccount cookie accountId
        closedSnapshotResp <- createFinanceAccountSnapshotExpectValue cookie accountId 2500 "2026-04-05T10:00:00Z"
        assertStatusCode "Snapshot create on a closed account should succeed" 200 closedSnapshotResp

      it "should list built-in finance categories and support user-owned category lifecycle" $ do
        uniquenessSuffix <- round . (* 1000000) <$> getPOSIXTime
        let categoryUsername = "fin-cat-" ++ show uniquenessSuffix
        ensureApprovedSandboxUser baseUsername categoryUsername basePassword
        cookie <- signinOnly categoryUsername basePassword

        initialCategories <- getFinanceCategories cookie
        assertBool "Expected built-in income root category to exist" (any (\category -> financeCategoryIdValue category == "income" && not (financeCategorySelectableValue category) && financeCategoryOwnerValue category == "built_in") initialCategories)
        assertBool "Expected built-in income salary category to exist" (any (\category -> financeCategoryIdValue category == "income.salary" && financeCategorySelectableValue category && financeCategoryOwnerValue category == "built_in") initialCategories)

        customRoot <- createFinanceCategory cookie "Custom Root" Nothing
        customRootId <- requireObjectStringField "id" customRoot
        underBuiltIn <- createFinanceCategory cookie "Under Built In" (Just "transport")
        underBuiltInId <- requireObjectStringField "id" underBuiltIn
        nested <- createFinanceCategory cookie "Nested Child" (Just customRootId)
        nestedId <- requireObjectStringField "id" nested

        categoriesAfterCreate <- getFinanceCategories cookie
        assertBool "Expected custom root category to appear in list" (any ((== customRootId) . financeCategoryIdValue) categoriesAfterCreate)
        assertBool "Expected built-in child category to appear in list" (any ((== underBuiltInId) . financeCategoryIdValue) categoriesAfterCreate)
        assertBool "Expected nested child category to appear in list" (any ((== nestedId) . financeCategoryIdValue) categoriesAfterCreate)

        updatedNested <- updateFinanceCategory cookie nestedId "Nested Child Updated" (Just "personal")
        assertEqual "Expected updated category name" "Nested Child Updated" (financeCategoryNameValue updatedNested)
        assertEqual "Expected updated category parent" (Just "personal") (financeCategoryParentIdValue updatedNested)

        builtInUpdateResp <- updateFinanceCategoryExpectValue cookie "income" "Nope" Nothing
        assertStatusCode "Updating a built-in category should return 409" 409 builtInUpdateResp
        assertMessageResponse "Built-in categories are read-only" builtInUpdateResp

        builtInDeleteResp <- deleteFinanceCategoryExpectValue cookie "income"
        assertStatusCode "Deleting a built-in category should return 409" 409 builtInDeleteResp
        assertMessageResponse "Category cannot be deleted" builtInDeleteResp

        deleteFinanceCategory cookie underBuiltInId
        deleteFinanceCategory cookie nestedId
        deleteFinanceCategory cookie customRootId
        categoriesAfterDelete <- getFinanceCategories cookie
        assertBool "Expected deleted custom root to disappear from list" (all ((/= customRootId) . financeCategoryIdValue) categoriesAfterDelete)
        assertBool "Expected deleted nested category to disappear from list" (all ((/= nestedId) . financeCategoryIdValue) categoriesAfterDelete)

      it "should reject invalid finance category parent choices and cross-user references" $ do
        uniquenessSuffix <- round . (* 1000000) <$> getPOSIXTime
        let ownerUsername = "fin-cat-owner-" ++ show uniquenessSuffix
            otherUsernameLocal = "fin-cat-other-" ++ show uniquenessSuffix
        ensureApprovedSandboxUser baseUsername ownerUsername basePassword
        ensureApprovedSandboxUser baseUsername otherUsernameLocal basePassword
        ownerCookie <- signinOnly ownerUsername basePassword
        otherCookie <- signinOnly otherUsernameLocal basePassword

        foreignRoot <- createFinanceCategory otherCookie "Foreign Root" Nothing
        foreignRootId <- requireObjectStringField "id" foreignRoot
        ownerRoot <- createFinanceCategory ownerCookie "Owner Root" Nothing
        ownerRootId <- requireObjectStringField "id" ownerRoot
        ownerChild <- createFinanceCategory ownerCookie "Owner Child" (Just ownerRootId)
        ownerChildId <- requireObjectStringField "id" ownerChild

        invalidParentResp <- createFinanceCategoryExpectValue ownerCookie "Bad Parent" (Just "missing-parent")
        assertStatusCode "Unknown category parent should return 400" 400 invalidParentResp
        assertMessageResponse "parentId must reference an accessible category and must not create a cycle" invalidParentResp

        foreignParentResp <- createFinanceCategoryExpectValue ownerCookie "Foreign Parent Link" (Just foreignRootId)
        assertStatusCode "Cross-user category parent should return 400" 400 foreignParentResp
        assertMessageResponse "parentId must reference an accessible category and must not create a cycle" foreignParentResp

        cycleResp <- updateFinanceCategoryExpectValue ownerCookie ownerRootId "Owner Root" (Just ownerChildId)
        assertStatusCode "Category cycle update should return 400" 400 cycleResp
        assertMessageResponse "parentId must reference an accessible category and must not create a cycle" cycleResp

        parentDeleteResp <- deleteFinanceCategoryExpectValue ownerCookie ownerRootId
        assertStatusCode "Deleting a category that still has children should return 409" 409 parentDeleteResp
        assertMessageResponse "Category cannot be deleted" parentDeleteResp

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

      it "should categorize and split transactions with replace semantics and validation rules" $ do
        uniquenessSuffix <- round . (* 1000000) <$> getPOSIXTime
        let classifyUsername = "fin-classify-" ++ show uniquenessSuffix
        ensureApprovedSandboxUser baseUsername classifyUsername basePassword
        cookie <- signinOnly classifyUsername basePassword

        account <- createFinanceAccount cookie "Classification Account"
        accountId <- requireObjectStringField "id" account

        txResp <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "classify-key-1" (object ["accountId" .= accountId, "amount" .= (4500 :: Int), "occurredAt" .= ("2026-04-10T10:00:00Z" :: String)])
        assertStatusCode "Transaction create should succeed" 200 txResp
        let transaction = getResponseBody txResp
        txId <- requireObjectStringField "id" transaction

        firstCategorize <- categorizeFinanceTransactionExpectValue cookie txId "pets.food"
        assertStatusCode "Categorize should succeed on uncategorized transaction" 200 firstCategorize
        assertFinanceTransactionCategoryAndSplits (Just "pets.food") [] (getResponseBody firstCategorize)

        secondCategorize <- categorizeFinanceTransactionExpectValue cookie txId "personal.clothing"
        assertStatusCode "Second categorize should replace whole-transaction category" 200 secondCategorize
        assertFinanceTransactionCategoryAndSplits (Just "personal.clothing") [] (getResponseBody secondCategorize)

        splitResp <- splitFinanceTransactionExpectValue cookie txId [("pets.food", 3500), ("personal.clothing", 1000)]
        assertStatusCode "Split should succeed with two rows summing to amount" 200 splitResp
        assertFinanceTransactionCategoryAndSplits Nothing [("pets.food", 3500), ("personal.clothing", 1000)] (getResponseBody splitResp)

        blockedCategorize <- categorizeFinanceTransactionExpectValue cookie txId "pets.food"
        assertStatusCode "Categorize should return 409 when split is active" 409 blockedCategorize
        assertMessageResponse "Transaction already has an active split" blockedCategorize

        replaceSplitResp <- splitFinanceTransactionExpectValue cookie txId [("pets.food", 2000), ("pets.food", 2500)]
        assertStatusCode "Second split should replace active split and allow repeated categories" 200 replaceSplitResp
        assertFinanceTransactionCategoryAndSplits Nothing [("pets.food", 2000), ("pets.food", 2500)] (getResponseBody replaceSplitResp)

        listed <- getFinanceTransactions cookie []
        case listed of
          [row] -> assertFinanceTransactionCategoryAndSplits Nothing [("pets.food", 2000), ("pets.food", 2500)] row
          _ -> assertFailure "Expected one transaction in list for classification scenario"

        invalidSplitCount <- splitFinanceTransactionExpectValue cookie txId [("pets.food", 4500)]
        assertStatusCode "Split should fail with fewer than two rows" 400 invalidSplitCount
        assertMessageResponse "splits must contain at least two rows, sum to the transaction amount, and use selectable categories" invalidSplitCount

        invalidSplitSum <- splitFinanceTransactionExpectValue cookie txId [("pets.food", 2000), ("personal.clothing", 2000)]
        assertStatusCode "Split should fail when row amounts do not match transaction amount" 400 invalidSplitSum
        assertMessageResponse "splits must contain at least two rows, sum to the transaction amount, and use selectable categories" invalidSplitSum

        nonSelectableCategory <- categorizeFinanceTransactionExpectValue cookie txId "income"
        assertStatusCode "Categorize should fail for non-selectable top-level category" 400 nonSelectableCategory
        assertMessageResponse "category must reference a selectable category" nonSelectableCategory

        unknownCategoryCategorize <- categorizeFinanceTransactionExpectValue cookie txId "missing.category"
        assertStatusCode "Categorize should fail for unknown category" 404 unknownCategoryCategorize
        assertMessageResponse "Transaction or category not found" unknownCategoryCategorize

        unknownCategorySplit <- splitFinanceTransactionExpectValue cookie txId [("pets.food", 2000), ("missing.category", 2500)]
        assertStatusCode "Split should fail for unknown category" 404 unknownCategorySplit
        assertMessageResponse "Transaction or category not found" unknownCategorySplit

        unknownTransactionCategorize <- categorizeFinanceTransactionExpectValue cookie "missing-tx" "pets.food"
        assertStatusCode "Categorize should fail for unknown transaction" 404 unknownTransactionCategorize
        assertMessageResponse "Transaction or category not found" unknownTransactionCategorize

        unknownTransactionSplit <- splitFinanceTransactionExpectValue cookie "missing-tx" [("pets.food", 2000), ("personal.clothing", 2500)]
        assertStatusCode "Split should fail for unknown transaction" 404 unknownTransactionSplit
        assertMessageResponse "Transaction or category not found" unknownTransactionSplit

      it "should link transfer transactions and enforce transfer validation rules" $ do
        uniquenessSuffix <- round . (* 1000000) <$> getPOSIXTime
        let transferUsername = "fin-transfer-" ++ show uniquenessSuffix
        ensureApprovedSandboxUser baseUsername transferUsername basePassword
        cookie <- signinOnly transferUsername basePassword

        sourceAccount <- createFinanceAccount cookie "Source Account"
        targetAccount <- createFinanceAccount cookie "Target Account"
        sameDirectionAccount <- createFinanceAccount cookie "Same Direction Account"
        sourceAccountId <- requireObjectStringField "id" sourceAccount
        targetAccountId <- requireObjectStringField "id" targetAccount
        sameDirectionAccountId <- requireObjectStringField "id" sameDirectionAccount

        sentTxResp <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "transfer-sent-1" (object ["accountId" .= sourceAccountId, "amount" .= (5000 :: Int), "occurredAt" .= ("2026-04-11T10:00:00Z" :: String)])
        receivedTxResp <- createFinanceTransactionExpectValue cookie financeTransactionsReceivedEndpoint "transfer-received-1" (object ["accountId" .= targetAccountId, "amount" .= (5000 :: Int), "occurredAt" .= ("2026-04-11T10:01:00Z" :: String)])
        assertStatusCode "Sent transfer candidate create should succeed" 200 sentTxResp
        assertStatusCode "Received transfer candidate create should succeed" 200 receivedTxResp
        sentTxId <- requireObjectStringField "id" (getResponseBody sentTxResp)
        receivedTxId <- requireObjectStringField "id" (getResponseBody receivedTxResp)

        linkedResp <- linkFinanceTransactionsExpectValue cookie sentTxId receivedTxId "transfer"
        assertStatusCode "Valid transfer link should succeed" 200 linkedResp
        assertTransferPairResponse sentTxId sourceAccountId 5000 receivedTxId targetAccountId 5000 (getResponseBody linkedResp)

        listed <- getFinanceTransactions cookie []
        let linkedRows = filter (\row -> financeTransactionIdValue row `elem` [sentTxId, receivedTxId]) listed
        assertEqual "Expected both linked transactions in ledger list" 2 (Prelude.length linkedRows)
        assertBool "Expected linked source row to include transfer summary" (any (transferPeerMatches receivedTxId targetAccountId 5000) linkedRows)
        assertBool "Expected linked target row to include transfer summary" (any (transferPeerMatches sentTxId sourceAccountId 5000) linkedRows)

        alreadyLinkedResp <- linkFinanceTransactionsExpectValue cookie sentTxId receivedTxId "transfer"
        assertStatusCode "Relinking already linked transactions should return 409" 409 alreadyLinkedResp
        assertMessageResponse "One or both transactions are already linked" alreadyLinkedResp

        invalidLinkTypeResp <- linkFinanceTransactionsExpectValue cookie sentTxId receivedTxId "manual"
        assertStatusCode "Unsupported linkType should return 400" 400 invalidLinkTypeResp
        assertMessageResponse "linkType must be transfer" invalidLinkTypeResp

        sameDirectionAResp <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "transfer-same-dir-a" (object ["accountId" .= sourceAccountId, "amount" .= (7000 :: Int), "occurredAt" .= ("2026-04-12T10:00:00Z" :: String)])
        sameDirectionBResp <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "transfer-same-dir-b" (object ["accountId" .= sameDirectionAccountId, "amount" .= (7000 :: Int), "occurredAt" .= ("2026-04-12T10:01:00Z" :: String)])
        sameDirectionAId <- requireObjectStringField "id" (getResponseBody sameDirectionAResp)
        sameDirectionBId <- requireObjectStringField "id" (getResponseBody sameDirectionBResp)
        sameDirectionResp <- linkFinanceTransactionsExpectValue cookie sameDirectionAId sameDirectionBId "transfer"
        assertStatusCode "Same direction transfer candidates should return 409" 409 sameDirectionResp
        assertMessageResponse "Invalid transfer link request" sameDirectionResp

        differentAmountAResp <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "transfer-diff-amount-a" (object ["accountId" .= sourceAccountId, "amount" .= (8100 :: Int), "occurredAt" .= ("2026-04-13T10:00:00Z" :: String)])
        differentAmountBResp <- createFinanceTransactionExpectValue cookie financeTransactionsReceivedEndpoint "transfer-diff-amount-b" (object ["accountId" .= targetAccountId, "amount" .= (8200 :: Int), "occurredAt" .= ("2026-04-13T10:01:00Z" :: String)])
        differentAmountAId <- requireObjectStringField "id" (getResponseBody differentAmountAResp)
        differentAmountBId <- requireObjectStringField "id" (getResponseBody differentAmountBResp)
        differentAmountResp <- linkFinanceTransactionsExpectValue cookie differentAmountAId differentAmountBId "transfer"
        assertStatusCode "Different amount transfer candidates should return 409" 409 differentAmountResp
        assertMessageResponse "Invalid transfer link request" differentAmountResp

        sameAccountSentResp <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "transfer-same-account-a" (object ["accountId" .= sourceAccountId, "amount" .= (9100 :: Int), "occurredAt" .= ("2026-04-14T10:00:00Z" :: String)])
        sameAccountReceivedResp <- createFinanceTransactionExpectValue cookie financeTransactionsReceivedEndpoint "transfer-same-account-b" (object ["accountId" .= sourceAccountId, "amount" .= (9100 :: Int), "occurredAt" .= ("2026-04-14T10:01:00Z" :: String)])
        sameAccountSentId <- requireObjectStringField "id" (getResponseBody sameAccountSentResp)
        sameAccountReceivedId <- requireObjectStringField "id" (getResponseBody sameAccountReceivedResp)
        sameAccountResp <- linkFinanceTransactionsExpectValue cookie sameAccountSentId sameAccountReceivedId "transfer"
        assertStatusCode "Same account transfer candidates should return 409" 409 sameAccountResp
        assertMessageResponse "Invalid transfer link request" sameAccountResp

        unknownTransactionResp <- linkFinanceTransactionsExpectValue cookie sentTxId "missing-tx" "transfer"
        assertStatusCode "Unknown transfer link transaction should return 404" 404 unknownTransactionResp
        assertMessageResponse "Transaction not found" unknownTransactionResp

      it "should build split-aware finance reports with deterministic filters and transfer exclusion" $ do
        uniquenessSuffix <- round . (* 1000000) <$> getPOSIXTime
        let reportUsername = "fin-report-" ++ show uniquenessSuffix
        ensureApprovedSandboxUser baseUsername reportUsername basePassword
        cookie <- signinOnly reportUsername basePassword

        accountA <- createFinanceAccount cookie "Report A"
        accountB <- createFinanceAccount cookie "Report B"
        accountC <- createFinanceAccount cookie "Report C"
        accountAId <- requireObjectStringField "id" accountA
        accountBId <- requireObjectStringField "id" accountB
        accountCId <- requireObjectStringField "id" accountC

        tx1Resp <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "report-key-1" (object ["accountId" .= accountAId, "amount" .= (1000 :: Int), "occurredAt" .= ("2026-04-01T10:00:00Z" :: String)])
        tx1Id <- requireObjectStringField "id" (getResponseBody tx1Resp)
        _ <- categorizeFinanceTransactionExpectValue cookie tx1Id "pets.food"

        tx2Resp <- createFinanceTransactionExpectValue cookie financeTransactionsReceivedEndpoint "report-key-2" (object ["accountId" .= accountBId, "amount" .= (2000 :: Int), "occurredAt" .= ("2026-04-02T10:00:00Z" :: String)])
        tx2Id <- requireObjectStringField "id" (getResponseBody tx2Resp)
        _ <- categorizeFinanceTransactionExpectValue cookie tx2Id "income.salary"

        tx3Resp <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "report-key-3" (object ["accountId" .= accountAId, "amount" .= (3000 :: Int), "occurredAt" .= ("2026-04-03T10:00:00Z" :: String)])
        tx3Id <- requireObjectStringField "id" (getResponseBody tx3Resp)
        _ <- splitFinanceTransactionExpectValue cookie tx3Id [("pets.food", 500), ("uncategorized.expense", 2500)]

        tx4Resp <- createFinanceTransactionExpectValue cookie financeTransactionsReceivedEndpoint "report-key-4" (object ["accountId" .= accountCId, "amount" .= (3000 :: Int), "occurredAt" .= ("2026-04-04T10:00:00Z" :: String)])
        tx4Id <- requireObjectStringField "id" (getResponseBody tx4Resp)
        _ <- splitFinanceTransactionExpectValue cookie tx4Id [("income.salary", 1000), ("uncategorized.income", 2000)]

        tx5Resp <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "report-key-5" (object ["accountId" .= accountAId, "amount" .= (700 :: Int), "occurredAt" .= ("2026-04-05T10:00:00Z" :: String)])
        tx5Id <- requireObjectStringField "id" (getResponseBody tx5Resp)

        tx6SourceResp <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "report-key-6" (object ["accountId" .= accountAId, "amount" .= (900 :: Int), "occurredAt" .= ("2026-04-06T10:00:00Z" :: String)])
        tx6TargetResp <- createFinanceTransactionExpectValue cookie financeTransactionsReceivedEndpoint "report-key-7" (object ["accountId" .= accountBId, "amount" .= (900 :: Int), "occurredAt" .= ("2026-04-06T10:01:00Z" :: String)])
        tx6SourceId <- requireObjectStringField "id" (getResponseBody tx6SourceResp)
        tx6TargetId <- requireObjectStringField "id" (getResponseBody tx6TargetResp)
        transferLinkResp <- linkFinanceTransactionsExpectValue cookie tx6SourceId tx6TargetId "transfer"
        assertStatusCode "Transfer link for report scenario should succeed" 200 transferLinkResp

        tx7Resp <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "report-key-8" (object ["accountId" .= accountAId, "amount" .= (1000 :: Int), "occurredAt" .= ("2026-04-07T10:00:00Z" :: String)])
        tx7Id <- requireObjectStringField "id" (getResponseBody tx7Resp)
        _ <- splitFinanceTransactionExpectValue cookie tx7Id [("pets.food", 400), ("pets.food", 600)]

        sentReport <- getFinanceReport cookie [("from", "2026-04-01T00:00:00Z"), ("to", "2026-04-08T00:00:00Z"), ("direction", "sent")]
        assertFinanceReportValues 5700 4 [tx7Id, tx5Id, tx3Id, tx1Id] sentReport

        receivedReport <- getFinanceReport cookie [("from", "2026-04-01T00:00:00Z"), ("to", "2026-04-08T00:00:00Z"), ("direction", "received")]
        assertFinanceReportValues 5000 2 [tx4Id, tx2Id] receivedReport

        allReport <- getFinanceReport cookie [("from", "2026-04-01T00:00:00Z"), ("to", "2026-04-08T00:00:00Z"), ("direction", "all")]
        assertFinanceReportValues (-700) 6 [tx7Id, tx5Id, tx4Id, tx3Id, tx2Id, tx1Id] allReport

        petsReport <- getFinanceReport cookie
          [ ("from", "2026-04-01T00:00:00Z")
          , ("to", "2026-04-08T00:00:00Z")
          , ("direction", "sent")
          , ("categoryIn", "pets.food")
          ]
        assertFinanceReportValues 2500 3 [tx7Id, tx3Id, tx1Id] petsReport

        uncategorizedAll <- getFinanceReport cookie
          [ ("from", "2026-04-01T00:00:00Z")
          , ("to", "2026-04-08T00:00:00Z")
          , ("direction", "all")
          , ("categoryIn", "uncategorized")
          ]
        assertFinanceReportValues (-1200) 3 [tx5Id, tx4Id, tx3Id] uncategorizedAll

        overlapAccountResp <- getFinanceReportExpectValue cookie
          [ ("from", "2026-04-01T00:00:00Z")
          , ("to", "2026-04-08T00:00:00Z")
          , ("accountIn", accountAId)
          , ("accountNotIn", accountAId)
          ]
        assertStatusCode "Overlapping account include/exclude should return 400" 400 overlapAccountResp
        assertMessageResponse "accountIn and accountNotIn must not overlap" overlapAccountResp

        overlapCategoryResp <- getFinanceReportExpectValue cookie
          [ ("from", "2026-04-01T00:00:00Z")
          , ("to", "2026-04-08T00:00:00Z")
          , ("categoryIn", "pets.food")
          , ("categoryNotIn", "pets.food")
          ]
        assertStatusCode "Overlapping category include/exclude should return 400" 400 overlapCategoryResp
        assertMessageResponse "categoryIn and categoryNotIn must not overlap" overlapCategoryResp

      it "should append, update, and delete transaction notes with shared trim and validation rules" $ do
        uniquenessSuffix <- round . (* 1000000) <$> getPOSIXTime
        let noteUsername = "fin-note-" ++ show uniquenessSuffix
            foreignUsername = "fin-nf-" ++ show uniquenessSuffix
        ensureApprovedSandboxUser baseUsername noteUsername basePassword
        ensureApprovedSandboxUser baseUsername foreignUsername basePassword
        cookie <- signinOnly noteUsername basePassword
        foreignCookie <- signinOnly foreignUsername basePassword

        account <- createFinanceAccount cookie "Notes Account"
        accountId <- requireObjectStringField "id" account
        foreignAccount <- createFinanceAccount foreignCookie "Foreign Notes Account"
        foreignAccountId <- requireObjectStringField "id" foreignAccount

        txResp <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "note-append-key-1" (object ["accountId" .= accountId, "amount" .= (1800 :: Int), "occurredAt" .= ("2026-04-21T10:00:00Z" :: String)])
        assertStatusCode "Transaction create for note append should succeed" 200 txResp
        txId <- requireObjectStringField "id" (getResponseBody txResp)

        foreignTxResp <- createFinanceTransactionExpectValue foreignCookie financeTransactionsSentEndpoint "note-append-key-2" (object ["accountId" .= foreignAccountId, "amount" .= (1900 :: Int), "occurredAt" .= ("2026-04-21T10:01:00Z" :: String)])
        assertStatusCode "Foreign transaction create should succeed" 200 foreignTxResp
        foreignTxId <- requireObjectStringField "id" (getResponseBody foreignTxResp)

        appendResp <- appendFinanceTransactionNoteExpectValue cookie txId "  First note  "
        assertStatusCode "Appending a note should succeed" 200 appendResp
        assertFinanceTransactionNotesTexts ["First note"] (getResponseBody appendResp)
        firstNoteId <- requireFinanceTransactionSingleNoteId (getResponseBody appendResp)
        firstNoteUpdatedAt <- requireFinanceTransactionSingleNoteUpdatedAt (getResponseBody appendResp)

        exact2000Resp <- appendFinanceTransactionNoteExpectValue cookie txId (replicate 2000 'a')
        assertStatusCode "Appending a 2000-char note should succeed" 200 exact2000Resp
        assertFinanceTransactionNotesTexts ["First note", replicate 2000 'a'] (getResponseBody exact2000Resp)

        tooLongResp <- appendFinanceTransactionNoteExpectValue cookie txId (replicate 2001 'a')
        assertStatusCode "Appending a >2000-char note should return 400" 400 tooLongResp
        assertMessageResponse "text must not be blank and must not exceed 2000 characters" tooLongResp

        blankResp <- appendFinanceTransactionNoteExpectValue cookie txId "    "
        assertStatusCode "Appending a blank note should return 400" 400 blankResp
        assertMessageResponse "text must not be blank and must not exceed 2000 characters" blankResp

        unknownResp <- appendFinanceTransactionNoteExpectValue cookie "missing-transaction" "hello"
        assertStatusCode "Appending note to unknown transaction should return 404" 404 unknownResp
        assertMessageResponse "Transaction not found" unknownResp

        foreignResp <- appendFinanceTransactionNoteExpectValue cookie foreignTxId "hello"
        assertStatusCode "Appending note to foreign transaction should return 404" 404 foreignResp
        assertMessageResponse "Transaction not found" foreignResp

        threadDelay 1100000
        updateResp <- updateFinanceTransactionNoteExpectValue cookie txId firstNoteId "  Revised note  "
        assertStatusCode "Updating note should succeed" 200 updateResp
        assertFinanceTransactionNotesTexts ["Revised note", replicate 2000 'a'] (getResponseBody updateResp)
        updatedNoteUpdatedAt <- requireFinanceTransactionFirstNoteUpdatedAt (getResponseBody updateResp)
        assertBool "Expected updatedAt to change after note update" (updatedNoteUpdatedAt /= firstNoteUpdatedAt)

        updateTooLongResp <- updateFinanceTransactionNoteExpectValue cookie txId firstNoteId (replicate 2001 'a')
        assertStatusCode "Updating note with >2000 chars should return 400" 400 updateTooLongResp
        assertMessageResponse "text must not be blank and must not exceed 2000 characters" updateTooLongResp

        updateBlankResp <- updateFinanceTransactionNoteExpectValue cookie txId firstNoteId "   "
        assertStatusCode "Updating note with blank text should return 400" 400 updateBlankResp
        assertMessageResponse "text must not be blank and must not exceed 2000 characters" updateBlankResp

        updateUnknownResp <- updateFinanceTransactionNoteExpectValue cookie txId "missing-note" "hello"
        assertStatusCode "Updating unknown note should return 404" 404 updateUnknownResp
        assertMessageResponse "Transaction or note not found" updateUnknownResp

        updateForeignResp <- updateFinanceTransactionNoteExpectValue cookie foreignTxId firstNoteId "hello"
        assertStatusCode "Updating foreign note should return 404" 404 updateForeignResp
        assertMessageResponse "Transaction or note not found" updateForeignResp

        deleteResp <- deleteFinanceTransactionNoteExpectValue cookie txId firstNoteId
        assertStatusCode "Deleting note should succeed" 200 deleteResp
        assertFinanceTransactionNotesTexts [replicate 2000 'a'] (getResponseBody deleteResp)

        deleteUnknownResp <- deleteFinanceTransactionNoteExpectValue cookie txId firstNoteId
        assertStatusCode "Deleting unknown note should return 404" 404 deleteUnknownResp
        assertMessageResponse "Transaction or note not found" deleteUnknownResp

        deleteForeignResp <- deleteFinanceTransactionNoteExpectValue cookie foreignTxId firstNoteId
        assertStatusCode "Deleting foreign note should return 404" 404 deleteForeignResp
        assertMessageResponse "Transaction or note not found" deleteForeignResp

        listed <- getFinanceTransactions cookie []
        case filter (\row -> financeTransactionIdValue row == txId) listed of
          [row] -> assertFinanceTransactionNotesTexts [replicate 2000 'a'] row
          _ -> assertFailure "Expected one matching transaction row for appended notes"

      it "should export canonical events and projection-backed finance views" $ do
        uniquenessSuffix <- round . (* 1000000) <$> getPOSIXTime
        let exportUsername = "fin-export-" ++ show uniquenessSuffix
        ensureApprovedSandboxUser baseUsername exportUsername basePassword
        cookie <- signinOnly exportUsername basePassword

        account <- createFinanceAccount cookie "Export Account"
        accountId <- requireObjectStringField "id" account

        txResp <- createFinanceTransactionExpectValue cookie financeTransactionsSentEndpoint "export-key-1" (object ["accountId" .= accountId, "amount" .= (1500 :: Int), "occurredAt" .= ("2026-05-01T10:00:00Z" :: String)])
        assertStatusCode "Export transaction create should succeed" 200 txResp
        txId <- requireObjectStringField "id" (getResponseBody txResp)

        _ <- appendFinanceTransactionNoteExpectValue cookie txId "Export note"
        _ <- createFinanceAccountSnapshotExpectValue cookie accountId 800 "2026-05-01T10:00:00Z"

        exportResp <- getFinanceExportExpectValue cookie
        assertStatusCode "Finance export should succeed" 200 exportResp
        let exportBody = getResponseBody exportResp
        case exportBody of
          Object root -> do
            case parseMaybe (.: "formatVersion") root of
              Just formatVersion -> assertEqual "Expected finance export formatVersion=1" (1 :: Int) formatVersion
              Nothing -> assertFailure "Expected finance export formatVersion"

            events <- case parseMaybe (.: "events") root of
              Just value -> pure (value :: [Value])
              Nothing -> assertFailure "Expected finance export events" >> pure []
            assertBool "Expected finance export events to be non-empty" (not (null events))
            let txEventMatches eventValue =
                  case eventValue of
                    Object eventObj ->
                      case parseMaybe (.: "payload") eventObj of
                        Just (Object payloadObj) ->
                          parseMaybe (.: "transactionId") payloadObj == Just txId
                        _ -> False
                    _ -> False
            assertBool "Expected canonical events to include created transaction payload" (any txEventMatches events)

            case parseMaybe (.: "views") root of
              Just (Object viewsObj) -> do
                case parseMaybe (.: "accounts") viewsObj of
                  Just accounts -> assertBool "Expected finance export accounts view to be non-empty" (not (null (accounts :: [Value])))
                  Nothing -> assertFailure "Expected finance export views.accounts"
                case parseMaybe (.: "categories") viewsObj of
                  Just categories -> assertBool "Expected finance export categories view to be non-empty" (not (null (categories :: [Value])))
                  Nothing -> assertFailure "Expected finance export views.categories"
                case parseMaybe (.: "transactions") viewsObj of
                  Just transactions -> do
                    let txRows = transactions :: [Value]
                    assertBool "Expected finance export transactions view to be non-empty" (not (null txRows))
                    case filter (\row -> financeTransactionIdValue row == txId) txRows of
                      [txRow] -> do
                        assertFinanceTransactionDirectionAndAmount "sent" 1500 txRow
                        assertFinanceTransactionNotesTexts ["Export note"] txRow
                      _ -> assertFailure "Expected exported transaction row to include created transaction"
                  Nothing -> assertFailure "Expected finance export views.transactions"
                case parseMaybe (.: "snapshots") viewsObj of
                  Just snapshots -> assertBool "Expected finance export snapshots view to be non-empty" (not (null (snapshots :: [Value])))
                  Nothing -> assertFailure "Expected finance export views.snapshots"
                assertBool "Expected finance export views to exclude reconciliation" (parseMaybe (.: "reconciliation") viewsObj == (Nothing :: Maybe Value))
                assertBool "Expected finance export views to exclude report" (parseMaybe (.: "report") viewsObj == (Nothing :: Maybe Value))
              _ -> assertFailure "Expected finance export views object"
          _ -> assertFailure "Expected finance export response object"

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

createFinanceAccountSnapshotExpectValue :: String -> String -> Int -> String -> IO (Response Value)
createFinanceAccountSnapshotExpectValue cookie accountId balance occurredAt = do
  req <- parseRequest ("POST http://localhost:8081" ++ financeAccountsEndpoint ++ "/" ++ accountId ++ "/snapshots")
  httpJSON $ setRequestMethod "POST"
         $ setRequestHeader "Cookie" [BS.pack cookie]
         $ setRequestHeader "Content-Type" ["application/json"]
         $ setRequestBodyJSON (object ["balance" .= balance, "occurredAt" .= occurredAt]) req

getFinanceAccountSnapshots :: String -> String -> IO [Value]
getFinanceAccountSnapshots cookie accountId = do
  req <- parseRequest ("GET http://localhost:8081" ++ financeAccountsEndpoint ++ "/" ++ accountId ++ "/snapshots")
  resp <- httpJSON $ setRequestMethod "GET"
                  $ setRequestHeader "Cookie" [BS.pack cookie]
                  req
  assertStatusCode "Finance account snapshot list should succeed" 200 (resp :: Response [Value])
  pure (getResponseBody resp)

getFinanceAccountReconciliationExpectValue :: String -> String -> Maybe String -> IO (Response Value)
getFinanceAccountReconciliationExpectValue cookie accountId mSnapshotId = do
  req <- parseRequest ("GET http://localhost:8081" ++ financeAccountsEndpoint ++ "/" ++ accountId ++ "/reconciliation")
  let query = maybe [] (\snapshotId -> [("snapshotId", Just (BS.pack snapshotId))]) mSnapshotId
  httpJSON $ setRequestMethod "GET"
         $ setRequestHeader "Cookie" [BS.pack cookie]
         $ setRequestQueryString query req

setFinanceAccountSnapshotReconciliationStatusExpectValue :: String -> String -> String -> String -> IO (Response Value)
setFinanceAccountSnapshotReconciliationStatusExpectValue cookie accountId snapshotId status = do
  req <- parseRequest ("PUT http://localhost:8081" ++ financeAccountsEndpoint ++ "/" ++ accountId ++ "/snapshots/" ++ snapshotId ++ "/reconciliation-status")
  httpJSON $ setRequestMethod "PUT"
         $ setRequestHeader "Cookie" [BS.pack cookie]
         $ setRequestHeader "Content-Type" ["application/json"]
         $ setRequestBodyJSON (object ["status" .= status]) req

createFinanceCategory :: String -> String -> Maybe String -> IO Value
createFinanceCategory cookie name parentId = do
  resp <- createFinanceCategoryExpectValue cookie name parentId
  assertStatusCode "Finance category create should succeed" 200 resp
  pure (getResponseBody resp)

createFinanceCategoryExpectValue :: String -> String -> Maybe String -> IO (Response Value)
createFinanceCategoryExpectValue cookie name parentId = do
  req <- parseRequest ("POST http://localhost:8081" ++ financeCategoriesEndpoint)
  let body =
        case parentId of
          Nothing -> object ["name" .= name]
          Just parentCategoryId -> object ["name" .= name, "parentId" .= parentCategoryId]
  httpJSON $ setRequestMethod "POST"
         $ setRequestHeader "Cookie" [BS.pack cookie]
         $ setRequestHeader "Content-Type" ["application/json"]
         $ setRequestBodyJSON body req

getFinanceCategories :: String -> IO [Value]
getFinanceCategories cookie = do
  req <- parseRequest ("GET http://localhost:8081" ++ financeCategoriesEndpoint)
  resp <- httpJSON $ setRequestMethod "GET"
                  $ setRequestHeader "Cookie" [BS.pack cookie]
                  req
  assertStatusCode "Finance category list should succeed" 200 (resp :: Response [Value])
  pure (getResponseBody resp)

updateFinanceCategory :: String -> String -> String -> Maybe String -> IO Value
updateFinanceCategory cookie categoryId name parentId = do
  resp <- updateFinanceCategoryExpectValue cookie categoryId name parentId
  assertStatusCode "Finance category update should succeed" 200 resp
  pure (getResponseBody resp)

updateFinanceCategoryExpectValue :: String -> String -> String -> Maybe String -> IO (Response Value)
updateFinanceCategoryExpectValue cookie categoryId name parentId = do
  req <- parseRequest ("POST http://localhost:8081" ++ financeCategoriesEndpoint ++ "/" ++ categoryId)
  let body =
        case parentId of
          Nothing -> object ["name" .= name]
          Just parentCategoryId -> object ["name" .= name, "parentId" .= parentCategoryId]
  httpJSON $ setRequestMethod "POST"
         $ setRequestHeader "Cookie" [BS.pack cookie]
         $ setRequestHeader "Content-Type" ["application/json"]
         $ setRequestBodyJSON body req

deleteFinanceCategory :: String -> String -> IO ()
deleteFinanceCategory cookie categoryId = do
  resp <- deleteFinanceCategoryExpectValue cookie categoryId
  assertStatusCode "Finance category delete should succeed" 200 resp

deleteFinanceCategoryExpectValue :: String -> String -> IO (Response Value)
deleteFinanceCategoryExpectValue cookie categoryId = do
  req <- parseRequest ("DELETE http://localhost:8081" ++ financeCategoriesEndpoint ++ "/" ++ categoryId)
  httpJSON $ setRequestMethod "DELETE"
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

getFinanceReport :: String -> [(String, String)] -> IO Value
getFinanceReport cookie queryParams = do
  resp <- getFinanceReportExpectValue cookie queryParams
  assertStatusCode "Finance report should succeed" 200 resp
  pure (getResponseBody resp)

getFinanceReportExpectValue :: String -> [(String, String)] -> IO (Response Value)
getFinanceReportExpectValue cookie queryParams = do
  req <- financeReportRequest cookie queryParams
  httpJSON req

getFinanceExportExpectValue :: String -> IO (Response Value)
getFinanceExportExpectValue cookie = do
  req <- parseRequest ("GET http://localhost:8081" ++ financeExportEndpoint)
  httpJSON $ setRequestMethod "GET"
         $ setRequestHeader "Cookie" [BS.pack cookie]
         req

financeTransactionsRequest :: String -> [(String, String)] -> IO Request
financeTransactionsRequest cookie queryParams = do
  req <- parseRequest ("GET http://localhost:8081" ++ financeTransactionsEndpoint)
  let encodedQuery = map (\(k, v) -> (BS.pack k, Just (BS.pack v))) queryParams
  pure $ setRequestHeader "Cookie" [BS.pack cookie] $ setRequestMethod "GET" $ setRequestQueryString encodedQuery req

financeReportRequest :: String -> [(String, String)] -> IO Request
financeReportRequest cookie queryParams = do
  req <- parseRequest ("GET http://localhost:8081" ++ financeReportEndpoint)
  let encodedQuery = map (\(k, v) -> (BS.pack k, Just (BS.pack v))) queryParams
  pure $ setRequestHeader "Cookie" [BS.pack cookie] $ setRequestMethod "GET" $ setRequestQueryString encodedQuery req

categorizeFinanceTransactionExpectValue :: String -> String -> String -> IO (Response Value)
categorizeFinanceTransactionExpectValue cookie transactionId categorySlug = do
  req <- parseRequest ("POST http://localhost:8081" ++ financeTransactionsEndpoint ++ "/" ++ transactionId ++ "/categorize")
  httpJSON $ setRequestMethod "POST"
         $ setRequestHeader "Cookie" [BS.pack cookie]
         $ setRequestHeader "Content-Type" ["application/json"]
         $ setRequestBodyJSON (object ["category" .= categorySlug]) req

splitFinanceTransactionExpectValue :: String -> String -> [(String, Int)] -> IO (Response Value)
splitFinanceTransactionExpectValue cookie transactionId rows = do
  req <- parseRequest ("POST http://localhost:8081" ++ financeTransactionsEndpoint ++ "/" ++ transactionId ++ "/split")
  let splitRows = map (\(categorySlug, amount) -> object ["amount" .= amount, "category" .= categorySlug]) rows
  httpJSON $ setRequestMethod "POST"
         $ setRequestHeader "Cookie" [BS.pack cookie]
         $ setRequestHeader "Content-Type" ["application/json"]
         $ setRequestBodyJSON (object ["splits" .= splitRows]) req

linkFinanceTransactionsExpectValue :: String -> String -> String -> String -> IO (Response Value)
linkFinanceTransactionsExpectValue cookie sourceTransactionId targetTransactionId linkType = do
  req <- parseRequest ("POST http://localhost:8081" ++ financeTransactionsEndpoint ++ "/link")
  httpJSON $ setRequestMethod "POST"
         $ setRequestHeader "Cookie" [BS.pack cookie]
         $ setRequestHeader "Content-Type" ["application/json"]
         $ setRequestBodyJSON
             (object
               [ "sourceTransactionId" .= sourceTransactionId
               , "targetTransactionId" .= targetTransactionId
               , "linkType" .= linkType
               ])
             req

appendFinanceTransactionNoteExpectValue :: String -> String -> String -> IO (Response Value)
appendFinanceTransactionNoteExpectValue cookie transactionId textValue = do
  req <- parseRequest ("POST http://localhost:8081" ++ financeTransactionsEndpoint ++ "/" ++ transactionId ++ "/notes")
  httpJSON $ setRequestMethod "POST"
         $ setRequestHeader "Cookie" [BS.pack cookie]
         $ setRequestHeader "Content-Type" ["application/json"]
         $ setRequestBodyJSON (object ["text" .= textValue]) req

updateFinanceTransactionNoteExpectValue :: String -> String -> String -> String -> IO (Response Value)
updateFinanceTransactionNoteExpectValue cookie transactionId noteId textValue = do
  req <- parseRequest ("PUT http://localhost:8081" ++ financeTransactionsEndpoint ++ "/" ++ transactionId ++ "/notes/" ++ noteId)
  httpJSON $ setRequestMethod "PUT"
         $ setRequestHeader "Cookie" [BS.pack cookie]
         $ setRequestHeader "Content-Type" ["application/json"]
         $ setRequestBodyJSON (object ["text" .= textValue]) req

deleteFinanceTransactionNoteExpectValue :: String -> String -> String -> IO (Response Value)
deleteFinanceTransactionNoteExpectValue cookie transactionId noteId = do
  req <- parseRequest ("DELETE http://localhost:8081" ++ financeTransactionsEndpoint ++ "/" ++ transactionId ++ "/notes/" ++ noteId)
  httpJSON $ setRequestMethod "DELETE"
         $ setRequestHeader "Cookie" [BS.pack cookie]
         $ setRequestHeader "Content-Type" ["application/json"]
         req

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

assertFinanceReportValues :: Int -> Int -> [String] -> Value -> Assertion
assertFinanceReportValues expectedTotal expectedCount expectedIds responseBody =
  case responseBody of
    Object value -> do
      case parseMaybe (.: "total") value of
        Just actualTotal -> assertEqual "Expected report total" expectedTotal (actualTotal :: Int)
        Nothing -> assertFailure "Expected report total"
      case parseMaybe (.: "count") value of
        Just actualCount -> assertEqual "Expected report count" expectedCount (actualCount :: Int)
        Nothing -> assertFailure "Expected report count"
      case parseMaybe (.: "transactionIds") value of
        Just actualIds -> assertEqual "Expected report transaction ids" expectedIds (actualIds :: [String])
        Nothing -> assertFailure "Expected report transaction ids"
    _ -> assertFailure "Expected finance report response object"

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

financeSnapshotIdValue :: Value -> String
financeSnapshotIdValue responseBody =
  case responseBody of
    Object value ->
      case parseMaybe (.: "id") value of
        Just actualId -> actualId
        Nothing -> error "Expected finance snapshot id field"
    _ -> error "Expected finance snapshot response object"

financeSnapshotReconciliationStatusValue :: Value -> String
financeSnapshotReconciliationStatusValue responseBody =
  case responseBody of
    Object value ->
      case parseMaybe (.: "reconciliationStatus") value of
        Just actualStatus -> actualStatus
        Nothing -> error "Expected finance snapshot reconciliationStatus field"
    _ -> error "Expected finance snapshot response object"

financeReconciliationSnapshotIdValue :: Value -> String
financeReconciliationSnapshotIdValue responseBody =
  case responseBody of
    Object value ->
      case parseMaybe (.: "snapshotId") value of
        Just actualId -> actualId
        Nothing -> error "Expected reconciliation snapshotId field"
    _ -> error "Expected finance reconciliation response object"

financeReconciliationStatusValue :: Value -> String
financeReconciliationStatusValue responseBody =
  case responseBody of
    Object value ->
      case parseMaybe (.: "reconciliationStatus") value of
        Just actualStatus -> actualStatus
        Nothing -> error "Expected reconciliation status field"
    _ -> error "Expected finance reconciliation response object"

financeReconciliationBasisSnapshotIdValue :: Value -> Maybe String
financeReconciliationBasisSnapshotIdValue responseBody =
  case responseBody of
    Object value ->
      case parseMaybe (.:? "basisSnapshotId") value of
        Just actualBasis -> actualBasis
        Nothing -> error "Expected reconciliation basisSnapshotId field"
    _ -> error "Expected finance reconciliation response object"

financeReconciliationBasisSnapshotOccurredAtValue :: Value -> Maybe String
financeReconciliationBasisSnapshotOccurredAtValue responseBody =
  case responseBody of
    Object value ->
      case parseMaybe (.:? "basisSnapshotOccurredAt") value of
        Just actualBasis -> actualBasis
        Nothing -> error "Expected reconciliation basisSnapshotOccurredAt field"
    _ -> error "Expected finance reconciliation response object"

assertFinanceReconciliationValues :: Int -> Int -> Value -> Assertion
assertFinanceReconciliationValues expectedDerivedBalance expectedDiscrepancy responseBody =
  case responseBody of
    Object value -> do
      case parseMaybe (.: "derivedBalanceAtSnapshot") value of
        Just actualDerived -> assertEqual "Expected reconciliation derivedBalanceAtSnapshot" expectedDerivedBalance (actualDerived :: Int)
        Nothing -> assertFailure "Expected reconciliation derivedBalanceAtSnapshot"
      case parseMaybe (.: "discrepancy") value of
        Just actualDiscrepancy -> assertEqual "Expected reconciliation discrepancy" expectedDiscrepancy (actualDiscrepancy :: Int)
        Nothing -> assertFailure "Expected reconciliation discrepancy"
      case parseMaybe (.: "reconciliationStatus") value of
        Just (_ :: String) -> pure ()
        Nothing -> assertFailure "Expected reconciliation reconciliationStatus"
      case parseMaybe (.:? "basisSnapshotId") value of
        Just (_ :: Maybe String) -> pure ()
        Nothing -> assertFailure "Expected reconciliation basisSnapshotId"
      case parseMaybe (.:? "basisSnapshotOccurredAt") value of
        Just (_ :: Maybe String) -> pure ()
        Nothing -> assertFailure "Expected reconciliation basisSnapshotOccurredAt"
      case ( parseMaybe (.: "snapshotId") value
           , parseMaybe (.: "snapshotOccurredAt") value
           , parseMaybe (.: "observedBalance") value
           ) of
        (Just snapshotId, Just snapshotOccurredAt, Just (_ :: Int)) -> do
          assertBool "Expected reconciliation snapshotId to be non-empty" (not (null (snapshotId :: String)))
          assertBool "Expected reconciliation snapshotOccurredAt to be non-empty" (not (null (snapshotOccurredAt :: String)))
        _ -> assertFailure "Expected reconciliation snapshot fields"
    _ -> assertFailure "Expected finance reconciliation response object"

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

financeTransactionCategoryValue :: Value -> Maybe String
financeTransactionCategoryValue responseBody =
  case responseBody of
    Object value -> parseMaybe (.: "category") value
    _ -> Nothing

financeTransactionSplitsValue :: Value -> [(String, Int)]
financeTransactionSplitsValue responseBody =
  case responseBody of
    Object value ->
      case parseMaybe (.: "splits") value of
        Just rows ->
          [ (categorySlug, amount)
          | Object splitRow <- (rows :: [Value])
          , Just categorySlug <- [parseMaybe (.: "category") splitRow]
          , Just amount <- [parseMaybe (.: "amount") splitRow]
          ]
        Nothing -> []
    _ -> []

financeTransactionTransferValue :: Value -> Maybe Value
financeTransactionTransferValue responseBody =
  case responseBody of
    Object value -> parseMaybe (.: "transfer") value
    _ -> Nothing

financeTransactionNotesValue :: Value -> [Value]
financeTransactionNotesValue responseBody =
  case responseBody of
    Object value ->
      case parseMaybe (.: "notes") value of
        Just notes -> notes
        Nothing -> []
    _ -> []

assertFinanceTransactionCategoryAndSplits :: Maybe String -> [(String, Int)] -> Value -> Assertion
assertFinanceTransactionCategoryAndSplits expectedCategory expectedSplits responseBody = do
  assertEqual "Expected finance transaction category" expectedCategory (financeTransactionCategoryValue responseBody)
  assertEqual "Expected finance transaction splits" expectedSplits (financeTransactionSplitsValue responseBody)

assertTransferPairResponse :: String -> String -> Int -> String -> String -> Int -> Value -> Assertion
assertTransferPairResponse sourceId sourceAccount sourceAmount targetId targetAccount targetAmount responseBody =
  case responseBody of
    Object value -> do
      case (parseMaybe (.: "source") value, parseMaybe (.: "target") value) of
        (Just sourceRow, Just targetRow) -> do
          assertEqual "Expected source transaction id in link response" sourceId (financeTransactionIdValue sourceRow)
          assertEqual "Expected target transaction id in link response" targetId (financeTransactionIdValue targetRow)
          assertBool "Expected source row transfer peer metadata" (transferPeerMatches targetId targetAccount targetAmount sourceRow)
          assertBool "Expected target row transfer peer metadata" (transferPeerMatches sourceId sourceAccount sourceAmount targetRow)
        _ -> assertFailure "Expected source and target transaction rows in transfer link response"
    _ -> assertFailure "Expected transfer link response object"

transferPeerMatches :: String -> String -> Int -> Value -> Bool
transferPeerMatches expectedPeerTransactionId expectedPeerAccountId expectedPeerAmount responseBody =
  case financeTransactionTransferValue responseBody of
    Just (Object transferObj) ->
      case ( parseMaybe (.: "linkType") transferObj
           , parseMaybe (.: "peerTransactionId") transferObj
           , parseMaybe (.: "peerAccountId") transferObj
           , parseMaybe (.: "peerAmount") transferObj
           , parseMaybe (.: "linkedAt") transferObj
           ) of
        (Just linkType, Just peerTransactionId, Just peerAccountId, Just peerAmount, Just linkedAt) ->
          (linkType :: String) == "transfer"
            && (peerTransactionId :: String) == expectedPeerTransactionId
            && (peerAccountId :: String) == expectedPeerAccountId
            && (peerAmount :: Int) == expectedPeerAmount
            && not (null (linkedAt :: String))
        _ -> False
    _ -> False

assertFinanceTransactionNotesTexts :: [String] -> Value -> Assertion
assertFinanceTransactionNotesTexts expectedTexts responseBody = do
  let notes = financeTransactionNotesValue responseBody
      extractedTexts =
        [ textValue
        | Object noteObj <- notes
        , Just textValue <- [parseMaybe (.: "text") noteObj]
        ]
  assertEqual "Expected finance transaction notes texts" expectedTexts extractedTexts
  mapM_ assertNoteShape notes
  where
    assertNoteShape noteValue =
      case noteValue of
        Object noteObj ->
          case ( parseMaybe (.: "id") noteObj
               , parseMaybe (.: "createdAt") noteObj
               , parseMaybe (.: "updatedAt") noteObj
               ) of
            (Just noteId, Just createdAt, Just updatedAt) -> do
              assertBool "Expected note id to be non-empty" (not (null (noteId :: String)))
              assertBool "Expected note createdAt to be non-empty" (not (null (createdAt :: String)))
              assertBool "Expected note updatedAt to be non-empty" (not (null (updatedAt :: String)))
            _ -> assertFailure "Expected note object to include id, createdAt, updatedAt"
        _ -> assertFailure "Expected note entry to be an object"

requireFinanceTransactionSingleNoteId :: Value -> IO String
requireFinanceTransactionSingleNoteId responseBody =
  case financeTransactionNotesValue responseBody of
    [Object noteObj] ->
      case parseMaybe (.: "id") noteObj of
        Just noteId -> pure noteId
        Nothing -> assertFailure "Expected note id field" >> pure ""
    _ -> assertFailure "Expected one note in transaction response" >> pure ""

requireFinanceTransactionSingleNoteUpdatedAt :: Value -> IO String
requireFinanceTransactionSingleNoteUpdatedAt responseBody =
  case financeTransactionNotesValue responseBody of
    [Object noteObj] ->
      case parseMaybe (.: "updatedAt") noteObj of
        Just updatedAt -> pure updatedAt
        Nothing -> assertFailure "Expected note updatedAt field" >> pure ""
    _ -> assertFailure "Expected one note in transaction response" >> pure ""

requireFinanceTransactionFirstNoteUpdatedAt :: Value -> IO String
requireFinanceTransactionFirstNoteUpdatedAt responseBody =
  case financeTransactionNotesValue responseBody of
    (Object noteObj:_) ->
      case parseMaybe (.: "updatedAt") noteObj of
        Just updatedAt -> pure updatedAt
        Nothing -> assertFailure "Expected note updatedAt field" >> pure ""
    _ -> assertFailure "Expected at least one note in transaction response" >> pure ""

financeCategoryIdValue :: Value -> String
financeCategoryIdValue responseBody =
  case responseBody of
    Object value ->
      case parseMaybe (.: "id") value of
        Just actualId -> actualId
        Nothing -> error "Expected finance category id field"
    _ -> error "Expected finance category response object"

financeCategoryNameValue :: Value -> String
financeCategoryNameValue responseBody =
  case responseBody of
    Object value ->
      case parseMaybe (.: "name") value of
        Just actualName -> actualName
        Nothing -> error "Expected finance category name field"
    _ -> error "Expected finance category response object"

financeCategoryOwnerValue :: Value -> String
financeCategoryOwnerValue responseBody =
  case responseBody of
    Object value ->
      case parseMaybe (.: "owner") value of
        Just actualOwner -> actualOwner
        Nothing -> error "Expected finance category owner field"
    _ -> error "Expected finance category response object"

financeCategorySelectableValue :: Value -> Bool
financeCategorySelectableValue responseBody =
  case responseBody of
    Object value ->
      case parseMaybe (.: "selectable") value of
        Just actualSelectable -> actualSelectable
        Nothing -> error "Expected finance category selectable field"
    _ -> error "Expected finance category response object"

financeCategoryParentIdValue :: Value -> Maybe String
financeCategoryParentIdValue responseBody =
  case responseBody of
    Object value -> parseMaybe (.: "parentId") value
    _ -> Nothing

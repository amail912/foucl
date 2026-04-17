{-# LANGUAGE OverloadedStrings #-}

module IntegrationPostgresTests (runIntegrationPostgresTests) where

import Data.Aeson (Value(..), object, (.:), (.=))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive (original)
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (isInfixOf)
import Data.Password.Argon2 (hashPassword, mkPassword, unPasswordHash)
import Data.Text (pack, unpack)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Simple
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Test.Hspec
import Test.HUnit (assertBool, assertEqual, assertFailure)
import Text.Read (readMaybe)

runIntegrationPostgresTests :: IO ()
runIntegrationPostgresTests = do
  assertPostgresReachable
  hspec $ do
    describe "Auth startup import" $ do
      it "imports filesystem-only users and preserves postgres-conflicting users" $ do
        fsOnlyExists <- authUserExists "startup-fs-only-user"
        assertBool "Expected filesystem-only startup user to be imported into Postgres" fsOnlyExists

        conflictHash <- fetchAuthUserPasswordHash "startup-conflict-user"
        assertEqual "Expected conflicting startup user to keep Postgres value" "postgres-conflict-hash" conflictHash

        postgresOnlyExists <- authUserExists "startup-postgres-only-user"
        assertBool "Expected existing Postgres-only startup user to be preserved" postgresOnlyExists

      it "emits startup overlap/conflict warnings in server log" $ do
        logContent <- readStartupLog
        assertBool
          "Expected overlap warning log entry for auth startup import"
          ("[startup][auth-import][warning] overlap detected" `isInfixOf` logContent)
        assertBool
          "Expected conflict warning log entry for startup-conflict-user"
          ("[startup][auth-import][warning] skipping conflicting username=startup-conflict-user policy=postgres-wins" `isInfixOf` logContent)

      it "remains idempotent across sandbox restart" $ do
        countBefore <- fetchAuthUserCount
        restartPostgresSandboxServer
        countAfter <- fetchAuthUserCount
        assertEqual "Expected startup import to remain idempotent after restart" countBefore countAfter

        fsOnlyCount <- fetchAuthUserCountByUsername "startup-fs-only-user"
        assertEqual "Expected exactly one imported filesystem-only startup user" 1 fsOnlyCount

        conflictHash <- fetchAuthUserPasswordHash "startup-conflict-user"
        assertEqual "Expected conflicting startup user to remain Postgres-authored after restart" "postgres-conflict-hash" conflictHash

    describe "Session startup import" $ do
      it "imports filesystem-only session records and preserves postgres-conflicting records" $ do
        fsOnlyStateUser <- fetchSessionStateUserId "33333333-3333-3333-3333-333333333333"
        assertEqual "Expected filesystem-only session state to be imported into Postgres" "startup-fs-only-session-user" fsOnlyStateUser

        fsOnlyHandleState <- fetchSessionHandleStateId "cccccccc-cccc-cccc-cccc-cccccccccccc"
        assertEqual "Expected filesystem-only session handle to be imported into Postgres" "33333333-3333-3333-3333-333333333333" fsOnlyHandleState

        fsOnlyBindingState <- fetchSessionBindingStateId "startup-fs-only-session-user"
        assertEqual "Expected filesystem-only user binding to be imported into Postgres" "33333333-3333-3333-3333-333333333333" fsOnlyBindingState

        conflictStateUser <- fetchSessionStateUserId "11111111-1111-1111-1111-111111111111"
        assertEqual "Expected conflicting startup session state to keep Postgres value" "startup-pg-conflict-user" conflictStateUser

        conflictHandleState <- fetchSessionHandleStateId "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"
        assertEqual "Expected conflicting startup session handle to keep Postgres value" "11111111-1111-1111-1111-111111111111" conflictHandleState

        conflictBindingState <- fetchSessionBindingStateId "startup-binding-conflict-user"
        assertEqual "Expected conflicting startup session user binding to keep Postgres value" "11111111-1111-1111-1111-111111111111" conflictBindingState

      it "emits startup overlap/conflict warnings in server log" $ do
        logContent <- readStartupLog
        assertBool
          "Expected overlap warning log entry for session startup import"
          ("[startup][session-import][warning] overlap detected" `isInfixOf` logContent)
        assertBool
          "Expected conflict warning log entry for session state conflict"
          ("[startup][session-import][warning] skipping conflicting state_id=11111111-1111-1111-1111-111111111111 policy=postgres-wins" `isInfixOf` logContent)
        assertBool
          "Expected conflict warning log entry for session handle conflict"
          ("[startup][session-import][warning] skipping conflicting session_id=aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa policy=postgres-wins" `isInfixOf` logContent)
        assertBool
          "Expected conflict warning log entry for session binding conflict"
          ("[startup][session-import][warning] skipping conflicting user_id=startup-binding-conflict-user policy=postgres-wins" `isInfixOf` logContent)

      it "remains idempotent across sandbox restart" $ do
        stateCountBefore <- fetchSessionStatesCount
        handleCountBefore <- fetchSessionHandlesCount
        bindingCountBefore <- fetchSessionBindingsCount

        restartPostgresSandboxServer

        stateCountAfter <- fetchSessionStatesCount
        handleCountAfter <- fetchSessionHandlesCount
        bindingCountAfter <- fetchSessionBindingsCount

        assertEqual "Expected startup state import to remain idempotent after restart" stateCountBefore stateCountAfter
        assertEqual "Expected startup handle import to remain idempotent after restart" handleCountBefore handleCountAfter
        assertEqual "Expected startup binding import to remain idempotent after restart" bindingCountBefore bindingCountAfter

        fsOnlyStateCount <- fetchSessionStateCountById "33333333-3333-3333-3333-333333333333"
        assertEqual "Expected exactly one imported filesystem-only startup state" 1 fsOnlyStateCount

        fsOnlyHandleCount <- fetchSessionHandleCountById "cccccccc-cccc-cccc-cccc-cccccccccccc"
        assertEqual "Expected exactly one imported filesystem-only startup handle" 1 fsOnlyHandleCount

        fsOnlyBindingCount <- fetchSessionBindingCountByUserId "startup-fs-only-session-user"
        assertEqual "Expected exactly one imported filesystem-only startup binding" 1 fsOnlyBindingCount

    describe "Calendar startup import" $ do
      it "imports filesystem-only items and preserves postgres-conflicting items" $ do
        fsOnlyExists <- calendarItemExists "startup-calendar-fs-only-user" "startup-calendar-fs-only-item"
        assertBool "Expected filesystem-only startup calendar item to be imported into Postgres" fsOnlyExists

        conflictTitle <- fetchCalendarItemLegacyTitle "startup-calendar-conflict-user" "startup-calendar-conflict-item"
        assertEqual "Expected conflicting startup calendar item to keep Postgres value" "postgres-calendar-conflict-title" conflictTitle

        postgresOnlyExists <- calendarItemExists "startup-calendar-postgres-only-user" "startup-calendar-postgres-only-item"
        assertBool "Expected existing Postgres-only startup calendar item to be preserved" postgresOnlyExists

      it "emits startup overlap/conflict warnings in server log" $ do
        logContent <- readStartupLog
        assertBool
          "Expected overlap warning log entry for calendar startup import"
          ("[startup][calendar-import][warning] overlap detected" `isInfixOf` logContent)
        assertBool
          "Expected conflict warning log entry for startup-calendar-conflict-item"
          ("[startup][calendar-import][warning] skipping conflicting user_id=startup-calendar-conflict-user item_id=startup-calendar-conflict-item policy=postgres-wins" `isInfixOf` logContent)

      it "remains idempotent across sandbox restart" $ do
        countBefore <- fetchCalendarItemsCount
        restartPostgresSandboxServer
        countAfter <- fetchCalendarItemsCount
        assertEqual "Expected calendar startup import to remain idempotent after restart" countBefore countAfter

        fsOnlyCount <- fetchCalendarItemCountByKey "startup-calendar-fs-only-user" "startup-calendar-fs-only-item"
        assertEqual "Expected exactly one imported filesystem-only startup calendar item" 1 fsOnlyCount

        conflictTitle <- fetchCalendarItemLegacyTitle "startup-calendar-conflict-user" "startup-calendar-conflict-item"
        assertEqual "Expected conflicting startup calendar item to remain Postgres-authored after restart" "postgres-calendar-conflict-title" conflictTitle

    describe "Trip-sharing startup import" $ do
      it "imports filesystem-only relations and preserves postgres-conflicting relations" $ do
        fsOnlyShareExists <- tripShareExists "startup-trip-sharing-owner" "startup-trip-sharing-fs-only-target"
        assertBool "Expected filesystem-only startup trip share to be imported into Postgres" fsOnlyShareExists

        conflictShareExists <- tripShareExists "startup-trip-sharing-owner" "startup-trip-sharing-conflict-target"
        assertBool "Expected conflicting startup trip share to keep Postgres value" conflictShareExists

        postgresOnlyShareExists <- tripShareExists "startup-trip-sharing-postgres-only-owner" "startup-trip-sharing-postgres-only-target"
        assertBool "Expected existing Postgres-only startup trip share to be preserved" postgresOnlyShareExists

        fsOnlySubscriptionExists <- tripSubscriptionExists "startup-trip-sharing-owner" "startup-trip-sharing-fs-only-target"
        assertBool "Expected filesystem-only startup trip subscription to be imported into Postgres" fsOnlySubscriptionExists

        conflictSubscriptionExists <- tripSubscriptionExists "startup-trip-sharing-owner" "startup-trip-sharing-conflict-target"
        assertBool "Expected conflicting startup trip subscription to keep Postgres value" conflictSubscriptionExists

        postgresOnlySubscriptionExists <- tripSubscriptionExists "startup-trip-sharing-postgres-only-owner" "startup-trip-sharing-postgres-only-target"
        assertBool "Expected existing Postgres-only startup trip subscription to be preserved" postgresOnlySubscriptionExists

      it "emits startup overlap/conflict warnings in server log" $ do
        logContent <- readStartupLog
        assertBool
          "Expected overlap warning log entry for trip-sharing startup import"
          ("[startup][trip-sharing-import][warning] overlap detected" `isInfixOf` logContent)
        assertBool
          "Expected conflict warning log entry for trip-sharing share conflict"
          ("[startup][trip-sharing-import][warning] skipping conflicting share owner_user_id=startup-trip-sharing-owner target_username=startup-trip-sharing-conflict-target policy=postgres-wins" `isInfixOf` logContent)
        assertBool
          "Expected conflict warning log entry for trip-sharing subscription conflict"
          ("[startup][trip-sharing-import][warning] skipping conflicting subscription owner_user_id=startup-trip-sharing-owner target_username=startup-trip-sharing-conflict-target policy=postgres-wins" `isInfixOf` logContent)

      it "remains idempotent across sandbox restart" $ do
        shareCountBefore <- fetchTripSharesCount
        subscriptionCountBefore <- fetchTripSubscriptionsCount
        restartPostgresSandboxServer
        shareCountAfter <- fetchTripSharesCount
        subscriptionCountAfter <- fetchTripSubscriptionsCount

        assertEqual "Expected trip-sharing share startup import to remain idempotent after restart" shareCountBefore shareCountAfter
        assertEqual "Expected trip-sharing subscription startup import to remain idempotent after restart" subscriptionCountBefore subscriptionCountAfter

        fsOnlyShareCount <- fetchTripShareCountByKey "startup-trip-sharing-owner" "startup-trip-sharing-fs-only-target"
        assertEqual "Expected exactly one imported filesystem-only startup trip share" 1 fsOnlyShareCount

        fsOnlySubscriptionCount <- fetchTripSubscriptionCountByKey "startup-trip-sharing-owner" "startup-trip-sharing-fs-only-target"
        assertEqual "Expected exactly one imported filesystem-only startup trip subscription" 1 fsOnlySubscriptionCount

    around_ withFreshPostgresFixtures $ do
      describe "Auth parity" $ do
        it "keeps signup success/conflict semantics" $ do
          suffix <- uniqueSuffix
          let username = "pg-signup-" ++ suffix
          signupOk <- performSignup username testPassword
          assertStatusCode "Signup should succeed" 200 signupOk

          signupDup <- performSignup username testPassword
          assertStatusCode "Duplicate signup should return bad request" 400 signupDup
          assertMessageResponse "Unable to create user" signupDup

        it "keeps signup rate-limiting semantics for non-bootstrap users" $ do
          suffix <- uniqueSuffix
          responses <- mapM
            (\i -> performSignupRaw ("pg-rate-limit-" ++ suffix ++ "-" ++ show i) testPassword)
            [1..6]
          let statusCodes = map getResponseStatusCode responses
          assertBool "Expected at least one allowed signup before rate-limit saturation" (any (== 200) statusCodes)
          assertBool "Expected signup rate-limiter to block after saturation" (any (== 400) statusCodes)

        it "keeps signin success, invalid credentials, and pending approval semantics" $ do
          adminSignin <- signinAsAdmin
          assertStatusCode "Bootstrap admin signin should succeed" 200 adminSignin
          assertSigninProfileResponse "admin" ["admin"] True adminSignin

          invalidSignin <- performSigninJSON "admin" "wrongpassword"
          assertStatusCode "Invalid credentials should return 401" 401 invalidSignin
          assertMessageResponse "Invalid credentials" invalidSignin

          suffix <- uniqueSuffix
          let pendingUsername = "pg-pending-" ++ suffix
          seedPendingUser pendingUsername

          pendingSignin <- performSigninJSON pendingUsername testPassword
          assertStatusCode "Pending account should return 403" 403 pendingSignin
          assertMessageResponse "Account pending approval" pendingSignin

        it "keeps auth profile success semantics" $ do
          adminCookie <- signinOnly "admin" testPassword

          profileResp <- getAuthProfile adminCookie
          assertStatusCode "Auth profile should succeed" 200 profileResp
          assertSigninProfileResponse "admin" ["admin"] True profileResp

        it "keeps admin pending moderation semantics" $ do
          adminCookie <- signinOnly "admin" testPassword

          suffix <- uniqueSuffix
          let pendingUsername = "pg-approvable-" ++ suffix
          seedPendingUser pendingUsername

          pendingUsers <- getPendingSignups adminCookie
          assertBool "Pending user should be listed" (pendingSignupValue pendingUsername `elem` pendingUsers)

          approveResp <- approvePendingSignupResponse adminCookie pendingUsername
          assertStatusCode "Approve should succeed" 200 approveResp

          approvedSignin <- performSigninJSON pendingUsername testPassword
          assertStatusCode "Approved user should sign in" 200 approvedSignin

          approveMissing <- approvePendingSignupResponse adminCookie "missing-pending-user"
          assertStatusCode "Missing pending user should return 404" 404 approveMissing
          assertMessageResponse "Not found" approveMissing

        it "keeps approved-user listing/delete/conflict semantics" $ do
          adminCookie <- signinOnly "admin" testPassword

          suffix <- uniqueSuffix
          let memberUsername = "pg-approved-" ++ suffix
          seedApprovedUser memberUsername ["member"]

          approvedUsers <- getApprovedUsers adminCookie
          assertBool "Bootstrap admin should be listed" (adminUserValue "admin" ["admin"] True `elem` approvedUsers)
          assertBool "Approved member should be listed" (adminUserValue memberUsername ["member"] True `elem` approvedUsers)

          deleteMember <- deleteApprovedUserResponse adminCookie memberUsername
          assertStatusCode "Delete approved member should succeed" 200 deleteMember

          deletedSignin <- performSigninJSON memberUsername testPassword
          assertStatusCode "Deleted approved user should no longer sign in" 401 deletedSignin
          assertMessageResponse "Invalid credentials" deletedSignin

          deleteMissing <- deleteApprovedUserResponse adminCookie "missing-approved-user"
          assertStatusCode "Missing approved user should return 404" 404 deleteMissing
          assertMessageResponse "Not found" deleteMissing

          deleteBootstrap <- deleteApprovedUserResponse adminCookie "admin"
          assertStatusCode "Deleting bootstrap admin should return conflict" 409 deleteBootstrap
          assertMessageResponse "Cannot delete bootstrap admin" deleteBootstrap

        it "keeps technical failure profile semantics" $ do
          adminCookie <- signinOnly "admin" testPassword

          _ <- runPsqlFile authDownMigration
          profileResp <- getAuthProfile adminCookie
          assertStatusCode "Profile should return technical error when storage fails" 500 profileResp
          assertMessageResponse "Unable to process authentication" profileResp

      describe "Session parity" $ do
        it "keeps session create and resolve semantics" $ do
          cookie <- signinOnly "admin" testPassword
          profileResp <- getAuthProfile cookie
          assertStatusCode "Authenticated profile should succeed with valid session" 200 profileResp
          assertSigninProfileResponse "admin" ["admin"] True profileResp

        it "keeps session resolve missing state handling semantics" $ do
          cookie <- signinOnly "admin" testPassword
          sid <- extractSessionIdFromCookie cookie
          stateId <- fetchStateIdForSession sid
          deleteHandleResult <- runPsqlCommand ("DELETE FROM session_handles WHERE session_id = " ++ quoteSql sid ++ "::uuid")
          case deleteHandleResult of
            Left err -> assertFailure ("Expected session handle delete success, got " ++ err)
            Right () -> pure ()
          deleteBindingResult <- runPsqlCommand ("DELETE FROM session_user_bindings WHERE user_id = " ++ quoteSql "admin")
          case deleteBindingResult of
            Left err -> assertFailure ("Expected session binding delete success, got " ++ err)
            Right () -> pure ()
          deleteStateResult <- runPsqlCommand ("DELETE FROM session_states WHERE state_id = " ++ quoteSql stateId ++ "::uuid")
          case deleteStateResult of
            Left err -> assertFailure ("Expected detached session state delete success, got " ++ err)
            Right () -> pure ()
          profileResp <- getAuthProfile cookie
          assertStatusCode "Missing session state should be treated as unauthenticated" 401 profileResp
          assertMessageResponse "Not authenticated" profileResp

        it "keeps session refresh/touch idle semantics" $ do
          cookie <- signinOnly "admin" testPassword
          sid <- extractSessionIdFromCookie cookie
          before <- fetchIdleEpochForSession sid

          profileResp <- getAuthProfile cookie
          assertStatusCode "Authenticated profile should resolve before idle refresh check" 200 profileResp

          after <- fetchIdleEpochForSession sid
          assertBool "Expected idle expiry timestamp to be refreshed or preserved forward" (after >= before)

        it "keeps session revoke single semantics" $ do
          cookie <- signinOnly "admin" testPassword
          signoutResp <- performSignoutRaw cookie False
          assertStatusCode "Signout should succeed" 200 signoutResp
          assertExpiredSetCookie signoutResp

          profileResp <- getAuthProfile cookie
          assertStatusCode "Signed-out session should no longer authenticate" 401 profileResp
          assertMessageResponse "Not authenticated" profileResp

        it "keeps session revoke-all semantics" $ do
          cookie1 <- signinOnly "admin" testPassword
          cookie2 <- signinOnly "admin" testPassword

          signoutResp <- performSignoutRaw cookie1 True
          assertStatusCode "Signout all should succeed" 200 signoutResp
          assertExpiredSetCookie signoutResp

          profileResp <- getAuthProfile cookie2
          assertStatusCode "Sibling session should be revoked by signout all" 401 profileResp
          assertMessageResponse "Not authenticated" profileResp

        it "keeps session technical failure semantics" $ do
          cookie <- signinOnly "admin" testPassword

          _ <- runPsqlFile sessionDownMigration

          profileResp <- getAuthProfile cookie
          assertStatusCode "Session storage technical failure should degrade to unauthenticated" 401 profileResp
          assertMessageResponse "Not authenticated" profileResp

          signoutResp <- performSignoutRaw cookie True
          assertStatusCode "Signout all should remain successful on session technical failure" 200 signoutResp
          assertExpiredSetCookie signoutResp

      describe "Calendar and trip-sharing parity" $ do
        it "keeps agenda create/list/update/validate/delete semantics" $ do
          suffix <- uniqueSuffix
          let username = "pg-030-agenda-" ++ suffix
          seedApprovedUser username ["member"]
          cookie <- signinOnly username testPassword

          initialItems <- getAgendaItems cookie
          assertEqual "Agenda should start empty" [] initialItems

          created <- createAgendaItemValue cookie (mkLegacyContent "Agenda parity title" "2025-04-01T09:00" "2025-04-01T10:00" "TODO")
          itemId <- extractCalendarItemId created
          assertEqual
            "Agenda create payload should keep expected shape"
            (object
              [ "id" .= itemId
              , "type" .= ("INTENTION" :: String)
              , "titre" .= ("Agenda parity title" :: String)
              , "fenetre_debut" .= ("2025-04-01T09:00" :: String)
              , "fenetre_fin" .= ("2025-04-01T10:00" :: String)
              , "statut" .= ("TODO" :: String)
              ])
            created

          listedAfterCreate <- getAgendaItems cookie
          assertEqual "Agenda list should contain created item" [created] listedAfterCreate

          updated <- createAgendaItemValue cookie
            (object
              [ "id" .= itemId
              , "type" .= ("INTENTION" :: String)
              , "titre" .= ("Agenda parity updated" :: String)
              , "fenetre_debut" .= ("2025-04-01T09:00" :: String)
              , "fenetre_fin" .= ("2025-04-01T10:00" :: String)
              , "statut" .= ("EN_COURS" :: String)
              ])
          assertEqual
            "Agenda update payload should keep expected shape"
            (object
              [ "id" .= itemId
              , "type" .= ("INTENTION" :: String)
              , "titre" .= ("Agenda parity updated" :: String)
              , "fenetre_debut" .= ("2025-04-01T09:00" :: String)
              , "fenetre_fin" .= ("2025-04-01T10:00" :: String)
              , "statut" .= ("EN_COURS" :: String)
              ])
            updated

          validateAgendaItem cookie itemId 37
          listedAfterValidate <- getAgendaItems cookie
          assertEqual
            "Agenda validate should set actual duration minutes"
            [ object
              [ "id" .= itemId
              , "type" .= ("INTENTION" :: String)
              , "titre" .= ("Agenda parity updated" :: String)
              , "fenetre_debut" .= ("2025-04-01T09:00" :: String)
              , "fenetre_fin" .= ("2025-04-01T10:00" :: String)
              , "statut" .= ("EN_COURS" :: String)
              , "duree_reelle_minutes" .= (37 :: Int)
              ]
            ]
            listedAfterValidate

          deleteAgendaItem cookie itemId
          listedAfterDelete <- getAgendaItems cookie
          assertEqual "Agenda should be empty after delete" [] listedAfterDelete

          deleteAgendaItemExpectStatus cookie "missing-agenda-item" 404

        it "keeps share/subscription add-list-delete semantics, ordering, and independence" $ do
          suffix <- uniqueSuffix
          let ownerUsername = "pg-030-share-owner-" ++ suffix
              otherUsername = "pg-030-share-other-" ++ suffix
              thirdUsername = "pg-030-share-third-" ++ suffix
          seedApprovedUser ownerUsername ["member"]
          seedApprovedUser otherUsername ["member"]
          seedApprovedUser thirdUsername ["member"]
          ownerCookie <- signinOnly ownerUsername testPassword

          emptyShares <- getSharedUsersList ownerCookie
          emptySubscriptions <- getSubscribedUsersList ownerCookie
          assertEqual "Shares should start empty" [] emptyShares
          assertEqual "Subscriptions should start empty" [] emptySubscriptions

          addSharedUser ownerCookie thirdUsername
          addSharedUser ownerCookie otherUsername
          addSharedUser ownerCookie otherUsername
          shares <- getSharedUsersList ownerCookie
          assertEqual
            "Shares should stay ordered and idempotent"
            [tripSharingUserValue otherUsername, tripSharingUserValue thirdUsername]
            shares

          deleteSharedUser ownerCookie otherUsername
          deleteSharedUser ownerCookie otherUsername
          sharesAfterDelete <- getSharedUsersList ownerCookie
          assertEqual "Deleting missing shared user should remain idempotent" [tripSharingUserValue thirdUsername] sharesAfterDelete

          addSubscribedUser ownerCookie thirdUsername
          addSubscribedUser ownerCookie otherUsername
          addSubscribedUser ownerCookie otherUsername
          subscriptions <- getSubscribedUsersList ownerCookie
          assertEqual
            "Subscriptions should stay ordered and idempotent"
            [tripSharingUserValue otherUsername, tripSharingUserValue thirdUsername]
            subscriptions

          deleteSubscribedUser ownerCookie otherUsername
          deleteSubscribedUser ownerCookie otherUsername
          subscriptionsAfterDelete <- getSubscribedUsersList ownerCookie
          assertEqual
            "Deleting missing subscribed user should remain idempotent"
            [tripSharingUserValue thirdUsername]
            subscriptionsAfterDelete

          addSharedUser ownerCookie otherUsername
          addSubscribedUser ownerCookie otherUsername
          independentShares <- getSharedUsersList ownerCookie
          independentSubscriptions <- getSubscribedUsersList ownerCookie
          assertEqual
            "Shares should remain independent from subscriptions"
            [tripSharingUserValue otherUsername, tripSharingUserValue thirdUsername]
            independentShares
          assertEqual
            "Subscriptions should remain independent from shares"
            [tripSharingUserValue otherUsername, tripSharingUserValue thirdUsername]
            independentSubscriptions

        it "keeps period-trips query validation semantics" $ do
          suffix <- uniqueSuffix
          let username = "pg-030-period-validate-" ++ suffix
          seedApprovedUser username ["member"]
          cookie <- signinOnly username testPassword

          assertPeriodTripsValidationError cookie Nothing (Just "2025-03-10T12:00") "start is required"
          assertPeriodTripsValidationError cookie (Just "not-a-date") (Just "2025-03-10T12:00") "start must be a valid ISO date-time string"
          assertPeriodTripsValidationError cookie (Just "2025-03-10T12:00") (Just "2025-03-10T12:00") "end must be strictly after start"

        it "keeps period-trips visibility ordering and seed-window semantics" $ do
          suffix <- uniqueSuffix
          let baseUsername = "pg-030-period-base-" ++ suffix
              otherUsername = "pg-030-period-other-" ++ suffix
              thirdUsername = "pg-030-period-third-" ++ suffix
          seedApprovedUser baseUsername ["member"]
          seedApprovedUser otherUsername ["member"]
          seedApprovedUser thirdUsername ["member"]
          baseCookie <- signinOnly baseUsername testPassword
          otherCookie <- signinOnly otherUsername testPassword
          thirdCookie <- signinOnly thirdUsername testPassword

          otherTrip <- createAgendaItemValue otherCookie (mkTripContent "2025-03-10T09:00" "2025-03-10T10:00" "Paris" "Le Mesnil")
          thirdTrip <- createAgendaItemValue thirdCookie (mkTripContent "2025-03-10T11:00" "2025-03-10T12:00" "Le Mesnil" "St Clair")

          addSubscribedUser baseCookie otherUsername
          addSharedUser thirdCookie baseUsername
          hiddenTrips <- getPeriodTripsList baseCookie "2025-03-10T00:00" "2025-03-11T00:00"
          assertEqual "Period trips should stay hidden without both relations" [] hiddenTrips

          addSharedUser otherCookie baseUsername
          addSubscribedUser baseCookie thirdUsername
          firstRead <- getPeriodTripsList baseCookie "2025-03-10T00:00" "2025-03-11T00:00"
          secondRead <- getPeriodTripsList baseCookie "2025-03-10T00:00" "2025-03-11T00:00"
          let expectedVisible =
                [ periodTripsUserValue otherUsername [otherTrip]
                , periodTripsUserValue thirdUsername [thirdTrip]
                ]
          assertEqual "Visible period-trip users should be returned in stable username order" expectedVisible firstRead
          assertEqual "Repeated period-trip reads should stay stable" expectedVisible secondRead

          deleteSubscribedUser baseCookie thirdUsername

          earlyTrip <- createAgendaItemValue otherCookie (mkTripContent "2025-03-20T07:00" "2025-03-20T08:00" "Paris" "Le Mesnil")
          seedTrip <- createAgendaItemValue otherCookie (mkTripContent "2025-03-20T09:00" "2025-03-20T09:30" "Le Mesnil" "Paris")
          startTrip <- createAgendaItemValue otherCookie (mkTripContent "2025-03-20T10:00" "2025-03-20T11:00" "Paris" "St Clair")
          middleTrip <- createAgendaItemValue otherCookie (mkTripContent "2025-03-20T12:00" "2025-03-20T13:00" "St Clair" "Le Mesnil")
          endTrip <- createAgendaItemValue otherCookie (mkTripContent "2025-03-20T15:00" "2025-03-20T16:00" "Le Mesnil" "Paris")

          seededWindowTrips <- getPeriodTripsList baseCookie "2025-03-20T10:00" "2025-03-20T15:00"
          assertEqual
            "Period trips should include last seed trip before start and only in-window trips"
            [periodTripsUserValue otherUsername [seedTrip, startTrip, middleTrip]]
            seededWindowTrips

          _ <- extractCalendarItemId earlyTrip
          _ <- extractCalendarItemId endTrip
          pure ()

withFreshPostgresFixtures :: IO () -> IO ()
withFreshPostgresFixtures action = do
  resetPostgresSchema
  seedApprovedUser "admin" ["admin"]
  action

assertPostgresReachable :: IO ()
assertPostgresReachable = do
  result <- runPsqlCommand "SELECT 1"
  case result of
    Left err -> assertFailure ("Expected reachable Postgres test database at " ++ postgresConn ++ ": " ++ err)
    Right () -> pure ()

resetPostgresSchema :: IO ()
resetPostgresSchema = do
  _ <- runPsqlFile tripSharingDownMigration
  _ <- runPsqlFile calendarDownMigration
  _ <- runPsqlFile sessionDownMigration
  _ <- runPsqlFile authDownMigration

  authUpResult <- runPsqlFile authUpMigration
  case authUpResult of
    Left err -> assertFailure ("Auth up migration failed: " ++ err)
    Right () -> pure ()

  sessionUpResult <- runPsqlFile sessionUpMigration
  case sessionUpResult of
    Left err -> assertFailure ("Session up migration failed: " ++ err)
    Right () -> pure ()

  calendarUpResult <- runPsqlFile calendarUpMigration
  case calendarUpResult of
    Left err -> assertFailure ("Calendar up migration failed: " ++ err)
    Right () -> pure ()

  tripSharingUpResult <- runPsqlFile tripSharingUpMigration
  case tripSharingUpResult of
    Left err -> assertFailure ("Trip-sharing up migration failed: " ++ err)
    Right () -> pure ()

  truncateResult <- runPsqlCommand "TRUNCATE TABLE auth_users, session_handles, session_user_bindings, session_states, calendar_items, trip_shares, trip_subscriptions"
  case truncateResult of
    Left err -> assertFailure ("Postgres table cleanup failed: " ++ err)
    Right () -> pure ()

signinAsAdmin :: IO (Response Value)
signinAsAdmin =
  performSigninJSON "admin" testPassword

performSignup :: String -> String -> IO (Response Value)
performSignup username password = do
  req <- parseRequest "POST http://localhost:8081/api/signup"
  httpJSON
    $ setRequestMethod "POST"
    $ setRequestHeader "Content-Type" ["application/json"]
    $ setRequestBodyJSON (authPayload username password) req

performSignupRaw :: String -> String -> IO (Response ByteString)
performSignupRaw username password = do
  req <- parseRequest "POST http://localhost:8081/api/signup"
  httpBS
    $ setRequestMethod "POST"
    $ setRequestHeader "Content-Type" ["application/json"]
    $ setRequestBodyJSON (authPayload username password) req

performSigninJSON :: String -> String -> IO (Response Value)
performSigninJSON username password = do
  req <- parseRequest "POST http://localhost:8081/api/signin"
  httpJSON
    $ setRequestMethod "POST"
    $ setRequestHeader "Content-Type" ["application/json"]
    $ setRequestBodyJSON (authPayload username password) req

signinOnly :: String -> String -> IO String
signinOnly username password = do
  signinResp <- performSigninRaw username password
  assertStatusCode "Signin should succeed" 200 signinResp
  case getFirstSetCookie signinResp of
    Nothing -> assertFailure "Expected Set-Cookie header" >> pure ""
    Just header -> pure (extractCookiePair header)

performSigninRaw :: String -> String -> IO (Response ByteString)
performSigninRaw username password = do
  req <- parseRequest "POST http://localhost:8081/api/signin"
  httpBS
    $ setRequestMethod "POST"
    $ setRequestHeader "Content-Type" ["application/json"]
    $ setRequestBodyJSON (authPayload username password) req

performSignoutRaw :: String -> Bool -> IO (Response ByteString)
performSignoutRaw cookie revokeAll = do
  req <- parseRequest endpoint
  httpBS
    $ setRequestMethod "POST"
    $ setRequestHeader "Cookie" [BS.pack cookie] req
  where
    endpoint =
      if revokeAll
        then "POST http://localhost:8081/api/signout?all=true"
        else "POST http://localhost:8081/api/signout?all=false"

getAuthProfile :: String -> IO (Response Value)
getAuthProfile cookie = do
  req <- parseRequest "GET http://localhost:8081/api/auth/profile"
  httpJSON
    $ setRequestMethod "GET"
    $ setRequestHeader "Cookie" [BS.pack cookie] req

getAgendaItems :: String -> IO [Value]
getAgendaItems cookie = do
  req <- parseRequest "GET http://localhost:8081/api/v1/calendar-items"
  resp <- httpJSON
    $ setRequestMethod "GET"
    $ setRequestHeader "Cookie" [BS.pack cookie] req
  assertStatusCode "Agenda list should succeed" 200 resp
  pure (getResponseBody resp)

createAgendaItemValue :: String -> Value -> IO Value
createAgendaItemValue cookie payload = do
  req <- parseRequest "POST http://localhost:8081/api/v1/calendar-items"
  resp <- httpJSON
    $ setRequestMethod "POST"
    $ setRequestHeader "Cookie" [BS.pack cookie]
    $ setRequestHeader "Content-Type" ["application/json"]
    $ setRequestBodyJSON payload req
  assertStatusCode "Agenda create/update should succeed" 200 resp
  pure (getResponseBody resp)

validateAgendaItem :: String -> String -> Int -> IO ()
validateAgendaItem cookie itemId minutes = do
  req <- parseRequest "POST http://localhost:8081/api/v1/calendar-items"
  resp <- httpNoBody
    $ setRequestMethod "POST"
    $ setRequestHeader "Cookie" [BS.pack cookie]
    $ setRequestHeader "Content-Type" ["application/json"]
    $ setRequestBodyJSON
      (object
        [ "id" .= itemId
        , "duree_reelle_minutes" .= minutes
        ])
      req
  assertStatusCode "Agenda validate should succeed" 200 resp

deleteAgendaItem :: String -> String -> IO ()
deleteAgendaItem cookie itemId = deleteAgendaItemExpectStatus cookie itemId 200

deleteAgendaItemExpectStatus :: String -> String -> Int -> IO ()
deleteAgendaItemExpectStatus cookie itemId expectedStatus = do
  req <- parseRequest ("DELETE http://localhost:8081/api/v1/calendar-items/" ++ itemId)
  resp <- httpNoBody
    $ setRequestMethod "DELETE"
    $ setRequestHeader "Cookie" [BS.pack cookie] req
  assertStatusCode "Agenda delete should return expected status" expectedStatus resp

getSharedUsersList :: String -> IO [Value]
getSharedUsersList cookie = do
  req <- parseRequest "GET http://localhost:8081/api/v1/trip-sharing/shares"
  resp <- httpJSON
    $ setRequestMethod "GET"
    $ setRequestHeader "Cookie" [BS.pack cookie] req
  assertStatusCode "Share list should succeed" 200 resp
  pure (getResponseBody resp)

addSharedUser :: String -> String -> IO ()
addSharedUser cookie username = do
  req <- parseRequest "POST http://localhost:8081/api/v1/trip-sharing/shares"
  resp <- httpNoBody
    $ setRequestMethod "POST"
    $ setRequestHeader "Cookie" [BS.pack cookie]
    $ setRequestHeader "Content-Type" ["application/json"]
    $ setRequestBodyJSON (object ["username" .= username]) req
  assertStatusCode "Share add should succeed" 200 resp

deleteSharedUser :: String -> String -> IO ()
deleteSharedUser cookie username = do
  req <- parseRequest ("DELETE http://localhost:8081/api/v1/trip-sharing/shares/" ++ username)
  resp <- httpNoBody
    $ setRequestMethod "DELETE"
    $ setRequestHeader "Cookie" [BS.pack cookie] req
  assertStatusCode "Share delete should succeed" 200 resp

getSubscribedUsersList :: String -> IO [Value]
getSubscribedUsersList cookie = do
  req <- parseRequest "GET http://localhost:8081/api/v1/trip-sharing/subscriptions"
  resp <- httpJSON
    $ setRequestMethod "GET"
    $ setRequestHeader "Cookie" [BS.pack cookie] req
  assertStatusCode "Subscription list should succeed" 200 resp
  pure (getResponseBody resp)

addSubscribedUser :: String -> String -> IO ()
addSubscribedUser cookie username = do
  req <- parseRequest "POST http://localhost:8081/api/v1/trip-sharing/subscriptions"
  resp <- httpNoBody
    $ setRequestMethod "POST"
    $ setRequestHeader "Cookie" [BS.pack cookie]
    $ setRequestHeader "Content-Type" ["application/json"]
    $ setRequestBodyJSON (object ["username" .= username]) req
  assertStatusCode "Subscription add should succeed" 200 resp

deleteSubscribedUser :: String -> String -> IO ()
deleteSubscribedUser cookie username = do
  req <- parseRequest ("DELETE http://localhost:8081/api/v1/trip-sharing/subscriptions/" ++ username)
  resp <- httpNoBody
    $ setRequestMethod "DELETE"
    $ setRequestHeader "Cookie" [BS.pack cookie] req
  assertStatusCode "Subscription delete should succeed" 200 resp

periodTripsRequest :: Maybe String -> Maybe String -> Maybe String -> IO Request
periodTripsRequest mCookie mStart mEnd = do
  req <- parseRequest "GET http://localhost:8081/api/v1/trip-sharing/period-trips"
  let withCookie =
        case mCookie of
          Nothing -> id
          Just cookie -> setRequestHeader "Cookie" [BS.pack cookie]
      query =
        maybe [] (\start -> [("start", Just (BS.pack start))]) mStart
          ++ maybe [] (\end -> [("end", Just (BS.pack end))]) mEnd
  pure $ withCookie $ setRequestMethod "GET" $ setRequestQueryString query req

getPeriodTripsList :: String -> String -> String -> IO [Value]
getPeriodTripsList cookie start end = do
  req <- periodTripsRequest (Just cookie) (Just start) (Just end)
  resp <- httpJSON req
  assertStatusCode "Period trips request should succeed" 200 resp
  pure (getResponseBody resp)

assertPeriodTripsValidationError :: String -> Maybe String -> Maybe String -> String -> IO ()
assertPeriodTripsValidationError cookie mStart mEnd expectedMessage = do
  req <- periodTripsRequest (Just cookie) mStart mEnd
  resp <- httpJSON req
  assertStatusCode "Period trips validation should return 400" 400 resp
  assertMessageResponse expectedMessage resp

extractCalendarItemId :: Value -> IO String
extractCalendarItemId value =
  case value of
    Object v ->
      case parseMaybe (.: "id") v of
        Just itemId ->
          if null (itemId :: String)
            then assertFailure "Expected non-empty calendar item id" >> pure ""
            else pure itemId
        Nothing -> assertFailure "Expected calendar item response with id field" >> pure ""
    _ -> assertFailure "Expected calendar item JSON object" >> pure ""

mkLegacyContent :: String -> String -> String -> String -> Value
mkLegacyContent title windowStart windowEnd status =
  object
    [ "type" .= ("INTENTION" :: String)
    , "titre" .= title
    , "fenetre_debut" .= windowStart
    , "fenetre_fin" .= windowEnd
    , "statut" .= status
    ]

mkTripContent :: String -> String -> String -> String -> Value
mkTripContent start end departure arrival =
  object
    [ "type" .= ("trip" :: String)
    , "windowStart" .= start
    , "windowEnd" .= end
    , "departurePlaceId" .= departure
    , "arrivalPlaceId" .= arrival
    ]

tripSharingUserValue :: String -> Value
tripSharingUserValue username = object ["username" .= username]

periodTripsUserValue :: String -> [Value] -> Value
periodTripsUserValue username trips = object ["username" .= username, "trips" .= trips]

getPendingSignups :: String -> IO [Value]
getPendingSignups cookie = do
  req <- parseRequest "GET http://localhost:8081/api/v1/admin/pending-signups"
  resp <- httpJSON
    $ setRequestMethod "GET"
    $ setRequestHeader "Cookie" [BS.pack cookie] req
  assertStatusCode "Pending signup list should succeed" 200 resp
  case getResponseBody resp of
    Array items -> pure (toList items)
    _ -> assertFailure "Expected pending signups array" >> pure []

approvePendingSignupResponse :: String -> String -> IO (Response Value)
approvePendingSignupResponse cookie username = do
  req <- parseRequest "POST http://localhost:8081/api/v1/admin/pending-signups/approve"
  httpJSON
    $ setRequestMethod "POST"
    $ setRequestHeader "Cookie" [BS.pack cookie]
    $ setRequestHeader "Content-Type" ["application/json"]
    $ setRequestBodyJSON (object ["username" .= username]) req

getApprovedUsers :: String -> IO [Value]
getApprovedUsers cookie = do
  req <- parseRequest "GET http://localhost:8081/api/v1/admin/users"
  resp <- httpJSON
    $ setRequestMethod "GET"
    $ setRequestHeader "Cookie" [BS.pack cookie] req
  assertStatusCode "Approved users list should succeed" 200 resp
  case getResponseBody resp of
    Array items -> pure (toList items)
    _ -> assertFailure "Expected approved users array" >> pure []

deleteApprovedUserResponse :: String -> String -> IO (Response Value)
deleteApprovedUserResponse cookie username = do
  req <- parseRequest ("DELETE http://localhost:8081/api/v1/admin/users/" ++ username)
  httpJSON
    $ setRequestMethod "DELETE"
    $ setRequestHeader "Cookie" [BS.pack cookie] req

fetchIdleEpochForSession :: String -> IO Double
fetchIdleEpochForSession sid = do
  scalarResult <- runPsqlScalar
    ("SELECT EXTRACT(EPOCH FROM s.idle_expires_at) "
      ++ "FROM session_states s "
      ++ "JOIN session_handles h ON h.state_id = s.state_id "
      ++ "WHERE h.session_id = " ++ quoteSql sid ++ "::uuid")
  case scalarResult of
    Left err -> assertFailure ("Unable to fetch idle expiry epoch for session: " ++ err) >> pure 0
    Right raw ->
      case readMaybe (trimTrailingNewline raw) of
        Nothing -> assertFailure ("Unable to parse idle expiry epoch from scalar value: " ++ raw) >> pure 0
        Just value -> pure value

fetchStateIdForSession :: String -> IO String
fetchStateIdForSession sid = do
  scalarResult <- runPsqlScalar
    ("SELECT state_id::text FROM session_handles WHERE session_id = " ++ quoteSql sid ++ "::uuid")
  case scalarResult of
    Left err -> assertFailure ("Unable to fetch state id for session: " ++ err) >> pure ""
    Right raw ->
      let value = trimTrailingNewline raw
       in if null value
            then assertFailure "Expected non-empty state id for session" >> pure ""
            else pure value

extractSessionIdFromCookie :: String -> IO String
extractSessionIdFromCookie cookie =
  case break (== '.') token of
    (sid, '.':_) | not (null sid) -> pure sid
    _ -> assertFailure "Expected signed session token format <sid>.<sig>" >> pure ""
  where
    token = drop (length ("foucl_session=" :: String)) cookie

runPsqlCommand :: String -> IO (Either String ())
runPsqlCommand sqlCommand = do
  (exitCode, _out, err) <- readProcessWithExitCode "psql" ["--dbname", postgresConn, "-v", "ON_ERROR_STOP=1", "-c", sqlCommand] ""
  pure $
    case exitCode of
      ExitSuccess -> Right ()
      ExitFailure _ -> Left err

runPsqlScalar :: String -> IO (Either String String)
runPsqlScalar sqlCommand = do
  (exitCode, out, err) <- readProcessWithExitCode "psql" ["--dbname", postgresConn, "-v", "ON_ERROR_STOP=1", "-tA", "-c", sqlCommand] ""
  pure $
    case exitCode of
      ExitSuccess -> Right out
      ExitFailure _ -> Left err

authUserExists :: String -> IO Bool
authUserExists username = do
  scalarResult <- runPsqlScalar ("SELECT EXISTS(SELECT 1 FROM auth_users WHERE username = " ++ quoteSql username ++ ")")
  case scalarResult of
    Left err -> assertFailure ("Unable to query auth user existence for '" ++ username ++ "': " ++ err) >> pure False
    Right raw ->
      case trimTrailingNewline raw of
        "t" -> pure True
        "f" -> pure False
        value -> assertFailure ("Unexpected EXISTS value for '" ++ username ++ "': " ++ value) >> pure False

fetchAuthUserPasswordHash :: String -> IO String
fetchAuthUserPasswordHash username = do
  scalarResult <- runPsqlScalar ("SELECT password_hash FROM auth_users WHERE username = " ++ quoteSql username)
  case scalarResult of
    Left err -> assertFailure ("Unable to query password hash for '" ++ username ++ "': " ++ err) >> pure ""
    Right raw ->
      let value = trimTrailingNewline raw
       in if null value
            then assertFailure ("Expected non-empty password hash for '" ++ username ++ "'") >> pure ""
            else pure value

fetchAuthUserCount :: IO Int
fetchAuthUserCount = do
  scalarResult <- runPsqlScalar "SELECT COUNT(*) FROM auth_users"
  case scalarResult of
    Left err -> assertFailure ("Unable to query auth user count: " ++ err) >> pure 0
    Right raw ->
      case readMaybe (trimTrailingNewline raw) of
        Nothing -> assertFailure ("Unable to parse auth user count from value: " ++ raw) >> pure 0
        Just value -> pure value

fetchAuthUserCountByUsername :: String -> IO Int
fetchAuthUserCountByUsername username = do
  scalarResult <- runPsqlScalar ("SELECT COUNT(*) FROM auth_users WHERE username = " ++ quoteSql username)
  case scalarResult of
    Left err -> assertFailure ("Unable to query auth user count for '" ++ username ++ "': " ++ err) >> pure 0
    Right raw ->
      case readMaybe (trimTrailingNewline raw) of
        Nothing -> assertFailure ("Unable to parse auth user count for '" ++ username ++ "' from value: " ++ raw) >> pure 0
        Just value -> pure value

fetchSessionStateUserId :: String -> IO String
fetchSessionStateUserId stateId = do
  scalarResult <- runPsqlScalar ("SELECT user_id FROM session_states WHERE state_id = " ++ quoteSql stateId ++ "::uuid")
  case scalarResult of
    Left err -> assertFailure ("Unable to query session state user for '" ++ stateId ++ "': " ++ err) >> pure ""
    Right raw ->
      let value = trimTrailingNewline raw
       in if null value
            then assertFailure ("Expected non-empty session state user for '" ++ stateId ++ "'") >> pure ""
            else pure value

fetchSessionHandleStateId :: String -> IO String
fetchSessionHandleStateId sessionId = do
  scalarResult <- runPsqlScalar ("SELECT state_id::text FROM session_handles WHERE session_id = " ++ quoteSql sessionId ++ "::uuid")
  case scalarResult of
    Left err -> assertFailure ("Unable to query session handle state for '" ++ sessionId ++ "': " ++ err) >> pure ""
    Right raw ->
      let value = trimTrailingNewline raw
       in if null value
            then assertFailure ("Expected non-empty session handle state for '" ++ sessionId ++ "'") >> pure ""
            else pure value

fetchSessionBindingStateId :: String -> IO String
fetchSessionBindingStateId userId = do
  scalarResult <- runPsqlScalar ("SELECT state_id::text FROM session_user_bindings WHERE user_id = " ++ quoteSql userId)
  case scalarResult of
    Left err -> assertFailure ("Unable to query session binding state for '" ++ userId ++ "': " ++ err) >> pure ""
    Right raw ->
      let value = trimTrailingNewline raw
       in if null value
            then assertFailure ("Expected non-empty session binding state for '" ++ userId ++ "'") >> pure ""
            else pure value

fetchSessionStatesCount :: IO Int
fetchSessionStatesCount = do
  scalarResult <- runPsqlScalar "SELECT COUNT(*) FROM session_states"
  case scalarResult of
    Left err -> assertFailure ("Unable to query session state count: " ++ err) >> pure 0
    Right raw ->
      case readMaybe (trimTrailingNewline raw) of
        Nothing -> assertFailure ("Unable to parse session state count from value: " ++ raw) >> pure 0
        Just value -> pure value

fetchSessionHandlesCount :: IO Int
fetchSessionHandlesCount = do
  scalarResult <- runPsqlScalar "SELECT COUNT(*) FROM session_handles"
  case scalarResult of
    Left err -> assertFailure ("Unable to query session handle count: " ++ err) >> pure 0
    Right raw ->
      case readMaybe (trimTrailingNewline raw) of
        Nothing -> assertFailure ("Unable to parse session handle count from value: " ++ raw) >> pure 0
        Just value -> pure value

fetchSessionBindingsCount :: IO Int
fetchSessionBindingsCount = do
  scalarResult <- runPsqlScalar "SELECT COUNT(*) FROM session_user_bindings"
  case scalarResult of
    Left err -> assertFailure ("Unable to query session binding count: " ++ err) >> pure 0
    Right raw ->
      case readMaybe (trimTrailingNewline raw) of
        Nothing -> assertFailure ("Unable to parse session binding count from value: " ++ raw) >> pure 0
        Just value -> pure value

fetchSessionStateCountById :: String -> IO Int
fetchSessionStateCountById stateId = do
  scalarResult <- runPsqlScalar ("SELECT COUNT(*) FROM session_states WHERE state_id = " ++ quoteSql stateId ++ "::uuid")
  case scalarResult of
    Left err -> assertFailure ("Unable to query session state count for '" ++ stateId ++ "': " ++ err) >> pure 0
    Right raw ->
      case readMaybe (trimTrailingNewline raw) of
        Nothing -> assertFailure ("Unable to parse session state count for '" ++ stateId ++ "' from value: " ++ raw) >> pure 0
        Just value -> pure value

fetchSessionHandleCountById :: String -> IO Int
fetchSessionHandleCountById sessionId = do
  scalarResult <- runPsqlScalar ("SELECT COUNT(*) FROM session_handles WHERE session_id = " ++ quoteSql sessionId ++ "::uuid")
  case scalarResult of
    Left err -> assertFailure ("Unable to query session handle count for '" ++ sessionId ++ "': " ++ err) >> pure 0
    Right raw ->
      case readMaybe (trimTrailingNewline raw) of
        Nothing -> assertFailure ("Unable to parse session handle count for '" ++ sessionId ++ "' from value: " ++ raw) >> pure 0
        Just value -> pure value

fetchSessionBindingCountByUserId :: String -> IO Int
fetchSessionBindingCountByUserId userId = do
  scalarResult <- runPsqlScalar ("SELECT COUNT(*) FROM session_user_bindings WHERE user_id = " ++ quoteSql userId)
  case scalarResult of
    Left err -> assertFailure ("Unable to query session binding count for '" ++ userId ++ "': " ++ err) >> pure 0
    Right raw ->
      case readMaybe (trimTrailingNewline raw) of
        Nothing -> assertFailure ("Unable to parse session binding count for '" ++ userId ++ "' from value: " ++ raw) >> pure 0
        Just value -> pure value

calendarItemExists :: String -> String -> IO Bool
calendarItemExists userId itemId = do
  scalarResult <- runPsqlScalar
    ( "SELECT EXISTS(SELECT 1 FROM calendar_items WHERE user_id = "
        ++ quoteSql userId
        ++ " AND item_id = "
        ++ quoteSql itemId
        ++ ")"
    )
  case scalarResult of
    Left err -> assertFailure ("Unable to query calendar item existence for user_id=" ++ userId ++ " item_id=" ++ itemId ++ ": " ++ err) >> pure False
    Right raw ->
      case trimTrailingNewline raw of
        "t" -> pure True
        "f" -> pure False
        value -> assertFailure ("Unexpected calendar EXISTS value for user_id=" ++ userId ++ " item_id=" ++ itemId ++ ": " ++ value) >> pure False

fetchCalendarItemLegacyTitle :: String -> String -> IO String
fetchCalendarItemLegacyTitle userId itemId = do
  scalarResult <- runPsqlScalar
    ( "SELECT legacy_title FROM calendar_items WHERE user_id = "
        ++ quoteSql userId
        ++ " AND item_id = "
        ++ quoteSql itemId
    )
  case scalarResult of
    Left err -> assertFailure ("Unable to query calendar legacy_title for user_id=" ++ userId ++ " item_id=" ++ itemId ++ ": " ++ err) >> pure ""
    Right raw ->
      let value = trimTrailingNewline raw
       in if null value
            then assertFailure ("Expected non-empty calendar legacy_title for user_id=" ++ userId ++ " item_id=" ++ itemId) >> pure ""
            else pure value

fetchCalendarItemsCount :: IO Int
fetchCalendarItemsCount = do
  scalarResult <- runPsqlScalar "SELECT COUNT(*) FROM calendar_items"
  case scalarResult of
    Left err -> assertFailure ("Unable to query calendar item count: " ++ err) >> pure 0
    Right raw ->
      case readMaybe (trimTrailingNewline raw) of
        Nothing -> assertFailure ("Unable to parse calendar item count from value: " ++ raw) >> pure 0
        Just value -> pure value

fetchCalendarItemCountByKey :: String -> String -> IO Int
fetchCalendarItemCountByKey userId itemId = do
  scalarResult <- runPsqlScalar
    ( "SELECT COUNT(*) FROM calendar_items WHERE user_id = "
        ++ quoteSql userId
        ++ " AND item_id = "
        ++ quoteSql itemId
    )
  case scalarResult of
    Left err -> assertFailure ("Unable to query calendar item count for user_id=" ++ userId ++ " item_id=" ++ itemId ++ ": " ++ err) >> pure 0
    Right raw ->
      case readMaybe (trimTrailingNewline raw) of
        Nothing -> assertFailure ("Unable to parse calendar item count for user_id=" ++ userId ++ " item_id=" ++ itemId ++ " from value: " ++ raw) >> pure 0
        Just value -> pure value

tripShareExists :: String -> String -> IO Bool
tripShareExists ownerUserId targetUsername = do
  scalarResult <- runPsqlScalar
    ( "SELECT EXISTS(SELECT 1 FROM trip_shares WHERE owner_user_id = "
        ++ quoteSql ownerUserId
        ++ " AND target_username = "
        ++ quoteSql targetUsername
        ++ ")"
    )
  case scalarResult of
    Left err -> assertFailure ("Unable to query trip share existence for owner_user_id=" ++ ownerUserId ++ " target_username=" ++ targetUsername ++ ": " ++ err) >> pure False
    Right raw ->
      case trimTrailingNewline raw of
        "t" -> pure True
        "f" -> pure False
        value -> assertFailure ("Unexpected trip share EXISTS value for owner_user_id=" ++ ownerUserId ++ " target_username=" ++ targetUsername ++ ": " ++ value) >> pure False

tripSubscriptionExists :: String -> String -> IO Bool
tripSubscriptionExists ownerUserId targetUsername = do
  scalarResult <- runPsqlScalar
    ( "SELECT EXISTS(SELECT 1 FROM trip_subscriptions WHERE owner_user_id = "
        ++ quoteSql ownerUserId
        ++ " AND target_username = "
        ++ quoteSql targetUsername
        ++ ")"
    )
  case scalarResult of
    Left err -> assertFailure ("Unable to query trip subscription existence for owner_user_id=" ++ ownerUserId ++ " target_username=" ++ targetUsername ++ ": " ++ err) >> pure False
    Right raw ->
      case trimTrailingNewline raw of
        "t" -> pure True
        "f" -> pure False
        value -> assertFailure ("Unexpected trip subscription EXISTS value for owner_user_id=" ++ ownerUserId ++ " target_username=" ++ targetUsername ++ ": " ++ value) >> pure False

fetchTripSharesCount :: IO Int
fetchTripSharesCount = do
  scalarResult <- runPsqlScalar "SELECT COUNT(*) FROM trip_shares"
  case scalarResult of
    Left err -> assertFailure ("Unable to query trip share count: " ++ err) >> pure 0
    Right raw ->
      case readMaybe (trimTrailingNewline raw) of
        Nothing -> assertFailure ("Unable to parse trip share count from value: " ++ raw) >> pure 0
        Just value -> pure value

fetchTripSubscriptionsCount :: IO Int
fetchTripSubscriptionsCount = do
  scalarResult <- runPsqlScalar "SELECT COUNT(*) FROM trip_subscriptions"
  case scalarResult of
    Left err -> assertFailure ("Unable to query trip subscription count: " ++ err) >> pure 0
    Right raw ->
      case readMaybe (trimTrailingNewline raw) of
        Nothing -> assertFailure ("Unable to parse trip subscription count from value: " ++ raw) >> pure 0
        Just value -> pure value

fetchTripShareCountByKey :: String -> String -> IO Int
fetchTripShareCountByKey ownerUserId targetUsername = do
  scalarResult <- runPsqlScalar
    ( "SELECT COUNT(*) FROM trip_shares WHERE owner_user_id = "
        ++ quoteSql ownerUserId
        ++ " AND target_username = "
        ++ quoteSql targetUsername
    )
  case scalarResult of
    Left err -> assertFailure ("Unable to query trip share count for owner_user_id=" ++ ownerUserId ++ " target_username=" ++ targetUsername ++ ": " ++ err) >> pure 0
    Right raw ->
      case readMaybe (trimTrailingNewline raw) of
        Nothing -> assertFailure ("Unable to parse trip share count for owner_user_id=" ++ ownerUserId ++ " target_username=" ++ targetUsername ++ " from value: " ++ raw) >> pure 0
        Just value -> pure value

fetchTripSubscriptionCountByKey :: String -> String -> IO Int
fetchTripSubscriptionCountByKey ownerUserId targetUsername = do
  scalarResult <- runPsqlScalar
    ( "SELECT COUNT(*) FROM trip_subscriptions WHERE owner_user_id = "
        ++ quoteSql ownerUserId
        ++ " AND target_username = "
        ++ quoteSql targetUsername
    )
  case scalarResult of
    Left err -> assertFailure ("Unable to query trip subscription count for owner_user_id=" ++ ownerUserId ++ " target_username=" ++ targetUsername ++ ": " ++ err) >> pure 0
    Right raw ->
      case readMaybe (trimTrailingNewline raw) of
        Nothing -> assertFailure ("Unable to parse trip subscription count for owner_user_id=" ++ ownerUserId ++ " target_username=" ++ targetUsername ++ " from value: " ++ raw) >> pure 0
        Just value -> pure value

readStartupLog :: IO String
readStartupLog = do
  let logPath = "dist-newstyle/sandbox/foucl/.foucl/.foucl.log"
  exists <- doesFileExist logPath
  if not exists
    then assertFailure ("Expected startup log file at " ++ logPath) >> pure ""
    else readFile logPath

restartPostgresSandboxServer :: IO ()
restartPostgresSandboxServer = do
  let cmd =
        "FOUCL_SESSION_SECRET=dev-only-session-secret "
          ++ "FOUCL_CONFIG_FILE=dist-newstyle/sandbox/foucl/config/app-config.auth-postgres.json "
          ++ "FOUCL_SESSION_COOKIE_SECURE=false "
          ++ "scripts/daemon/foucld restart "
          ++ "--bin dist-newstyle/sandbox/foucl/foucl "
          ++ "--pidfile dist-newstyle/sandbox/foucl/.foucl/foucl.pid"
  (exitCode, _out, err) <- readProcessWithExitCode "/bin/bash" ["-lc", cmd] ""
  case exitCode of
    ExitSuccess -> waitForServerReady
    ExitFailure _ -> assertFailure ("Failed to restart sandbox server for startup import idempotency check: " ++ err)

waitForServerReady :: IO ()
waitForServerReady = do
  let cmd =
        "for i in $(seq 1 40); do "
          ++ "if curl --silent --show-error --output /dev/null --max-time 1 http://127.0.0.1:8081/; then exit 0; fi; "
          ++ "sleep 0.25; "
          ++ "done; "
          ++ "exit 1"
  (exitCode, _out, err) <- readProcessWithExitCode "/bin/bash" ["-lc", cmd] ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> assertFailure ("Sandbox server did not become ready after restart: " ++ err)

runPsqlFile :: FilePath -> IO (Either String ())
runPsqlFile filePath = do
  (exitCode, _out, err) <- readProcessWithExitCode "psql" ["--dbname", postgresConn, "-v", "ON_ERROR_STOP=1", "-f", filePath] ""
  pure $
    case exitCode of
      ExitSuccess -> Right ()
      ExitFailure _ -> Left err

seedApprovedUser :: String -> [String] -> IO ()
seedApprovedUser username roles = do
  pHash <- hashPassword $ mkPassword (pack testPassword)
  let role = if "admin" `elem` roles then "admin" else "member"
      sql = "INSERT INTO auth_users (username, password_hash, role, approved) VALUES ("
            ++ quoteSql username ++ ", "
            ++ quoteSql (unpack (unPasswordHash pHash)) ++ ", "
            ++ quoteSql role ++ ", true)"
  result <- runPsqlCommand sql
  case result of
    Left err -> assertFailure ("Unable to seed approved user '" ++ username ++ "': " ++ err)
    Right () -> pure ()

seedPendingUser :: String -> IO ()
seedPendingUser username = do
  pHash <- hashPassword $ mkPassword (pack testPassword)
  let sql = "INSERT INTO auth_users (username, password_hash, role, approved) VALUES ("
            ++ quoteSql username ++ ", "
            ++ quoteSql (unpack (unPasswordHash pHash)) ++ ", "
            ++ quoteSql "member" ++ ", false)"
  result <- runPsqlCommand sql
  case result of
    Left err -> assertFailure ("Unable to seed pending user '" ++ username ++ "': " ++ err)
    Right () -> pure ()

quoteSql :: String -> String
quoteSql raw = "'" ++ concatMap escape raw ++ "'"
  where
    escape '\'' = "''"
    escape c = [c]

assertStatusCode :: String -> Int -> Response a -> IO ()
assertStatusCode message expected response =
  assertEqual (message ++ ": unexpected status code") expected (getResponseStatusCode response)

assertMessageResponse :: String -> Response Value -> IO ()
assertMessageResponse expectedMessage response =
  case getResponseBody response of
    Object value ->
      case parseMaybe (.: "message") value of
        Just actualMessage -> assertEqual "Unexpected message response" expectedMessage (actualMessage :: String)
        Nothing -> assertFailure "Expected message field in response body"
    _ -> assertFailure "Expected JSON object response body"

assertSigninProfileResponse :: String -> [String] -> Bool -> Response Value -> IO ()
assertSigninProfileResponse expectedUsername expectedRoles expectedApproved response =
  case getResponseBody response of
    Object value -> do
      case parseMaybe (.: "username") value of
        Just actualUsername -> assertEqual "Unexpected signin profile username" expectedUsername (actualUsername :: String)
        Nothing -> assertFailure "Expected signin profile username"
      case parseMaybe (.: "roles") value of
        Just actualRoles -> assertEqual "Unexpected signin profile roles" expectedRoles (actualRoles :: [String])
        Nothing -> assertFailure "Expected signin profile roles"
      case parseMaybe (.: "approved") value of
        Just actualApproved -> assertEqual "Unexpected signin profile approval flag" expectedApproved (actualApproved :: Bool)
        Nothing -> assertFailure "Expected signin profile approval flag"
    _ -> assertFailure "Expected signin profile JSON object"

assertExpiredSetCookie :: Response ByteString -> IO ()
assertExpiredSetCookie response =
  case BS.unpack <$> getFirstSetCookie response of
    Nothing -> assertFailure "Expected Set-Cookie header"
    Just cookieHeader ->
      assertBool "Expected expired cookie with Max-Age=0" ("Max-Age=0" `isInfixOf` cookieHeader)

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

authPayload :: String -> String -> Value
authPayload username password =
  object
    [ "username" .= username
    , "password" .= password
    ]

pendingSignupValue :: String -> Value
pendingSignupValue username = object ["username" .= username]

adminUserValue :: String -> [String] -> Bool -> Value
adminUserValue username roles approved =
  object
    [ "username" .= username
    , "roles" .= roles
    , "approved" .= approved
    ]

trimTrailingNewline :: String -> String
trimTrailingNewline value =
  case reverse value of
    '\n':rest -> reverse rest
    _ -> value

uniqueSuffix :: IO String
uniqueSuffix = show . round . (* 1000000) <$> getPOSIXTime

postgresConn :: String
postgresConn = "host=127.0.0.1 port=5432 dbname=foucl user=foucl password=foucl"

authUpMigration :: FilePath
authUpMigration = "db/migrations/auth/0001_auth_schema.up.sql"

authDownMigration :: FilePath
authDownMigration = "db/migrations/auth/0001_auth_schema.down.sql"

sessionUpMigration :: FilePath
sessionUpMigration = "db/migrations/session/0001_session_schema.up.sql"

sessionDownMigration :: FilePath
sessionDownMigration = "db/migrations/session/0001_session_schema.down.sql"

calendarUpMigration :: FilePath
calendarUpMigration = "db/migrations/calendar/0001_calendar_schema.up.sql"

calendarDownMigration :: FilePath
calendarDownMigration = "db/migrations/calendar/0001_calendar_schema.down.sql"

tripSharingUpMigration :: FilePath
tripSharingUpMigration = "db/migrations/trip-sharing/0001_trip_sharing_schema.up.sql"

tripSharingDownMigration :: FilePath
tripSharingDownMigration = "db/migrations/trip-sharing/0001_trip_sharing_schema.down.sql"

testPassword :: String
testPassword = "averystrongpass"

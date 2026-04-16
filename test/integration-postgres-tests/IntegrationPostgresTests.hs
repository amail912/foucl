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
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Test.Hspec
import Test.HUnit (assertBool, assertEqual, assertFailure)
import Text.Read (readMaybe)

runIntegrationPostgresTests :: IO ()
runIntegrationPostgresTests = do
  assertPostgresReachable
  hspec $ around_ withFreshPostgresFixtures $ do
    describe "Postgres parity integration" $ do
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

  truncateResult <- runPsqlCommand "TRUNCATE TABLE auth_users"
  case truncateResult of
    Left err -> assertFailure ("Auth table cleanup failed: " ++ err)
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

testPassword :: String
testPassword = "averystrongpass"

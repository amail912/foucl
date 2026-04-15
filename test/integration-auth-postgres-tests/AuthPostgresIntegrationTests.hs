{-# LANGUAGE OverloadedStrings #-}

module AuthPostgresIntegrationTests (runAuthPostgresIntegrationTests) where

import Data.Aeson (Value(..), object, (.:), (.=))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive (original)
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Simple
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Test.Hspec
import Test.HUnit (assertBool, assertEqual, assertFailure)

runAuthPostgresIntegrationTests :: IO ()
runAuthPostgresIntegrationTests = do
  assertPostgresReachable
  hspec $ around_ withFreshAuthSchema $ do
    describe "Auth Postgres parity integration" $ do
      it "keeps signup success/conflict semantics" $ do
        suffix <- uniqueSuffix
        let username = "pg-signup-" ++ suffix
        signupOk <- performSignup username testPassword
        assertStatusCode "Signup should succeed" 200 signupOk

        signupDup <- performSignup username testPassword
        assertStatusCode "Duplicate signup should return bad request" 400 signupDup
        assertMessageResponse "Unable to create user" signupDup

      it "keeps signin success, invalid credentials, and pending approval semantics" $ do
        adminSignin <- bootstrapAndSigninAdmin
        assertStatusCode "Bootstrap admin signin should succeed" 200 adminSignin
        assertSigninProfileResponse "admin" ["admin"] True adminSignin

        invalidSignin <- performSigninJSON "admin" "wrongpassword"
        assertStatusCode "Invalid credentials should return 401" 401 invalidSignin
        assertMessageResponse "Invalid credentials" invalidSignin

        suffix <- uniqueSuffix
        let pendingUsername = "pg-pending-" ++ suffix
        pendingSignup <- performSignup pendingUsername testPassword
        assertStatusCode "Pending signup should succeed" 200 pendingSignup

        pendingSignin <- performSigninJSON pendingUsername testPassword
        assertStatusCode "Pending account should return 403" 403 pendingSignin
        assertMessageResponse "Account pending approval" pendingSignin

      it "keeps auth profile success semantics" $ do
        _ <- bootstrapAndSigninAdmin
        adminCookie <- signinOnly "admin" testPassword

        profileResp <- getAuthProfile adminCookie
        assertStatusCode "Auth profile should succeed" 200 profileResp
        assertSigninProfileResponse "admin" ["admin"] True profileResp

      it "keeps admin pending moderation semantics" $ do
        _ <- bootstrapAndSigninAdmin
        adminCookie <- signinOnly "admin" testPassword

        suffix <- uniqueSuffix
        let pendingUsername = "pg-approvable-" ++ suffix
        pendingSignup <- performSignup pendingUsername testPassword
        assertStatusCode "Pending signup should succeed" 200 pendingSignup

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
        _ <- bootstrapAndSigninAdmin
        adminCookie <- signinOnly "admin" testPassword

        suffix <- uniqueSuffix
        let memberUsername = "pg-approved-" ++ suffix
        pendingSignup <- performSignup memberUsername testPassword
        assertStatusCode "Member signup should succeed" 200 pendingSignup

        approveResp <- approvePendingSignupResponse adminCookie memberUsername
        assertStatusCode "Approve should succeed" 200 approveResp

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
        _ <- bootstrapAndSigninAdmin
        adminCookie <- signinOnly "admin" testPassword

        _ <- runPsqlFile authDownMigration
        profileResp <- getAuthProfile adminCookie
        assertStatusCode "Profile should return technical error when storage fails" 500 profileResp
        assertMessageResponse "Unable to process authentication" profileResp

withFreshAuthSchema :: IO () -> IO ()
withFreshAuthSchema action = do
  resetAuthSchema
  action

assertPostgresReachable :: IO ()
assertPostgresReachable = do
  result <- runPsqlCommand "SELECT 1"
  case result of
    Left err -> assertFailure ("Expected reachable Postgres test database at " ++ authPostgresConn ++ ": " ++ err)
    Right () -> pure ()

resetAuthSchema :: IO ()
resetAuthSchema = do
  downResult <- runPsqlFile authDownMigration
  case downResult of
    Left err -> assertFailure ("Auth down migration failed: " ++ err)
    Right () -> pure ()

  upResult <- runPsqlFile authUpMigration
  case upResult of
    Left err -> assertFailure ("Auth up migration failed: " ++ err)
    Right () -> pure ()

  truncateResult <- runPsqlCommand "TRUNCATE TABLE auth_users"
  case truncateResult of
    Left err -> assertFailure ("Auth table cleanup failed: " ++ err)
    Right () -> pure ()

bootstrapAndSigninAdmin :: IO (Response Value)
bootstrapAndSigninAdmin = do
  signupResp <- performSignup "admin" testPassword
  assertStatusCode "Bootstrap admin signup should succeed" 200 signupResp
  performSigninJSON "admin" testPassword

performSignup :: String -> String -> IO (Response Value)
performSignup username password = do
  req <- parseRequest "POST http://localhost:8081/api/signup"
  httpJSON
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

runPsqlCommand :: String -> IO (Either String ())
runPsqlCommand sqlCommand = do
  (exitCode, _out, err) <- readProcessWithExitCode "psql" ["--dbname", authPostgresConn, "-v", "ON_ERROR_STOP=1", "-c", sqlCommand] ""
  pure $
    case exitCode of
      ExitSuccess -> Right ()
      ExitFailure _ -> Left err

runPsqlFile :: FilePath -> IO (Either String ())
runPsqlFile filePath = do
  (exitCode, _out, err) <- readProcessWithExitCode "psql" ["--dbname", authPostgresConn, "-v", "ON_ERROR_STOP=1", "-f", filePath] ""
  pure $
    case exitCode of
      ExitSuccess -> Right ()
      ExitFailure _ -> Left err

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

uniqueSuffix :: IO String
uniqueSuffix = show . round . (* 1000000) <$> getPOSIXTime

authPostgresConn :: String
authPostgresConn = "host=127.0.0.1 port=5432 dbname=foucl user=foucl password=foucl"

authUpMigration :: FilePath
authUpMigration = "db/migrations/auth/0001_auth_schema.up.sql"

authDownMigration :: FilePath
authDownMigration = "db/migrations/auth/0001_auth_schema.down.sql"

testPassword :: String
testPassword = "averystrongpass"

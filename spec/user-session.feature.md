# User session

## Authentication <a name="authentication"></a>

An authentication process must provide the user with various capabilities

### Create account <a name="authentication-create-account"></a>

As a user, I can create an account by giving a username and a password

1. Account creation <a name="authentication-create-account-account-creation"></a>
2. Username already exists : Should send back a 400 bad request with no other information
3. Password not safe enough : Should send back a 400 with precision on the safety of the password

### Log-in <a name="authentication-log-in"></a>

1. A user should be able to log-in and the username should be sent back
2. Should send 400 when username or password param is not present
3. Should send 401 when user is not found. Reason : "Authentication failed"
4. Should send 401 when the password does not match. Reason : "authentication failed"

### Delete account

1. A user should be able to delete its account
2. Account cannot be deleted when user is not connected. Send back a 401
3. Account cannot be deleted when the user is not the right one. Send back a 401

## Session

1. A cookie containing the user-id should be set after log-in
2. The cookie should have short and long TTL depending on user selection at log-in
3. The cookie should be deleted after its TTL
4. The user can log-out. The cookie should then be deleted
5. Should result in 400 when user is not connected

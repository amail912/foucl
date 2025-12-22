# Features

## [CRUD Note](./crud-note.feature.md#epics)<a name="crudNote"></a>

The goal is to provide the user with a CRUD (Create, Read, Update, Delete) systems for personal notes.
In a first step, notes are simply titled documents.

As a user, I am assured of the CRUD system's _simple operative semantics_ :

1. [Read semantics](./crud-note.feature.md#crudNote-readSemantics): Read is **safe**: it won't cause any observable change (from a user's perpective) in the server state<a name="crudNote-readSemantics"></a>
2. [Update semantincs](./crud-note.feature.md#crudNote-updateSemantics) is **idempotent**:
causing two updates **(without taking concurrency into account)** will leave the server in the same state as if only one update was caused.<a name="crudNote-updateSemantics"></a>
1. [Delete semantics](./crud-note.feature.md#deleteSemantics) is **idempotent**:
causing two deletions **(without taking concurrency into account)** will leave the server in the same state as if only one update was caused.<a name="crudNote-deleteSemantics"></a>

As a user, I am also assured of the CRUD system's _coupled semantics_ :

1. [Create-Read](./crud-note.feature.md#createReadSemantics): Creating then Reading **Notes** gives back created **Notes**<a name="crudNote-createReadSemantics"></a>
2. [Create-Update-Read](./crud-note.feature.md#createUpdateReadSemantics): Creating, Updating and Reading **Notes** gives back updated **Notes** <a name="crudNote-createUpdateReadSemantics"></a>
3. [Create-Delete-Read](./crud-note.feature.md#createDeleteReadSemantics): Creating, Deleting then Reading **Notes** gives back no **Note** <a name="crudNote-createDeleteReadSemantics"></a>

## [Trash](./trash.feature.md#epics)<a name="trash"></a>

The goal is to provide a trash system for **Notes** managed by Sharad.
The trash should have a _server-configurable_ (as opposed to _user-configurable_) **TTL** and **MAX_SIZE**

As a user, I can be assured of

1. The [Trashing](./trash.feature.md#trashing) of deleted **Notes**<a name="trash-trashing"></a>
2. The [Access](./trash.feature.md#access) to __trashed__ **Notes**:<a name="trash-access"></a>
3. The [Deletion](./trash.feature.md#deletion) of __trashed__ **Notes**<a name="trash-deletion"></a>
4. The [Restoration](./trash.feature.md#restoration) of __trahed__ **Notes**<a name="trash-restoration"></a>

As an Sharad administrator, I can be assured of

5. The [trash time-safety](./trash.feature.md#timeSafety)<a name="trash-timeSafety"></a>
6. The [trash space-safety](./trash.feature.md#spaceSafety)<a name="trash-spaceSafety"></a>
7. The [Server Configurable](./trash.feature.md#serverConfigurable) aspects of the trash<a name="trash-serverConfigurable"></a>

## [User session](./user-session.feature.md)<a name="user-session"></a>

The goal is to provide a user session. This epic is divided into sub-tasks:

### [Authentication](./user-session.feature.md#authentication)

The authentication lifecycle is as follows:

As a user, I can:

1. [Subscribe](./user-session.feature.md) via username and password
2. [Log-in] via username and password
3. The user can delete its login information (username and password)

There are technical requirements:

1. Password should not be stored. We store the hash of the password instead
2. Session should be safe. It cannot be forged.
3. The session has a limited lifetime decided by the server

# [Trash](./summary.md#trash) Epics

## [Trashing](./summary.md#trash-trashing)<a name="trashing"></a>

The goal is to ensure that deleted **Notes** end up in the trash

As a user, I can

1. [Do Trash](#stories-trash-trashing-doTrash) a **Note**

## [Access](./summary.md#trash-access) <a name="access"></a>

The goal is to provide easy access to trashed items for the user

As a user, I can

1. GET __trashed__ **Notes** [only](#stories-trash-access-only)<a name="access-only"></a>
2. GET __trashed__ **Notes** [along with](#stories-trash-access-withFresh) __fresh__ **Notes**<a name="access-withFresh"></a>

## [Deletion](./summary.md#trash-deletion)<a name="deletion"></a>

The goal is to provide the user with the ability to force-delete a trashed item. A forcibly deleted __trashed__ **Note** is not recoverable.

As a user, I can

1. [DELETE](#stories-trash-deletion-delete) a __trashed__ **Note**<a name="deletion-delete"></a>

## [Restoration](./summary.md#trash-restoration)<a name="restoration"></a>

The goal is to provide the user with a way of restoring any __trashed__ **Note**

As a user, I can

1. [Restore](#stories-trash-restoration-restore) a __trashed__ **Note**<a name="restoration-restore"></a>

## [trash-timeSafety](./summary.md#trash-timeSafety)<a name="timeSafety"></a>

The goal is to ensure that the trash doesn't keep __trashed__ **Notes** forever.

As an andministrator, I can be assured of

1. The [Youngness](#stories-trash-timeSafety-youngness) of __trashed__ **Notes**<a name="timeSafety-youngness"></a>
2. The [Oldness](#stories-trash-timeSafety-oldness) of the __trashed__ **Notes**

## [trash-spaceSafety](./summary.md#trash-spaceSafety)<a name="spaceSafety"></a>

The goal is to ensure that the trash doesn't keep an infinite volume of __trashed__ **Notes**

As an andministrator, I can be assured of

1. The [Smallness](#stories-trash-spaceSafety-smallness) of __trashed__ **Notes** store<a name="spaceSafety-smallness"></a>

## [Server Configurable](./summary.md#trash-serverConfigurable)<a name="serverConfigurable"></a>

The goal is to be able to parameterize the **TTL** of the trash as well as its **MAX_SIZE**

As an administrator, I can

1. [Configure the TTL](#stories-trash-serverConfigurable-ttl) via **server configuration**<a name="serverConfigurable-ttl"></a>
2. [Configure the MAX_SIZE](#stories-trash-serverConfigurable-maxSize) via **server configuration** <a name="serverConfigurable-maxSize"></a>

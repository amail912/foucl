# Session technique (foucl)

## Vue d'ensemble

Le serveur implémente une session cookie signée avec état côté serveur (store fichier).

- Le cookie transporte un `sessionId` signé (`sessionId.signature`).
- La signature est un **HMAC-SHA256** basé sur `FOUCL_SESSION_SECRET`.
- Le store côté serveur contient:
  - des **handles** de session (`sessionId -> stateId`),
  - des **states** de session partagés entre devices,
  - un binding utilisateur vers `stateId`.

## Configuration runtime

### Fichier de configuration

Par défaut: `config/app-config.json` (surchargé par `FOUCL_CONFIG_FILE`).

```json
{
  "session": {
    "cookieName": "foucl_session",
    "absoluteTtlSeconds": 604800,
    "idleTtlSeconds": 86400
  }
}
```

### Secret

Le secret n'est **pas** lu depuis le fichier: il est lu depuis la variable d'environnement `FOUCL_SESSION_SECRET`.

- absent => échec de startup,
- vide => échec de startup.

## Auth pipeline

- `POST /api/signin`:
  1. valide les credentials,
  2. crée un handle de session,
  3. pose un cookie `Secure`, `HttpOnly`, `SameSite=Lax`.

- `requireAuth`:
  1. lit le cookie,
  2. vérifie la signature,
  3. résout la session dans le store,
  4. injecte un `AppContext` avec `SessionPrincipal` vers le handler.

## Logout

`POST /api/signout`

- sans paramètre: révoque uniquement la session courante (`sessionId`).
- avec `?all=true`: révoque toutes les sessions partageant le même `stateId` (logout multi-device).

Dans les deux cas, le serveur renvoie aussi un cookie expiré (`Max-Age=0`).

## TTL et sliding renewal

Chaque state porte:

- `expiresAt` (TTL absolu),
- `idleExpiresAt` (TTL idle).

À chaque requête authentifiée valide, `idleExpiresAt` est prolongé (sliding) sans dépasser `expiresAt`.

## Limites connues

- store fichier: simple et robuste pour démarrer, mais parcours de fichiers pour certaines opérations (`all=true`).
- pour montée en charge, prévoir backend dédié (Redis/SQL) et index plus efficaces.


## Exploitation

Voir aussi le runbook d'exploitation: `docs/session-runbook.md`.

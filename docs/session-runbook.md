# Runbook session (exploitation)

## Variables et configuration

- `FOUCL_SESSION_SECRET` (**obligatoire**): secret de signature HMAC des cookies.
- `FOUCL_CONFIG_FILE` (optionnel): chemin du fichier de config (défaut: `config/app-config.json`).

Paramètres sessions dans le fichier:

- `session.cookieName`
- `session.absoluteTtlSeconds`
- `session.idleTtlSeconds`

## Démarrage

1. Définir `FOUCL_SESSION_SECRET` avec une valeur forte (longue, aléatoire).
2. Démarrer le serveur.
3. Vérifier qu'aucun log `[startup-error]` n'est présent.

## Rotation du secret

Effet important: changer `FOUCL_SESSION_SECRET` invalide les cookies signés existants.

Procédure recommandée:

1. Planifier une fenêtre de maintenance légère.
2. Déployer le nouveau secret.
3. Redémarrer le service.
4. Vérifier le taux de `401` et l'augmentation attendue de reconnexions.

## Incident / révocation large

En cas de suspicion de compromission de cookie:

1. Rotater immédiatement `FOUCL_SESSION_SECRET`.
2. Redémarrer l'application.
3. Communiquer une reconnexion utilisateur.

Pour une révocation ciblée:

- utiliser `POST /api/signout` pour la session courante,
- utiliser `POST /api/signout?all=true` pour toutes les sessions d'un même state utilisateur.

## Vérifications post-déploiement

- Signin nominal: cookie posé avec attributs attendus.
- Endpoint protégé sans cookie: `401`.
- Signout courant: session actuelle invalide.
- Signout `all=true`: sessions sœurs invalidées.

## Observabilité minimale (recommandée)

- Compter les `401` (auth invalide / session expirée).
- Compter les signins/signouts.
- Journaliser les erreurs de chargement config au startup.

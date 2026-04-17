.PHONY: help lint build test ci start-sandbox restart-sandbox stop-sandbox integration-test integration-test-postgres integration-test-auth-postgres _prepare-sandbox _wait-server _prepare-postgres-test-db _write-postgres-config _write-auth-startup-import-fixtures _write-session-startup-import-fixtures _start-postgres-test-db _stop-postgres-test-db

SHELL := /bin/bash

DAEMON_SCRIPT := $(CURDIR)/scripts/daemon/foucld
SANDBOX_DIR := ./dist-newstyle/sandbox/foucl
SANDBOX_EXE := $(SANDBOX_DIR)/foucl
SANDBOX_PIDFILE := $(abspath $(SANDBOX_DIR)/.foucl/foucl.pid)
FOUCL_CONFIG_FILE_DEFAULT := $(CURDIR)/config/app-config.json
AUTH_PG_TEST_HOST := 127.0.0.1
AUTH_PG_TEST_PORT := 5432
AUTH_PG_TEST_DB := foucl
AUTH_PG_TEST_USER := foucl
AUTH_PG_TEST_PASSWORD := foucl
AUTH_PG_TEST_CONN := host=$(AUTH_PG_TEST_HOST) port=$(AUTH_PG_TEST_PORT) dbname=$(AUTH_PG_TEST_DB) user=$(AUTH_PG_TEST_USER) password=$(AUTH_PG_TEST_PASSWORD)
SANDBOX_AUTH_PG_CONFIG := $(abspath $(SANDBOX_DIR)/config/app-config.auth-postgres.json)
AUTH_PG_COMPOSE_FILE := $(CURDIR)/docker-compose.auth-postgres-tests.yml

help:
	@echo "Available targets:"
	@echo "  make lint             Run Haskell linter (hlint)"
	@echo "  make build            Build executable"
	@echo "  make test             Run unit tests"
	@echo "  make ci               Run build + unit + integration tests"
	@echo "  make start-sandbox    Build and start sandbox server"
	@echo "  make restart-sandbox  Restart sandbox server"
	@echo "  make stop-sandbox     Stop sandbox server"
	@echo "  make integration-test Run integration tests against sandbox server"
	@echo "  make integration-test-postgres Run postgres parity integration tests (auth + session + calendar + trip-sharing) against postgres backends"

lint:
	./scripts/lint.sh

build:
	cabal build exe:foucl

test:
	cabal test foucl-unit-tests

ci:
	-$(MAKE) --no-print-directory lint
	$(MAKE) --no-print-directory build
	$(MAKE) --no-print-directory test
	$(MAKE) --no-print-directory integration-test

_prepare-sandbox:
	@set -euo pipefail; \
	cabal build exe:foucl >/dev/null; \
	built_exe="$$(cabal list-bin exe:foucl | tail -n 1)"; \
	rm -rf "$(SANDBOX_DIR)"; \
	mkdir -p "$(SANDBOX_DIR)/data/note" "$(SANDBOX_DIR)/data/checklist" "$(SANDBOX_DIR)/data/users" "$(SANDBOX_DIR)/data/calendar-items"; \
	cp "$$built_exe" "$(SANDBOX_EXE)"; \
	chmod +x "$(SANDBOX_EXE)"

_prepare-postgres-test-db:
	@set -euo pipefail; \
	psql --dbname "$(AUTH_PG_TEST_CONN)" -v ON_ERROR_STOP=1 -f "$(CURDIR)/db/migrations/trip-sharing/0001_trip_sharing_schema.down.sql"; \
	psql --dbname "$(AUTH_PG_TEST_CONN)" -v ON_ERROR_STOP=1 -f "$(CURDIR)/db/migrations/calendar/0001_calendar_schema.down.sql"; \
	psql --dbname "$(AUTH_PG_TEST_CONN)" -v ON_ERROR_STOP=1 -f "$(CURDIR)/db/migrations/session/0001_session_schema.down.sql"; \
	psql --dbname "$(AUTH_PG_TEST_CONN)" -v ON_ERROR_STOP=1 -f "$(CURDIR)/db/migrations/auth/0001_auth_schema.down.sql"; \
	psql --dbname "$(AUTH_PG_TEST_CONN)" -v ON_ERROR_STOP=1 -f "$(CURDIR)/db/migrations/auth/0001_auth_schema.up.sql"; \
	psql --dbname "$(AUTH_PG_TEST_CONN)" -v ON_ERROR_STOP=1 -f "$(CURDIR)/db/migrations/session/0001_session_schema.up.sql"; \
	psql --dbname "$(AUTH_PG_TEST_CONN)" -v ON_ERROR_STOP=1 -f "$(CURDIR)/db/migrations/calendar/0001_calendar_schema.up.sql"; \
	psql --dbname "$(AUTH_PG_TEST_CONN)" -v ON_ERROR_STOP=1 -f "$(CURDIR)/db/migrations/trip-sharing/0001_trip_sharing_schema.up.sql"; \
	psql --dbname "$(AUTH_PG_TEST_CONN)" -v ON_ERROR_STOP=1 -c "TRUNCATE TABLE auth_users, session_handles, session_user_bindings, session_states, calendar_items, trip_shares, trip_subscriptions"; \
	psql --dbname "$(AUTH_PG_TEST_CONN)" -v ON_ERROR_STOP=1 -c "INSERT INTO auth_users (username, password_hash, role, approved) VALUES ('startup-conflict-user', 'postgres-conflict-hash', 'admin'::auth_user_role, true), ('startup-postgres-only-user', 'postgres-only-hash', 'member'::auth_user_role, false)";
	psql --dbname "$(AUTH_PG_TEST_CONN)" -v ON_ERROR_STOP=1 -c "INSERT INTO session_states (state_id, user_id, created_at, expires_at, idle_expires_at, revoked_at) VALUES ('11111111-1111-1111-1111-111111111111'::uuid, 'startup-pg-conflict-user', '2025-01-01T00:00:00Z'::timestamptz, '2030-01-01T00:00:00Z'::timestamptz, '2030-01-01T01:00:00Z'::timestamptz, NULL), ('22222222-2222-2222-2222-222222222222'::uuid, 'startup-pg-only-user', '2025-01-01T00:00:00Z'::timestamptz, '2030-01-01T00:00:00Z'::timestamptz, '2030-01-01T01:00:00Z'::timestamptz, NULL)"; \
	psql --dbname "$(AUTH_PG_TEST_CONN)" -v ON_ERROR_STOP=1 -c "INSERT INTO session_handles (session_id, state_id, issued_at, revoked_at) VALUES ('aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa'::uuid, '11111111-1111-1111-1111-111111111111'::uuid, '2025-01-01T00:00:00Z'::timestamptz, NULL), ('bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb'::uuid, '22222222-2222-2222-2222-222222222222'::uuid, '2025-01-01T00:00:00Z'::timestamptz, NULL)"; \
	psql --dbname "$(AUTH_PG_TEST_CONN)" -v ON_ERROR_STOP=1 -c "INSERT INTO session_user_bindings (user_id, state_id) VALUES ('startup-binding-conflict-user', '11111111-1111-1111-1111-111111111111'::uuid), ('startup-pg-only-user', '22222222-2222-2222-2222-222222222222'::uuid)";

_start-postgres-test-db:
	@set -euo pipefail; \
	docker compose -f "$(AUTH_PG_COMPOSE_FILE)" up -d --wait

_stop-postgres-test-db:
	@set -euo pipefail; \
	docker compose -f "$(AUTH_PG_COMPOSE_FILE)" down -v --remove-orphans

_write-postgres-config:
	@set -euo pipefail; \
	mkdir -p "$(SANDBOX_DIR)/config"; \
	printf '%s\n' \
	'{' \
	'  "auth": {' \
	'    "bootstrapAdminUsername": "admin",' \
	'    "authBackend": "postgres"' \
	'  },' \
	'  "session": {' \
	'    "cookieName": "foucl_session",' \
	'    "absoluteTtlSeconds": 604800,' \
	'    "idleTtlSeconds": 86400,' \
	'    "sessionBackend": "postgres"' \
	'  },' \
	'  "calendarBackend": "postgres",' \
	'  "tripSharingBackend": "postgres",' \
	'  "database": {' \
	'    "host": "127.0.0.1",' \
	'    "port": 5432,' \
	'    "name": "foucl",' \
	'    "user": "foucl",' \
	'    "password": "foucl"' \
	'  }' \
	'}' > "$(SANDBOX_AUTH_PG_CONFIG)"

_write-auth-startup-import-fixtures:
	@set -euo pipefail; \
	mkdir -p "$(SANDBOX_DIR)/data/users/startup-fs-only-user" "$(SANDBOX_DIR)/data/users/startup-conflict-user"; \
	printf '%s\n' \
	'{' \
	'  "uname": "startup-fs-only-user",' \
	'  "passwordHash": "fs-only-hash",' \
	'  "role": "member",' \
	'  "approvalStatus": "pending"' \
	'}' > "$(SANDBOX_DIR)/data/users/startup-fs-only-user/profile.json"; \
	printf '%s\n' \
	'{' \
	'  "uname": "startup-conflict-user",' \
	'  "passwordHash": "fs-conflict-hash",' \
	'  "role": "member",' \
	'  "approvalStatus": "pending"' \
	'}' > "$(SANDBOX_DIR)/data/users/startup-conflict-user/profile.json"

_write-session-startup-import-fixtures:
	@set -euo pipefail; \
	mkdir -p "$(SANDBOX_DIR)/data/sessions/states" "$(SANDBOX_DIR)/data/sessions/handles" "$(SANDBOX_DIR)/data/sessions/users"; \
	printf '%s\n' \
	'{' \
	'  "stateId": "33333333-3333-3333-3333-333333333333",' \
	'  "stateUserId": "startup-fs-only-session-user",' \
	'  "stateCreatedAt": "2025-01-02T00:00:00Z",' \
	'  "stateExpiresAt": "2030-01-02T00:00:00Z",' \
	'  "stateIdleExpiresAt": "2030-01-02T01:00:00Z",' \
	'  "stateRevokedAt": null' \
	'}' > "$(SANDBOX_DIR)/data/sessions/states/33333333-3333-3333-3333-333333333333.json"; \
	printf '%s\n' \
	'{' \
	'  "stateId": "11111111-1111-1111-1111-111111111111",' \
	'  "stateUserId": "startup-fs-conflicting-user",' \
	'  "stateCreatedAt": "2025-01-02T00:00:00Z",' \
	'  "stateExpiresAt": "2030-01-02T00:00:00Z",' \
	'  "stateIdleExpiresAt": "2030-01-02T01:00:00Z",' \
	'  "stateRevokedAt": null' \
	'}' > "$(SANDBOX_DIR)/data/sessions/states/11111111-1111-1111-1111-111111111111.json"; \
	printf '%s\n' \
	'{' \
	'  "handleSessionId": "cccccccc-cccc-cccc-cccc-cccccccccccc",' \
	'  "handleStateId": "33333333-3333-3333-3333-333333333333",' \
	'  "handleIssuedAt": "2025-01-02T00:00:00Z",' \
	'  "handleRevokedAt": null' \
	'}' > "$(SANDBOX_DIR)/data/sessions/handles/cccccccc-cccc-cccc-cccc-cccccccccccc.json"; \
	printf '%s\n' \
	'{' \
	'  "handleSessionId": "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa",' \
	'  "handleStateId": "33333333-3333-3333-3333-333333333333",' \
	'  "handleIssuedAt": "2025-01-02T00:00:00Z",' \
	'  "handleRevokedAt": null' \
	'}' > "$(SANDBOX_DIR)/data/sessions/handles/aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa.json"; \
	printf '%s\n' \
	'{' \
	'  "boundStateId": "33333333-3333-3333-3333-333333333333"' \
	'}' > "$(SANDBOX_DIR)/data/sessions/users/startup-fs-only-session-user.json"; \
	printf '%s\n' \
	'{' \
	'  "boundStateId": "33333333-3333-3333-3333-333333333333"' \
	'}' > "$(SANDBOX_DIR)/data/sessions/users/startup-binding-conflict-user.json"

_wait-server:
	@set -euo pipefail; \
	for i in $$(seq 1 40); do \
		if curl --silent --show-error --output /dev/null --max-time 1 "http://127.0.0.1:8081/"; then \
			echo "Server is ready"; \
			exit 0; \
		fi; \
		sleep 0.25; \
	done; \
	echo "Server did not become ready on localhost:8081" >&2; \
	exit 1

start-sandbox:
	@set -euo pipefail; \
	$(MAKE) --no-print-directory _prepare-sandbox; \
	$(DAEMON_SCRIPT) stop --pidfile "$(SANDBOX_PIDFILE)" >/dev/null 2>&1 || true; \
	( \
		export FOUCL_SESSION_SECRET="$${FOUCL_SESSION_SECRET:-dev-only-session-secret}"; \
		export FOUCL_CONFIG_FILE="$${FOUCL_CONFIG_FILE:-$(FOUCL_CONFIG_FILE_DEFAULT)}"; \
		export FOUCL_SESSION_COOKIE_SECURE="$${FOUCL_SESSION_COOKIE_SECURE:-false}"; \
		cd "$(SANDBOX_DIR)"; \
		$(DAEMON_SCRIPT) start --bin "$(abspath $(SANDBOX_EXE))" --pidfile "$(abspath $(SANDBOX_PIDFILE))"; \
	); \
	$(MAKE) --no-print-directory _wait-server

restart-sandbox:
	@set -euo pipefail; \
	$(MAKE) --no-print-directory stop-sandbox; \
	$(MAKE) --no-print-directory start-sandbox

stop-sandbox:
	$(DAEMON_SCRIPT) stop --pidfile $(SANDBOX_PIDFILE)

integration-test:
	@set -euo pipefail; \
	$(MAKE) --no-print-directory _prepare-sandbox; \
	trap '$(DAEMON_SCRIPT) stop --pidfile "$(SANDBOX_PIDFILE)" >/dev/null 2>&1 || true' EXIT INT TERM; \
	export FOUCL_SESSION_COOKIE_SECURE="$${FOUCL_SESSION_COOKIE_SECURE:-false}"; \
	( \
		export FOUCL_SESSION_SECRET="$${FOUCL_SESSION_SECRET:-dev-only-session-secret}"; \
		export FOUCL_CONFIG_FILE="$${FOUCL_CONFIG_FILE:-$(FOUCL_CONFIG_FILE_DEFAULT)}"; \
		export FOUCL_SESSION_COOKIE_SECURE="$$FOUCL_SESSION_COOKIE_SECURE"; \
		cd "$(SANDBOX_DIR)"; \
		$(DAEMON_SCRIPT) start --bin "$(abspath $(SANDBOX_EXE))" --pidfile "$(abspath $(SANDBOX_PIDFILE))"; \
	); \
	$(MAKE) --no-print-directory _wait-server; \
	cabal test foucl-integration-tests

integration-test-postgres:
	@set -euo pipefail; \
	$(MAKE) --no-print-directory _prepare-sandbox; \
	$(MAKE) --no-print-directory _start-postgres-test-db; \
	$(MAKE) --no-print-directory _prepare-postgres-test-db; \
	$(MAKE) --no-print-directory _write-postgres-config; \
	$(MAKE) --no-print-directory _write-auth-startup-import-fixtures; \
	$(MAKE) --no-print-directory _write-session-startup-import-fixtures; \
	trap '$(DAEMON_SCRIPT) stop --pidfile "$(SANDBOX_PIDFILE)" >/dev/null 2>&1 || true; $(MAKE) --no-print-directory _stop-postgres-test-db >/dev/null 2>&1 || true' EXIT INT TERM; \
	export FOUCL_SESSION_COOKIE_SECURE="$${FOUCL_SESSION_COOKIE_SECURE:-false}"; \
	( \
		export FOUCL_SESSION_SECRET="$${FOUCL_SESSION_SECRET:-dev-only-session-secret}"; \
		export FOUCL_CONFIG_FILE="$(SANDBOX_AUTH_PG_CONFIG)"; \
		export FOUCL_SESSION_COOKIE_SECURE="$$FOUCL_SESSION_COOKIE_SECURE"; \
		cd "$(SANDBOX_DIR)"; \
		$(DAEMON_SCRIPT) start --bin "$(abspath $(SANDBOX_EXE))" --pidfile "$(abspath $(SANDBOX_PIDFILE))"; \
	); \
	$(MAKE) --no-print-directory _wait-server; \
	cabal test foucl-integration-postgres-tests

integration-test-auth-postgres: integration-test-postgres

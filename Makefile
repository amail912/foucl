.PHONY: help lint build test ci start-sandbox restart-sandbox stop-sandbox integration-test integration-test-postgres integration-test-auth-postgres _prepare-sandbox _wait-server _prepare-postgres-test-db _write-postgres-config _start-postgres-test-db _stop-postgres-test-db

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
	@echo "  make integration-test-postgres Run postgres parity integration tests (auth + session + calendar + trip-sharing + notes + checklists) against postgres backends"

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
	cp -R "$(CURDIR)/db" "$(SANDBOX_DIR)/db"; \
	cp "$$built_exe" "$(SANDBOX_EXE)"; \
	chmod +x "$(SANDBOX_EXE)"

_prepare-postgres-test-db:
	@set -euo pipefail; \
	psql --dbname "$(AUTH_PG_TEST_CONN)" -v ON_ERROR_STOP=1 -f "$(CURDIR)/db/migrations/checklist/0001_checklist_schema.down.sql"; \
	psql --dbname "$(AUTH_PG_TEST_CONN)" -v ON_ERROR_STOP=1 -f "$(CURDIR)/db/migrations/note/0001_note_schema.down.sql"; \
	psql --dbname "$(AUTH_PG_TEST_CONN)" -v ON_ERROR_STOP=1 -f "$(CURDIR)/db/migrations/trip-sharing/0001_trip_sharing_schema.down.sql"; \
	psql --dbname "$(AUTH_PG_TEST_CONN)" -v ON_ERROR_STOP=1 -f "$(CURDIR)/db/migrations/calendar/0001_calendar_schema.down.sql"; \
	psql --dbname "$(AUTH_PG_TEST_CONN)" -v ON_ERROR_STOP=1 -f "$(CURDIR)/db/migrations/session/0001_session_schema.down.sql"; \
	psql --dbname "$(AUTH_PG_TEST_CONN)" -v ON_ERROR_STOP=1 -f "$(CURDIR)/db/migrations/auth/0001_auth_schema.down.sql"; \
	psql --dbname "$(AUTH_PG_TEST_CONN)" -v ON_ERROR_STOP=1 -c "DROP TABLE IF EXISTS schema_migrations";

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
	'  "noteBackend": "postgres",' \
	'  "checklistBackend": "postgres",' \
	'  "database": {' \
	'    "host": "127.0.0.1",' \
	'    "port": 5432,' \
	'    "name": "foucl",' \
	'    "user": "foucl",' \
	'    "password": "foucl"' \
	'  }' \
	'}' > "$(SANDBOX_AUTH_PG_CONFIG)"

_wait-server:
	@set -euo pipefail; \
	for i in $$(seq 1 40); do \
		if curl --silent --show-error --output /dev/null --max-time 1 "http://127.0.0.1:8081/"; then \
			echo "Server is ready"; \
			exit 0; \
		fi; \
		sleep 1; \
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

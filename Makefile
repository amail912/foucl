.PHONY: help lint build test ci start-sandbox restart-sandbox stop-sandbox integration-test _prepare-sandbox _wait-server

SHELL := /bin/bash

DAEMON_SCRIPT := $(CURDIR)/scripts/daemon/foucld
SANDBOX_DIR := ./dist-newstyle/sandbox/foucl
SANDBOX_EXE := $(SANDBOX_DIR)/foucl
SANDBOX_PIDFILE := $(abspath $(SANDBOX_DIR)/.foucl/foucl.pid)
FOUCL_CONFIG_FILE_DEFAULT := $(CURDIR)/config/app-config.json

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
	( \
		export FOUCL_SESSION_SECRET="$${FOUCL_SESSION_SECRET:-dev-only-session-secret}"; \
		export FOUCL_CONFIG_FILE="$${FOUCL_CONFIG_FILE:-$(FOUCL_CONFIG_FILE_DEFAULT)}"; \
		export FOUCL_SESSION_COOKIE_SECURE="$${FOUCL_SESSION_COOKIE_SECURE:-false}"; \
		cd "$(SANDBOX_DIR)"; \
		$(DAEMON_SCRIPT) start --bin "$(abspath $(SANDBOX_EXE))" --pidfile "$(abspath $(SANDBOX_PIDFILE))"; \
	); \
	$(MAKE) --no-print-directory _wait-server; \
	cabal test foucl-integration-tests

.PHONY: all
all: backend frontend

.PHONY: backend
backend: backend/Version.hs
	cabal new-build

.PHONY: frontend
frontend: assets/app.js assets/app.css

.PHONY: clean
clean:
	rm assets/app.js || true
	rm -R elm-stuff || true

.PHONY: deep-clean
deep-clean: clean
	rm -R dist || true

.PHONY: debug
debug:
	mkdir -p $(@D) && elm make frontend/app/src/Main.elm --debug --output assets/app.js

assets/app.js: frontend/app/src/** frontend/shared/src/**
	mkdir -p $(@D) && elm make frontend/app/src/Main.elm --output $@

assets/app.css: assets/css/**
	cat assets/css/my3ml.css assets/css/navbar.css assets/css/spinner.css > assets/app.css

frontend/shared/src/Api.elm: code-generator/*.hs api/*.hs api/**/*.hs
	mkdir -p $(@D) && cabal new-run code-generator

VERSION_FILE=backend/Version.hs

backend/Version.hs: .git/refs/heads/*
	printf "{-# LANGUAGE OverloadedStrings #-}\nmodule Version where\nimport Data.Text(Text)\n\nrevision :: Text\nrevision = \"$(shell git rev-parse HEAD)\"\n\nversion :: Text\nversion = \"$(shell git describe --always)\"" > $(VERSION_FILE)

.PHONY: serve
serve:
	cabal new-run backend

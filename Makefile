.PHONY: all
all: backend frontend admin

.PHONY: backend
backend: backend/Version.hs
	cabal new-build

.PHONY: frontend
frontend: app assets/app.css

.PHONY: app
app: frontend/shared/src/Api.elm
	$(MAKE) -C frontend/app
	cp frontend/app/output/app.js assets/app.js

.PHONY: admin
admin: frontend/shared/src/Api.elm
	$(MAKE) -C frontend/admin
	cp frontend/admin/output/admin.js assets/admin.js

.PHONY: clean
clean:
	$(MAKE) -C frontend/app clean
	$(MAKE) -C frontend/admin clean

.PHONY: deep-clean
deep-clean: clean
	rm -R dist-newstyle || true


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

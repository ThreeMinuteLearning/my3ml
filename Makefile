.PHONY: all
all: backend frontend

.PHONY: backend
backend:
	stack build

.PHONY: frontend
frontend: assets/app.js

.PHONY: debug
debug:
	mkdir -p $(@D) && elm-make frontend/src/Main.elm --debug --output assets/app.js

assets/app.js: frontend/src/**
	mkdir -p $(@D) && elm-make frontend/src/Main.elm --output $@

frontend/src/Api.elm: code-generator/*.hs api/*.hs api/**/*.hs
	mkdir -p $(@D) && stack exec code-generator

.PHONY: serve
serve:
	stack exec backend

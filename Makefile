all: backend frontend

.PHONY: backend
backend:
	stack build

.PHONY: frontend
frontend: assets/app.js

assets/app.js: frontend/src/*.elm frontend/src/*.elm
	mkdir -p $(@D) && elm-make frontend/src/Main.elm --output $@

frontend/src/Api.elm: code-generator/*.hs api/**/*.hs backend
	mkdir -p $(@D) && stack exec code-generator

.PHONY: serve
serve:
	stack exec backend

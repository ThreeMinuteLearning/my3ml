.PHONY: all
all: backend frontend

.PHONY: backend
backend:
	stack build

.PHONY: frontend
frontend: assets/app.js assets/app.css

.PHONY: clean
clean:
	rm assets/app.js
	rm -R elm-stuff/build-artifacts/0.18.0/ThreeMinuteLearning

.PHONY: deep-clean
deep-clean: clean
	rm -R .stack-work

.PHONY: debug
debug:
	mkdir -p $(@D) && elm-make frontend/src/Main.elm --debug --output assets/app.js

assets/app.js: frontend/src/** frontend/src/Api.elm
	mkdir -p $(@D) && elm-make frontend/src/Main.elm --output $@

assets/app.css: assets/css/**
	cat assets/css/my3ml.css assets/css/navbar.css assets/css/multiselect.css assets/css/spinner.css > assets/app.css

frontend/src/Api.elm: code-generator/*.hs api/*.hs api/**/*.hs
	mkdir -p $(@D) && stack exec code-generator

.PHONY: serve
serve:
	stack exec backend

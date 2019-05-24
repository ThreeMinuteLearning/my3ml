
.PHONY: all
all: version backend frontend admin

.PHONY: backend
backend: version
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

.PHONY: compress
compress: frontend
	uglifyjs frontend/app/output/app.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=assets/app.js
	cp assets/app.css tmp.css
	uglifycss tmp.css > assets/app.css
	rm tmp.css

.PHONY: package
package: compress
	rm app.tar.bz2 || true
	cd assets && tar -cjf ../app.tar.bz2 *.js *.css *.html

assets/app.css: assets/css/** tailwind.config.js
	./node_modules/.bin/tailwind build assets/css/my3ml.css -c tailwind.config.js -o assets/app.css
	cat assets/css/spinner.css >> assets/app.css

frontend/shared/src/Api.elm: code-generator/*.hs api/*.hs api/**/*.hs
	mkdir -p $(@D) && cabal new-run code-generator

VERSION_FILE=backend/Version.hs
current_revision:=$(shell git rev-parse HEAD)
current_version:=$(shell git describe --always)
version_up_to_date:=$(shell grep -c $(current_revision) backend/Version.hs)

.PHONY: version
version:
  ifneq ($(version_up_to_date), 1)
		printf "{-# LANGUAGE OverloadedStrings #-}\nmodule Version where\nimport Data.Text(Text)\n\nrevision :: Text\n" > $(VERSION_FILE)
		printf "revision = \"$(current_revision)\"\n\nversion :: Text\nversion = \"$(current_version)\"" >> $(VERSION_FILE)
  endif

.PHONY: serve
serve:
	cabal new-run backend

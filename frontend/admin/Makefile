.PHONY: all
all: output/admin.js

.PHONY: clean
clean:
	@rm -r output || true
	@rm -r elm-stuff || true

output/admin.js: src/Admin.elm src/** ../shared/src/**
	mkdir -p $(@D) && elm make --output=$@ --optimize $<


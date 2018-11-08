.PHONY: all
all: output/app.js

.PHONY: clean
clean:
	@rm -r output || true
	@rm -r elm-stuff || true

output/app.js: src/Main.elm src/** ../shared/src/**
	mkdir -p $(@D) && elm make --output=$@ --optimize $<


.DEFAULT_GOAL := run

.PHONY: release
release:
	dune build --profile release

.PHONY: install
install: release
	install -Dm755 _build/default/bin/main.exe $(HOME)/.local/bin/epsql

.PHONY: clean
clean:
	rm -f $(HOME)/.local/bin/epsql

.PHONY: build
build:
	dune build

.PHONY: run
run:
	dune exec epsql -- $(filter-out $@,$(MAKECMDGOALS))

%:
	@:

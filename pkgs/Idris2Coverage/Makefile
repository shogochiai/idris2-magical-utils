# idris2-coverage Makefile

.PHONY: build test clean install

build:
	idris2 --build idris2-coverage.ipkg

# Run tests via idris2 executable
test: build
	./build/exec/idris2-cov

# Run actual tests via pack (slower but complete)
test-full:
	pack --cg chez run idris2-coverage.ipkg --exec main src/Coverage/Tests/AllTests.idr

clean:
	rm -rf build

install: build
	mkdir -p ~/.local/bin
	ln -sf $(PWD)/build/exec/idris2-cov ~/.local/bin/idris2-cov
	@echo "Installed: ~/.local/bin/idris2-cov"
	@echo "Make sure ~/.local/bin is in your PATH"

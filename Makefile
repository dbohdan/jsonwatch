BUILD_USER ?= $(USER)
USER_TEMP ?= /tmp/$(BUILD_USER)
PROJECT_TEMP ?= $(USER_TEMP)/cargo/jsonwatch
TARGET ?= x86_64-unknown-linux-musl
BUILD_OPTS ?= --target $(TARGET)
BUILD_OPTS_WITH_DIR ?= $(BUILD_OPTS) --target-dir $(PROJECT_TEMP)
DEV_COMMAND ?= "cat tests/weather1.json"

dev: temp-dir
	# A workaround for https://github.com/rust-lang/rust/issues/46981
	find Cargo.* src/ tests/ | entr -r sh -c 'cargo run $(BUILD_OPTS_WITH_DIR) -- -c $(DEV_COMMAND) -n 1 < /dev/null'

debug: temp-dir
	cargo build $(BUILD_OPTS_WITH_DIR)

install:
	install $(PROJECT_TEMP)/$(TARGET)/release/jsonwatch /usr/local/bin
	strip /usr/local/bin/jsonwatch

release: temp-dir
	cargo build $(BUILD_OPTS_WITH_DIR) --release

temp-dir:
	@-mkdir -m 0700 $(USER_TEMP)/ 2> /dev/null
	@-mkdir -p $(PROJECT_TEMP)/ 2> /dev/null

test: test-unit test-integration

test-integration: debug
	tclsh tests/integration.tcl $(PROJECT_TEMP)/$(TARGET)/debug/jsonwatch

test-unit:
	cargo test $(BUILD_OPTS_WITH_DIR)

uninstall:
	rm /usr/local/bin/jsonwatch

PHONY: dev install release temp-dir test test-integration test-unit uninstall

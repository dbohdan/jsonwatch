JSONWATCH_COMMAND ?= target/debug/jsonwatch

all: test

debug:
	cargo build

release: release-linux release-windows

release-linux:
	cargo build --release --target x86_64-unknown-linux-musl
	cp target/x86_64-unknown-linux-musl/release/jsonwatch jsonwatch-linux-x86_64
	strip jsonwatch-linux-x86_64

release-windows:
	cargo build --release --target i686-pc-windows-gnu
	cp target/i686-pc-windows-gnu/release/jsonwatch.exe jsonwatch-win32.exe
	strip jsonwatch-win32.exe

test: debug test-unit test-integration

test-integration:
	tclsh tests/integration.tcl "$(JSONWATCH_COMMAND)"

test-unit:
	cargo test

PHONY: debug test test-integration test-unit

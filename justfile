set windows-shell := ["powershell.exe", "-NoLogo", "-Command"]

export JSONWATCH_COMMAND := "target/debug/jsonwatch"
export TCLSH := "tclsh"

default: test

debug:
  cargo build

[unix]
release: release-linux release-windows

[unix]
release-linux:
  cargo build --release --target x86_64-unknown-linux-musl
  cp target/x86_64-unknown-linux-musl/release/jsonwatch jsonwatch-linux-x86_64
  strip jsonwatch-linux-x86_64

[unix]
release-windows:
  cargo build --release --target i686-pc-windows-gnu
  cp target/i686-pc-windows-gnu/release/jsonwatch.exe jsonwatch-win32.exe
  strip jsonwatch-win32.exe

[unix]
test: debug test-unit test-integration

[windows]
test: debug test-unit

# The integration tests use Expect and do not work on Windows.
[unix]
test-integration:
  "$TCLSH" tests/integration.tcl

test-unit:
  cargo test

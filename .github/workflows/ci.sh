#! /bin/sh
set -eu

sudo apt install -y expect
cargo build
make test

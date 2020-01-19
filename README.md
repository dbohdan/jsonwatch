jsonwatch — like watch -d but for JSON
======================================

[![Build Status](https://travis-ci.org/dbohdan/jsonwatch.svg?branch=master)](https://travis-ci.org/dbohdan/jsonwatch)

`jsonwatch` is a command line utility with which you can track changes in JSON data delivered by a shell command or a web (HTTP/HTTPS) API. `jsonwatch` requests data from the designated source repeatedly at a set interval and displays the differences when the data changes.  It is similar but not isomorphic in its behavior to how [watch(1)](https://manpages.debian.org/stable/procps/watch.1.en.html) with the `-d` switch works for plain-text data.

It has been tested on Debian 10, Ubuntu 18.04, and Windows 7.

The two previous versions of `jsonwatch` are preserved in the branch [`python`](https://github.com/dbohdan/jsonwatch/tree/python) and [`haskell`](https://github.com/dbohdan/jsonwatch/tree/haskell).


Installation
============

Prebuilt Linux and Windows binaries are available.  They are attached to releases on the [Releases](https://github.com/dbohdan/jsonwatch/releases) page.

Building on Debian and Ubuntu
-----------------------------

Follow the instructions to build a static Linux binary of `jsonwatch` from source on recent Debian and Ubuntu.

1\. Install [Rustup](https://rustup.rs/).  Through Rustup add the stable MUSL target for your CPU.

```sh
rustup target add x86_64-unknown-linux-musl
```

2\. Install the build and testing dependencies.

```sh
sudo apt install build-essential expect musl-tools tcl
```

3\. Clone this repository.  Build and install the binary.

    git clone https://github.com/dbohdan/jsonwatch
    cd jsonwatch
    make test
    make release
    sudo make install "BUILD_USER=$USER"

Cross-compiling for Windows
---------------------------

Follow the instructions to build a 32-bit Windows binary of `jsonwatch` on recent Debian and Ubuntu.

1\. Install [Rustup](https://rustup.rs/).  Through Rustup add the i686 GNU ABI Windows target.

```sh
rustup target add i686-pc-windows-gnu
```

2\. Install the build dependencies.

```sh
sudo apt install build-essential mingw-w64
```

3\. Configure Cargo for cross-compilation.  Put the following in `~/.cargo/config`.

```toml
[target.i686-pc-windows-gnu]
linker = "/usr/bin/i686-w64-mingw32-gcc"
```

4\. Fix the [`crt2.o` issue](https://github.com/rust-lang/rust/issues/48272#issuecomment-429596397).

```sh
cd ~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/i686-pc-windows-gnu/lib/
mv crt2.o crt2.o.bak
cp /usr/i686-w64-mingw32/lib/crt2.o .
```

5\. Clone this repository.  Build the binary.

    git clone https://github.com/dbohdan/jsonwatch
    cd jsonwatch
    make release TARGET=i686-pc-windows-gnu
    cp "/tmp/$USER/jsonwatch-rust/i686-pc-windows-gnu/release/jsonwatch.exe" .


Use examples
============

Commands
--------

### *nix

Testing `jsonwatch`.

    $ jsonwatch -n 1 -c "echo '{ \"filename\": \"'\$(mktemp -u)'\"}'"

    {
      "filename": "/tmp/tmp.dh3Y7LJTaK"
    }
    2020-01-19T18:52:19+0000 .filename: "/tmp/tmp.dh3Y7LJTaK" -> "/tmp/tmp.i4s56VENEJ"
    2020-01-19T18:52:20+0000 .filename: "/tmp/tmp.i4s56VENEJ" -> "/tmp/tmp.zzMUSn45Fc"
    2020-01-19T18:52:21+0000 .filename: "/tmp/tmp.zzMUSn45Fc" -> "/tmp/tmp.Jj1cKt6VLr"
    2020-01-19T18:52:22+0000 .filename: "/tmp/tmp.Jj1cKt6VLr" -> "/tmp/tmp.1LGk4ok8O2"
    2020-01-19T18:52:23+0000 .filename: "/tmp/tmp.1LGk4ok8O2" -> "/tmp/tmp.wWulyho8Qj"

Docker process information.

    $ jsonwatch -c 'docker ps -a "--format={{json .}}"' -n 1

    2020-01-19T18:57:20+0000
        + .Command: "\"bash\""
        + .CreatedAt: "2020-01-19 18:57:20 +0000 UTC"
        + .ID: "dce7fb2194ed"
        + .Image: "i386/ubuntu:latest"
        + .Labels: ""
        + .LocalVolumes: "0"
        + .Mounts: ""
        + .Names: "dreamy_edison"
        + .Networks: "bridge"
        + .Ports: ""
        + .RunningFor: "Less than a second ago"
        + .Size: "0B"
        + .Status: "Created"
    2020-01-19T18:57:21+0000 .RunningFor: "Less than a second ago" -> "1 second ago"
    2020-01-19T18:57:23+0000
        .RunningFor: "1 second ago" -> "3 seconds ago"
        .Status: "Created" -> "Up 1 second"
    2020-01-19T18:57:24+0000
        .RunningFor: "3 seconds ago" -> "4 seconds ago"
        .Status: "Up 1 second" -> "Up 2 seconds"
    2020-01-19T18:57:25+0000
        .RunningFor: "4 seconds ago" -> "5 seconds ago"
        .Status: "Up 2 seconds" -> "Up 3 seconds"

### Windows

On Windows `-c` executes `cmd.exe` commands.

    > jsonwatch -c "type tests\weather1.json"

    {
      "clouds": {
        "all": 92
      },
      "name": "Kiev",
      "coord": {
        "lat": 50.43,
        "lon": 30.52
      },
      "sys": {
        "country": "UA",
        "message": 0.0051,
        "sunset": 1394985874,
        "sunrise": 1394942901
      },
      "weather": [
        {
          "main": "Snow",
          "id": 612,
          "icon": "13d",
          "description": "light shower sleet"
        },
        {
          "main": "Rain",
          "id": 520,
          "icon": "09d",
          "description": "light intensity shower rain"
        }
      ],
      "rain": {
        "3h": 2
      },
      "base": "cmc stations",
      "dt": 1394979003,
      "main": {
        "pressure": 974.8229,
        "humidity": 91,
        "temp_max": 277.45,
        "temp": 276.45,
        "temp_min": 276.15
      },
      "id": 703448,
      "wind": {
        "speed": 10.27,
        "deg": 245.507
      },
      "cod": 200
    }
    2020-01-19T18:51:04+0000 + .test: true
    2020-01-19T18:51:10+0000 .test: true -> false
    2020-01-19T18:51:23+0000 - .test: false

URLs
----

Watching a URL works identically on *nix and on Windows.

Weather tracking.

    $ jsonwatch -u http://api.openweathermap.org/data/2.5/weather\?q\=Kiev,ua --no-initial-values -n 300

    2014-03-17T23:06:19.073790
        + .rain.1h: 0.76
        - .rain.3h: 0.5
        .dt: 1395086402 -> 1395089402
        .main.temp: 279.07 -> 278.66
        .main.temp_max: 279.82 -> 280.15
        .main.temp_min: 277.95 -> 276.05
        .sys.message: 0.0353 -> 0.0083

Geolocation. (Try this on a mobile device.)

    $ jsonwatch -u https://ipinfo.io/ --no-initial-values -n 300


License
=======

`jsonwatch` is distributed under the MIT license.  See the file `LICENSE` for details.   [Wapp](tests/vendor/wapp/wapp.tcl) is copyright (c) 2017 D. Richard Hipp and is distributed under the Simplified BSD License.

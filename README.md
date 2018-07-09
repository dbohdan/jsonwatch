jsonwatch — like watch -d but for JSON
======================================

[![Build Status](https://travis-ci.org/dbohdan/jsonwatch.svg?branch=master)](https://travis-ci.org/dbohdan/jsonwatch)

`jsonwatch` is a command line utility with which you can track changes in JSON data delivered by a shell command or a web (HTTP/HTTPS) API. `jsonwatch` requests data from the designated source repeatedly at a set interval and displays the differences when the data changes. It is similar but not isomorphic in its behavior to how [watch(1)](https://manpages.debian.org/jessie/procps/watch.1.en.html) with the `-d` switch works for plain-text data.

It has been tested on FreeBSD 11.0-RELEASE, Debian 8, Ubuntu 16.04 and Windows 10.

The previous version of `jsonwatch` written in Python is preserved in the branch [`python`](https://github.com/dbohdan/jsonwatch/tree/python).


Installation
============

Prebuilt Linux and Windows binaries are available. They are attached to releases on the [Releases](https://github.com/dbohdan/jsonwatch/releases) page.

Building normally
-----------------

To build `jsonwatch` from the source on FreeBSD, a Linux distribution or Windows follow the instructions below.

1\. Install the [Haskell Stack](https://haskell-lang.org/get-started) and Git.

2\. Clone this repository. Build and install the binary.

    git clone https://github.com/dbohdan/jsonwatch
    cd jsonwatch
    stack test
    stack install

Building a static binary for Linux
----------------------------------

You will need [Docker](https://www.docker.com/). Clone this repository and in it run

```sh
# First build only. Create a volume to cache compiled build dependencies.
docker volume create jsonwatch-stack-dir
# Every build. Build a static binary of jsonwatch.
docker build --tag jsonwatch .
docker run --name jsonwatch-build \
           --mount source=jsonwatch-stack-dir,target=/root/.stack \
           jsonwatch \
           stack --local-bin-path /usr/local/bin \
                 --install-ghc install \
                 --test \
                 --ghc-options='-optl-static -optl-pthread'
docker cp jsonwatch-build:/usr/local/bin/jsonwatch jsonwatch-static
# Every build. Clean up the containers after a build. This includes the
# containers from any failed builds.
docker ps --all --quiet --filter=ancestor=jsonwatch | xargs docker rm
# Last build only. Remove the volume and the image. Do this to free up disk
# space (circa 2 GB total) once you do not expect to build jsonwatch again
# soon. Note that building will take considerably longer if you have to
# recreate the volume.
docker rmi jsonwatch
docker volume rm jsonwatch-stack-dir
```

Use examples
============

Commands
--------

### *nix

Testing `jsonwatch`.

    $ jsonwatch -n 1 -c "echo '{ \"filename\": \"'\$(mktemp -u)'\"}'"

    {
        "filename": "/tmp/tmp.ZYFQ5RwGN5"
    }
    2014-03-16T22:40:08.130170 .filename: /tmp/tmp.ZYFQ5RwGN5 -> /tmp/tmp.Pi0WXp2Aoj
    2014-03-16T22:40:09.133995 .filename: /tmp/tmp.Pi0WXp2Aoj -> /tmp/tmp.2U181cBL2L
    2014-03-16T22:40:10.137640 .filename: /tmp/tmp.2U181cBL2L -> /tmp/tmp.i5sGwYig4S
    2014-03-16T22:40:11.141320 .filename: /tmp/tmp.i5sGwYig4S -> /tmp/tmp.Sv0s60LuoT
    2014-03-16T22:40:12.144990 .filename: /tmp/tmp.Sv0s60LuoT -> /tmp/tmp.skSIruBLfQ

Cryptocurrency daemon information (including balance changes).

    $ jsonwatch --no-initial-values -c "dogecoind getinfo"

    2014-03-18T14:16:57.855226 .blocks: 145779 -> 145780
    2014-03-18T14:17:07.922137
        .blocks: 145780 -> 145781
        .difficulty: 1316.42722979 -> 1178.89009968
    2014-03-18T14:19:13.921734 .connections: 8 -> 7
    2014-03-18T14:19:39.128119 .connections: 7 -> 8

### Windows

On Windows `-c` executes `cmd.exe` commands.

    > jsonwatch -c "type test\weather1.json"

    {"clouds": {"all": 92}, "name": "Kiev", "coord": {
    "lat": 50.43, "lon": 30.52}, "sys": {"country": "UA",
    "message": 0.0051, "sunset": 1394985874, "sunrise": 1394942901
    }, "weather": [{"main": "Snow", "id": 612, "icon": "13d",
    "description": "light shower sleet"}, {"main": "Rain", "id":
    520, "icon": "09d", "description": "light intensity shower rain"}],
    "rain": {"3h": 2}, "base": "cmc stations", "dt":
    1394979003, "main": {"pressure": 974.8229, "humidity": 91,
    "temp_max": 277.45, "temp": 276.45, "temp_min": 276.15}, "id"
    : 703448, "wind": {"speed": 10.27, "deg": 245.507}, "cod":
    200}

    2017-03-02T16:58:08+0200 + .test: true
    2017-03-02T17:00:52+0200 .test: true -> false
    2017-03-02T17:01:04+0200 - .test: false

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

`jsonwatch` is distributed under the MIT license. See the file `LICENSE` for details.

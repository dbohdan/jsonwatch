jsonwatch — like watch -d but for JSON
======================================

`jsonwatch` is a command-line utility you can use to track changes in JSON data delivered by a shell command or a web (HTTP/HTTPS) API.
`jsonwatch` requests data from the designated source repeatedly at a set interval and displays the differences when the data change.
It is similar but not isomorphic in its behavior to how [watch(1)](https://manpages.debian.org/stable/procps/watch.1.en.html) with the `-d` switch works for plain-text data.

This is a legacy version of `jsonwatch` ported to modern Python tooling.
In most cases, you want to use the [current version](https://github.com/dbohdan/jsonwatch).

`jsonwatch` requires Python 3.8 or newer.

Installation
============

It is recommended to use [pipx](https://github.com/pypa/pipx) or [uv](https://github.com/astral-sh/uv) to install `jsonwatch`.

```shell
pipx install git+https://github.com/dbohdan/jsonwatch@python
# or
uv tool install git+https://github.com/dbohdan/jsonwatch@python
```

Use examples
============

Commands
--------

Testing `jsonwatch`.

```none
$ jsonwatch -n 1 -c "echo '{ \"filename\": \"'\$(mktemp -u)'\"}'"

{
    "filename": "/tmp/tmp.ZYFQ5RwGN5"
}
2014-03-16T22:40:08.130170 .filename: /tmp/tmp.ZYFQ5RwGN5 -> /tmp/tmp.Pi0WXp2Aoj
2014-03-16T22:40:09.133995 .filename: /tmp/tmp.Pi0WXp2Aoj -> /tmp/tmp.2U181cBL2L
2014-03-16T22:40:10.137640 .filename: /tmp/tmp.2U181cBL2L -> /tmp/tmp.i5sGwYig4S
2014-03-16T22:40:11.141320 .filename: /tmp/tmp.i5sGwYig4S -> /tmp/tmp.Sv0s60LuoT
2014-03-16T22:40:12.144990 .filename: /tmp/tmp.Sv0s60LuoT -> /tmp/tmp.skSIruBLfQ
```

Cryptocurrency daemon information (including balance changes).

```none
$ jsonwatch --no-initial-values -c "dogecoind getinfo"

2014-03-18T14:16:57.855226 .blocks: 145779 -> 145780
2014-03-18T14:17:07.922137
    .blocks: 145780 -> 145781
    .difficulty: 1316.42722979 -> 1178.89009968
2014-03-18T14:19:13.921734 .connections: 8 -> 7
2014-03-18T14:19:39.128119 .connections: 7 -> 8
```

URLs
----

Weather tracking.

```none
$ jsonwatch -u http://api.openweathermap.org/data/2.5/weather\?q\=Kiev,ua --no-initial-values -n 300

2014-03-17T23:06:19.073790
    + .rain.1h: 0.76
    - .rain.3h: 0.5
    .dt: 1395086402 -> 1395089402
    .main.temp: 279.07 -> 278.66
    .main.temp_max: 279.82 -> 280.15
    .main.temp_min: 277.95 -> 276.05
    .sys.message: 0.0353 -> 0.0083
```

Geolocation.
(Try this on a mobile device.)

```none
$ jsonwatch -u http://freegeoip.net/json/ --no-initial-values -n 300
```

License
=======

`jsonwatch` is distributed under the MIT License.
See the file `LICENSE` for details.

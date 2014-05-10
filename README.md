jsonwatch — like watch -d but for JSON
======================================

`jsonwatch` is a command line utility with which you can track changes in JSON data delivered by a shell command or a web (HTTP/HTTPS) API. `jsonwatch` requests data from the designated source repeatedly at a set interval and displays the differences when the data changes. It is similar but not isomorphic in its behavior to how [watch(1)](http://manpages.debian.org/cgi-bin/man.cgi?query=watch&apropos=0&sektion=0&manpath=Debian+7.0+wheezy&format=html&locale=en) with the `-d` switch works for plain-text data.

`jsonwatch` requires Python 2.7 or 3.x. It has been tested on Debian 7, Ubuntu 12.04 and Fedora 20.

Installation
============

The following instructions cover installing `jsonwatch` with Python 2.7.

1\. Install Setuptools for Python. On Debian and Ubuntu you can do this with

    sudo apt-get install python-setuptools

On Fedora do

    su -
    yum install python-setuptools

2\. Clone the repository then run

    sudo python setup.py install

The command `jsonwatch` will be installed.

Use examples
============

Commands
--------

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

URLs
----

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

    $ jsonwatch -u http://freegeoip.net/json/ --no-initial-values -n 300

License
=======

`jsonwatch` is distributed under the MIT license. See the file `LICENSE` for details.

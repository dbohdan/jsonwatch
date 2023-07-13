#! /usr/bin/env tclsh
# Integration tests for jsonwatch.
# ==============================================================================
# Copyright (c) 2020, 2023 D. Bohdan and contributors listed in AUTHORS
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
# ==============================================================================

package require Expect 5

set max_match 100000
set timeout 5

set ::argv [lassign $::argv binary]
if {![file exists $binary]} {
    puts stderr "usage: [file tail [info script]] binary \[tcltest-arg ...\]"
    exit 1
}

package require tcltest 2
source [file dirname [info script]]/vendor/wapp/wapp.tcl

set host 127.0.0.1
set port 8015


# CLI.

tcltest::test cli-1.1 {} -body {
    exec $binary -h
} -match regexp -result ***:(?i)usage

tcltest::test cli-1.2 {} -body {
    exec $binary --nonsense
} -returnCodes error -match glob -result *

tcltest::test cli-1.3 {} -body {
    exec $binary -c false -u http://example.com
} -returnCodes error -match glob -result *


# Command tests.

tcltest::test command-1.1 {} -body {
    spawn $binary -c {cat tests/weather1.json}
    expect {
        -regexp {"description":\s*"light shower sleet"} { return matched }
        timeout { return {timed out} }
    }
} -cleanup close -result matched


tcltest::test command-1.2 {} -setup {set timeout 2} -body {
    spawn $binary -c {cat tests/weather1.json} -n 1 --no-initial-values
    expect {
        -regexp {[A-Za-z0-9]} { return text }
        timeout { return {timed out} }
    }
} -cleanup {close; set timeout 5} -result {timed out}


tcltest::test command-1.3 {} -body {
    set ch [file tempfile path]
    puts $ch {[1,2,3,4,5]}
    flush $ch

    spawn $binary --command "cat '$path'" -n 1
    expect {
        -glob *1*2*3*4*5* {}
        timeout { return {first timeout} }
    }

    seek $ch 0
    puts $ch {[1,2,3,4,5,6,7]}
    flush $ch

    expect \
        -glob *[clock format [clock seconds] -format %Y-%m]*6*7* {} \
        timeout { return {second timeout} } \

    close $ch

    lindex completed
} -cleanup {close; file delete $path} -result completed


### URL tests.

proc wapp-page-timestamp {} {
    wapp-mimetype application/json
    wapp-subst {{"timestamp": %string([clock seconds])}}
}


set count 0
proc wapp-page-alternate {} {
    wapp-mimetype application/json

    if {$::count % 2} {
        wapp {["foo", "baz"]}
    } else {
        wapp {["bar"]}
    }

    incr ::count
}


proc wapp-page-nested-obj {} {
    wapp-mimetype application/json

    if {$::count % 2} {
        wapp {{"k": "v"}}
    } else {
        wapp {{"k": {"nested": "v2"}}}
    }

    incr ::count
}


wapp-start [list -fromip 127.0.0.1 -nowait -server $port -trace]


tcltest::test url-1.1 {} -body {
    spawn $binary -u http://$host:$port/timestamp --interval 1
    set re {{\s*"timestamp":\s*\d+\s*}}
    expect \
        -regexp $re { lindex matched } \
        timeout { lindex {first timeout} } \

    set re2 [format {%1$s[\dT:+-]+ .timestamp: \d+ -> \d+} \
                    [clock format [clock seconds] -format %Y-%m] \
    ]
    expect \
        -regexp $re2 { lindex matched } \
        timeout { lindex {second timeout} } \

} -cleanup close -result matched


tcltest::test url-1.2 {} -body {
    spawn $binary --url http://$host:$port/timestamp -n 1 --no-date

    expect {
        -regexp {\n.timestamp: \d+ -> \d+} { lindex matched }
        timeout { lindex {timed out} }
    }
} -cleanup close -result matched


tcltest::test url-1.3 {} -body {
    spawn $binary --url http://$host:$port/alternate \
                  -n 1 \
                  --no-initial-values

    expect {
        -regexp {    ?\.0: "?bar"? -> "?foo"?\s+    ?\+ .1: "?baz"?} {
            lindex matched
        }
        timeout {
            lindex {timed out}
        }
    }
} -cleanup close -result matched


tcltest::test url-1.4 {} -body {
    spawn $binary --url http://$host:$port/nested-obj \
                  -n 1 \
                  --no-initial-values

    expect {
        -regexp {    ?- \.k\.nested: "v2"\s+    \+ \.k: "v"} {
            lindex matched
        }
        timeout {
            lindex {timed out}
        }
    }
} -cleanup close -result matched


# Exit with a nonzero status if there are failed tests.
set failed [expr {$tcltest::numTests(Failed) > 0}]

tcltest::cleanupTests
if {$failed} {
    exit 1
}

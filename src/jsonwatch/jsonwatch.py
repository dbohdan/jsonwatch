#!/usr/bin/env python2
# jsonwatch 0.1.0
# Copyright 2014 Danyil Bohdan

from __future__ import print_function

import urllib2
import argparse
import json
import sys
import subprocess
import time
import datetime
import traceback
from jsondiff import json_flatten, json_flat_diff, json_diff_str


class JSONRequestURL(object):
    def __init__(self, url):
        self.url = url

    def perform(self):
        return json.loads(urllib2.urlopen(self.url).read())


class JSONRequestCommand(object):
    def __init__(self, command):
        self.command = command

    def perform(self):
        return json.loads(subprocess.check_output(self.command, shell=True))


def json_print(jsn):
    print(json.dumps(jsn, indent=4))


def poll_loop(interval, req, date=True, initial_values=True):
    prev_output = None
    output = None
    try:
        output = req.perform()
        if initial_values:
            json_print(output)
        output = json_flatten(output)
    except Exception:
        #print(str(e))
        print(traceback.format_exc())
    while True:
        time.sleep(interval)
        try:
            prev_output, output = output, json_flatten(req.perform())
            diff = json_flat_diff(prev_output, output)
            if diff is not None:
                msg = json_diff_str(diff)
                msg.sort()
                # If msg is multi-line print each difference on a new line
                # with indentation.
                prefix = ''
                if date:
                    prefix += datetime.datetime.now().isoformat()
                if len(msg) > 1:
                    print(prefix, \
                           "\n   ", "\n    ".join(msg))
                else:
                    print(prefix, msg[0])
        except Exception:
            print(traceback.format_exc())
            #print(str(e))


def main():
    parser = argparse.ArgumentParser(description='Track changes in JSON data')
    parser.add_argument('-u', '--url', help='URL',
                        default='', required=False, metavar='url',
                        dest='url')
    parser.add_argument('-c', '--command', help='command to execute',
                        default='', required=False, metavar='command',
                        dest='command')
    parser.add_argument('-n', '--interval', help='interval',
                        default=5, type=int, required=False,
                        metavar='seconds', dest='interval')
    parser.add_argument('--no-date',
                        help='don\'t print date and time for each diff',
                        default=True, required=False,
                        dest='print_date', action='store_false')
    parser.add_argument('--no-initial-values',
                        help='don\'t print the initial JSON values',
                        default=True, required=False,
                        dest='print_init_val', action='store_false')
    # Process command line arguments.
    args = parser.parse_args()

    # If both or none of 'url' and 'command' given display help and exit.
    if (args.url == '') == (args.command == ''):
        parser.print_help()
        sys.exit(1)

    req = None
    if args.url != '':
        req = JSONRequestURL(args.url)
    else:
        req = JSONRequestCommand(args.command)
    poll_loop(args.interval, req, args.print_date, args.print_init_val)


if __name__ == "__main__":
    main()

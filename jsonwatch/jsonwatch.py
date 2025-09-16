#! /usr/bin/env python3
# jsonwatch
# Copyright (c) 2014, 2025 D. Bohdan
# This code is released under the MIT license. See the file LICENSE.

import argparse
import datetime
import json
import subprocess
import sys
import time
import traceback
import urllib.error
import urllib.request

from .jsondiff import json_diff_str, json_flat_diff, json_flatten


class JSONRequestURL:
    """Abstracts away requests for JSON data from URLs."""

    def __init__(self, url):
        self.url = url
        self.opener = urllib.request.build_opener()
        # User agent needed for some APIs that decide whether to feed
        # you JSON data or a webpage/error 403 based on it.
        self.opener.addheaders = [("User-agent", "curl")]

    def perform(self):
        return json.loads(self.opener.open(self.url).read().decode("utf-8"))


class JSONRequestCommand:
    """Abstracts away requests for JSON data from shell commands."""

    def __init__(self, command):
        self.command = command

    def perform(self):
        return json.loads(
            subprocess.check_output(self.command, shell=True, text=True)  # noqa: S602
        )


def json_print(jsn):
    print(json.dumps(jsn, indent=4))  # noqa: T201


def poll_loop(interval, req, *, date=True, initial_values=True):
    """Perform requests for JSON data. Print out changes when they occur."""

    prev_output = None
    output = None
    try:
        output = req.perform()
        if initial_values:
            json_print(output)
        output = json_flatten(output)
    except (subprocess.CalledProcessError, urllib.error.HTTPError, ValueError):
        print(traceback.format_exc(), file=sys.stderr)  # noqa: T201
    while True:
        try:
            time.sleep(interval)
            try:
                prev_output, output = output, json_flatten(req.perform())
                diff = json_flat_diff(prev_output, output)
                if diff is not None:
                    msg = json_diff_str(diff)
                    msg.sort()
                    # If msg is multi-line print each difference on a new line
                    # with indentation.
                    prefix = ""
                    if date:
                        prefix += datetime.datetime.now(
                            datetime.timezone.utc
                        ).isoformat()
                    if len(msg) > 1:
                        indented_msg = "\n    ".join(msg)
                        print(f"{prefix}\n    {indented_msg}")  # noqa: T201
                    else:
                        print(f"{prefix} {msg[0]}")  # noqa: T201
            except (
                subprocess.CalledProcessError,
                urllib.error.HTTPError,
                ValueError,
            ):
                print(traceback.format_exc(), file=sys.stderr)  # noqa: T201
        except KeyboardInterrupt:  # noqa: PERF203
            sys.exit(0)


def main():
    parser = argparse.ArgumentParser(description="Track changes in JSON data")
    parser.add_argument(
        "-u",
        "--url",
        help="URL",
        default="",
        required=False,
        metavar="url",
        dest="url",
    )
    parser.add_argument(
        "-c",
        "--command",
        help="command to execute",
        default="",
        required=False,
        metavar="command",
        dest="command",
    )
    parser.add_argument(
        "-n",
        "--interval",
        help="interval",
        default=None,
        type=int,
        required=False,
        metavar="seconds",
        dest="interval",
    )
    parser.add_argument(
        "--no-date",
        help="don't print date and time for each diff",
        default=True,
        required=False,
        dest="print_date",
        action="store_false",
    )
    parser.add_argument(
        "--no-initial-values",
        help="don't print the initial JSON values",
        default=True,
        required=False,
        dest="print_init_val",
        action="store_false",
    )
    # Process command line arguments.
    args = parser.parse_args()

    # If both or none of 'url' and 'command' given display help and exit.
    if (args.url == "") == (args.command == ""):
        parser.print_help()
        sys.exit(1)

    req = None
    if args.url != "":
        if args.interval is None:
            args.interval = 60
        req = JSONRequestURL(args.url)
    else:
        if args.interval is None:
            args.interval = 5
        req = JSONRequestCommand(args.command)
    poll_loop(
        args.interval, req, date=args.print_date, initial_values=args.print_init_val
    )


if __name__ == "__main__":
    main()

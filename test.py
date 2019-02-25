#!/usr/bin/env python3

# prettysharp
# Copyright (C) 2019 John Doty
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

"""The test harness for prettysharp."""

import argparse
import subprocess
import os
import os.path
import sys
import difflib
from collections import namedtuple

TestResult = namedtuple("TestResult", ["path", "message", "passed"])


test_count = 0


def baseline_file(path):
    expectedfile = path + ".expected"
    proc = subprocess.Popen(
        ["./prettysharp", path], stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    stdout, stderr = proc.communicate()
    with open(expectedfile, "wb") as f:
        f.write(stdout)


def test_file(path, compare):
    expectedfile = path + ".expected"
    proc = subprocess.Popen(
        ["./prettysharp", path], stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    stdout, stderr = proc.communicate()
    stdout = stdout.decode("utf-8")
    stderr = stderr.decode("utf-8")
    if proc.returncode:
        result = TestResult(
            path=path,
            message="  {}".format("\n  ".join(stderr.splitlines())),
            passed=False,
        )
    elif not compare:
        result = TestResult(path=path, message="OK! (No compare)", passed=True)
    elif not os.path.exists(expectedfile):
        result = TestResult(
            path=path, message="  Baseline file doesn't exist", passed=False
        )
    else:
        actual = stdout.splitlines()
        with open(expectedfile, "rb") as f:
            text = f.read().decode("utf-8")
            expected = text.splitlines()
        diff = list(
            difflib.unified_diff(
                expected, actual, fromfile=path, tofile=path + ".expected", lineterm=""
            )
        )
        if diff:
            result = TestResult(
                path=path, message="  {}".format("\n  ".join(diff)), passed=False
            )
            pass
        else:
            result = TestResult(path=path, message="OK!", passed=True)

    global test_count
    test_count += 1
    if test_count > 80:
        end = "\n"
        test_count = 0
    else:
        end = ""

    print("." if result.passed else "F", end=end)
    return result


parser = argparse.ArgumentParser(description="Test harness for prettysharp.")
parser.add_argument(
    "root", nargs="?", default="tests", help="The root directory to test."
)
parser.add_argument(
    "--no-compare", action="store_true", help="Don't look for baselines and stuff"
)
parser.add_argument(
    "--rebase", action="store_true", help="Regenerate baseline files if necessary"
)

args = parser.parse_args()

results = []
for path, dirs, files in os.walk(args.root):
    if "fuzzing" in path:
        continue

    for fname in files:
        if fname.endswith(".cs"):
            results.append(
                test_file(os.path.join(path, fname), compare=(not args.no_compare))
            )

print()
failures = 0
for result in results:
    if not result.passed:
        print("FAILED {}:\n{}\n\n".format(result.path, result.message))
        failures += 1
        if args.rebase:
            yn = input("Re-write baseline? (yN) ")
            if len(yn) > 0 and yn[0] in ("Y", "y"):
                baseline_file(result.path)
                print("Wrote new baseline for {}".format(result.path))
                failures -= 1
            print()


if failures > 0:
    print("{} test{} failed.".format(failures, "s" if failures > 1 else ""))
else:
    print("All tests passed.")

sys.exit(-1 if failures > 0 else 0)

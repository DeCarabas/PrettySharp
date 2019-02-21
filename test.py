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

import subprocess
import os
import sys
import difflib
from collections import namedtuple

TestResult = namedtuple("TestResult", ["path", "message", "passed"])


def test_file(path):
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
    else:
        actual = stdout.splitlines()
        with open(path + ".expected", "rb") as f:
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

    print("." if result.passed else "F", end="")
    return result


results = []
for path, dirs, files in os.walk("tests"):
    if "fuzzing" in path:
        continue

    for fname in files:
        if fname.endswith(".cs"):
            results.append(test_file(os.path.join(path, fname)))

print()
failed = False
for result in results:
    if not result.passed:
        print("FAILED {}:\n{}\n\n".format(result.path, result.message))
        failed = True


sys.exit(-1 if failed else 0)

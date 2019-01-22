#!env python3
import subprocess
import os
import difflib
from collections import namedtuple

TestResult = namedtuple("TestResult", ["path", "message", "passed"])


def test_file(path):
    proc = subprocess.run(
        ["./prettysharp", path],
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
        encoding="utf8",
    )
    if proc.returncode:

        result = TestResult(
            path=path,
            message="  {}".format("\n  ".join(proc.stderr.splitlines())),
            passed=False,
        )
    else:
        actual = proc.stdout.splitlines()
        with open(path + ".expected") as f:
            expected = f.read().splitlines()
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
    for fname in files:
        if fname.endswith(".cs"):
            results.append(test_file(os.path.join(path, fname)))

print()
for result in results:
    if not result.passed:
        print("FAILED {}:\n{}\n\n".format(result.path, result.message))

#!env python3
import subprocess
import os

def test_file(path):
    proc = subprocess.run(
        ["./prettysharp", path],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        )
    if proc.returncode:
        print("F", end="")
    else:
        print(".", end="")


results = []
for path, dirs, files in os.walk('tests'):
    for fname in files:
        if fname.endswith('.cs'):
            results.append(test_file(os.path.join(path, fname)))
print()

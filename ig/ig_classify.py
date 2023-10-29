#!/usr/bin/env python

from statistics import mean
import subprocess
import sys

ths = (mean((2/3, 3/4)), mean((3/4, 1)), mean((1, 4/3)), mean((4/3, 3/2)))
dds = ("2-3", "3-4", "1-1", "4-3", "3-2")

for im in sys.argv[1:]:
    h = int(subprocess.run(("identify", "-ping", "-format", "%h", im),
                           capture_output=True, check=True).stdout.decode(
                               "utf-8"))
    w = int(subprocess.run(("identify", "-ping", "-format", "%w", im),
                           capture_output=True, check=True).stdout.decode(
                               "utf-8"))
    for i, th in enumerate(ths):
        if w / h < th:
            rat = dds[i]
            break
    else:
        rat = dds[-1]
    print(im, w, h, rat)

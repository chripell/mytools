#!/usr/bin/env python
"""Normalizes ini files."""

# pylint: disable=C0103
# pylint: disable=R0903
# pylint: disable=R1702
# pylint: disable=R0912

import re
import sys
from collections import defaultdict


class Processor:
    """Process and normalizes an ini file."""

    def __init__(self):
        self.r: dict[str, dict[str, str]] = defaultdict(dict)
        self.heading = re.compile(r"\[(\w+)\]")
        self.entry = re.compile(r"(\w+)=(.*)")
        self.cur = None

    def line(self, line: str):
        """Process a line of an ini file to be normalized."""
        if m := self.heading.match(line):
            self.cur = m[1]
        if m := self.entry.match(line):
            if not self.cur:
                raise ValueError("Missing section header")
            self.r[self.cur][m[1]] = m[2]

    def out(self) -> str:
        """Generates normalized ini file."""
        sections = []
        hdrs = list(self.r.keys())
        hdrs.sort()
        for hdr in hdrs:
            rc = self.r[hdr]
            sec = [f"[{hdr}]\n"]
            ks = list(rc.keys())
            ks.sort()
            for k in ks:
                sec.append(f"{k}={rc[k]}\n")
            sections.append("".join(sec))
        return "\n".join(sections)


def main():
    """Main function."""
    rep = Processor()
    for fname in sys.argv[1:]:
        with open(fname, encoding="utf8") as fd:
            for line in fd:
                rep.line(line)
    print(rep.out(), end="")


if __name__ == '__main__':
    main()

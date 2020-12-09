#!/usr/bin/env python

import argparse
import collections
import os
import re

def show_top(d):
    for x, n in d:
        print(x, n)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Gets stats from a not much mailbox.")
    parser.add_argument("--dir", type=str, default=".",
                        help="directory containing the mailbox")
    parser.add_argument("--top", type=int, default=100,
                        help="number of top to show")
    args = parser.parse_args()
    by_n = collections.Counter()
    by_bytes = collections.Counter()
    matcher = re.compile(r"^From:.*?\n?.*[^\w\.-]([\w\.-]+@[\w\.-]+)",
                         flags=(re.MULTILINE | re.IGNORECASE))
    for name in os.listdir(args.dir):
        fname = os.path.join(args.dir, name)
        if not os.path.isfile(fname):
            continue
        with open(fname, "rt", encoding='utf-8', errors='ignore') as fd:
            data = fd.read()
            m = matcher.search(data)
            if m is None:
                print(f"File {name} didn't match")
                continue
            email = m.group(1)
            by_n.update((email,))
            by_bytes.update({email: len(data)})
    print(f"Top {args.top} by number:")
    show_top(by_n.most_common(args.top))
    print(f"Top {args.top} by size:")
    show_top(by_bytes.most_common(args.top))

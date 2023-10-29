#!/usr/bin/env python

import os
from PIL import Image, ImageFilter
import sys

for fname in sys.argv[1:]:
    im = Image.open(fname)
    w, h = im.size
    if w == h:
        imout = im
    else:
        if w > h:
            off_x = (w - h) // 2
            off_y = 0
            lout = w
            lin = h
        else:
            off_x = 0
            off_y = (h - w) // 2
            lout = h
            lin = w
        imout = im.crop((off_x, off_y, lin, lin)).resize((lout, lout))
        imout = imout.filter(ImageFilter.GaussianBlur(radius=33))
        imout.paste(im, (off_y, off_x))
    ofname = os.path.splitext(fname)[0] + "_square.jpg"
    print(fname, w, h, "->", ofname)
    imout.save(ofname)

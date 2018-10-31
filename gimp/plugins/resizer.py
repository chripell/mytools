#!/usr/bin/env python

import ConfigParser
import errno
from gimpfu import register, PF_INT16, pdb, main
from gimpenums import INTERPOLATION_CUBIC
import gimp
import os


def mkdir_p(path):
    try:
        os.makedirs(path)
    except OSError as exc:
        if exc.errno == errno.EEXIST and os.path.isdir(path):
            pass
        else:
            raise


CONFIG_DIR = os.path.join(
    os.path.expanduser("~"),
    ".config",
    "GIMP_plugins")
mkdir_p(CONFIG_DIR)
CONFIG_FILE = os.path.join(
    CONFIG_DIR,
    "resizer_max")
config = ConfigParser.RawConfigParser()
config.read(CONFIG_FILE)
try:
    msize = config.getint("sizes", "max")
except (ConfigParser.NoOptionError, ConfigParser.NoSectionError):
    config.add_section("sizes")
    config.set("sizes", "max", "1600")
    msize = config.getint("sizes", "max")


def resizer(img, drawable, size_max):
    gimp.context_push()
    img.undo_group_start()

    w = img.width
    h = img.height
    if w > h:
        factor = float(size_max) / w
    else:
        factor = float(size_max) / h
    pdb.gimp_image_scale_full(
        img, w * factor, h * factor,
        INTERPOLATION_CUBIC)
    config.set("sizes", "max", size_max)
    with open(CONFIG_FILE, 'wb') as configfile:
        config.write(configfile)

    img.undo_group_end()
    gimp.context_pop()


register(
    "python_resizer_max",
    "Resize to a given maximum dimension",
    "Resize to a given maximum dimension",
    "chripell@gmail.com",
    "Public Domain",
    "2018",
    "<Image>/Script-Fu/Resizer Max",
    "RGB*, GRAY*",
    [
        (PF_INT16, "max_size", "Maximum size", msize),
    ],
    [],
    resizer)

main()

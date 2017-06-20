#!/usr/bin/python2

import errno
import json
import os
import sys
import xmltodict


def mkdir_p(path):
    try:
        os.makedirs(path)
    except OSError as exc:  # Python >2.5
        if exc.errno == errno.EEXIST and os.path.isdir(path):
            pass
        else:
            raise


def alist(x):
    if isinstance(x, (list, tuple)) and not isinstance(x, basestring):
        return x
    return (x,)


def get_files(assoc, path):
    ass = assoc["association"]
    if len(path) == 1:
        for a in alist(ass):
            if a["@category"] == path[0]:
                return [f["@name"] for f in alist(a["mainFiles"]["file"])]
        return []
    p = path.pop(0)
    for a in alist(ass):
        if a["@category"] == p:
            return get_files(a["associatedFiles"], path)
    return []


def write_list(fname, l):
    with open(fname, "w") as fp:
        fp.writelines(["%s\n" % i for i in l])


def run(cmd):
    if os.system(cmd) != 0:
        raise OSError("Execution of '%s' failed" % cmd)


cwd = os.getcwd()
fname = sys.argv[1]
x = xmltodict.parse(open(fname))
base_fname = os.path.splitext(fname)[0]
base_dir = os.path.join(cwd, base_fname)

sky_dir = os.path.join(base_dir, "sky")
mkdir_p(sky_dir)
os.chdir(sky_dir)
if not os.path.isfile("bias.sof"):
    sky_flat_biases = get_files(x, ["SCI_IMG_2x2_low", "FLAT_SKY", "BIAS"])
    if len(sky_flat_biases) == 0:
        raise ValueError("Cannot find sky flat biases")
    write_list("bias.sof",
               ["%s.fits BIAS" %
                os.path.join(cwd, i) for i in sky_flat_biases])
    run("esorex fors_bias bias.sof")

if not os.path.isfile("master_sky_flat_img.fits"):
    sky_flat = get_files(x, ["SCI_IMG_2x2_low", "FLAT_SKY"])
    if len(sky_flat) == 0:
        raise ValueError("Cannot find sky flats")
    write_list("flat.sof",
               ["%s.fits SKY_FLAT_IMG" %
                os.path.join(cwd, i) for i in sky_flat] +
               ["master_bias.fits MASTER_BIAS"])
    run("esorex fors_img_sky_flat flat.sof")

os.chdir(base_dir)
if not os.path.isfile("bias.sof"):
    biases = get_files(x, ["SCI_IMG_2x2_low", "BIAS"])
    if len(biases) == 0:
        raise ValueError("Cannot find biases")
    write_list("bias.sof",
               ["%s.fits BIAS" %
                os.path.join(cwd, i) for i in biases])
    run("esorex fors_bias bias.sof")

if not os.path.isfile("science_reduced_img.fits"):
    write_list("sci.sof",
               ["%s/%s.fits SCIENCE_IMG" % (cwd, base_fname),
                "%s/master_bias.fits MASTER_BIAS" % base_dir,
                "%s/master_sky_flat_img.fits MASTER_SKY_FLAT_IMG" % sky_dir,
                "%s/eso/calib/fors-5.3.23/cal/fors2_Norma_phot.fits "
                "PHOT_TABLE" % os.getenv("HOME")])
    run("esorex fors_img_science sci.sof")

# print json.dumps(x, indent=4)

#!/usr/bin/env python
#-*- coding:utf-8 -*-

# Simulate HDR v1.4.1
# Copyright (c) 2013 David Moreno
# dmc.coder@gmail.com
# Licence: GNU GPLv3  ( http://www.gnu.org/licenses/ )

# This program is distributed WITHOUT ANY WARRANTY.
# You using it in your own risk.

#Merging options.
MERGE_NONE = 0
MERGE_HDR = 1
MERGE_ALL = 2

#Creates the tone mapping layer.
def tone_map (img, layer, blur):
	#Duplicates the HDR layer.
	dup_lay = pdb.gimp_layer_copy(layer, True)
	pdb.gimp_image_add_layer(img, dup_lay, -1)

	#Greyscale duplicate.
	grey_lay = pdb.gimp_layer_copy(dup_lay, True)
	pdb.gimp_image_add_layer(img, grey_lay, -1)

	#Desaturates, inverts and blurs.
	pdb.gimp_desaturate_full(grey_lay, DESATURATE_LUMINOSITY)
	pdb.gimp_invert(grey_lay)
	if blur > 0:
		pdb.plug_in_gauss_rle2(img, grey_lay, blur, blur)

	#Creates the tone map layer.
	pdb.gimp_layer_set_opacity(grey_lay, 75)
	tm_lay = pdb.gimp_image_merge_down(img, grey_lay, CLIP_TO_IMAGE)
	pdb.gimp_layer_set_mode(tm_lay, OVERLAY_MODE)
	pdb.gimp_drawable_set_name(tm_lay, "tone map")

	return tm_lay

def plugin_main (img, lay, dark, clear, blur, tm_blur, merge, tm_only):
	pdb.gimp_image_undo_group_start(img)
	active_lay = pdb.gimp_image_get_active_layer(img)

	#Checks if the user wants only a tone map.
	if tm_only:
		tone_map(img, active_lay, tm_blur)
		pdb.gimp_image_undo_group_end(img)
		return

	#Creates the dark layer.
	dark_lay = pdb.gimp_layer_copy(active_lay, True)
	pdb.gimp_image_add_layer(img, dark_lay, -1)
	pdb.gimp_drawable_set_name(dark_lay, "dark")

	#Darkens the levels.
	pdb.gimp_levels(dark_lay, HISTOGRAM_VALUE, 0, 255, dark, 0, 255)

	#Creates the mask.
	dark_mask = pdb.gimp_layer_create_mask(dark_lay, ADD_COPY_MASK)
	pdb.gimp_layer_add_mask(dark_lay, dark_mask)
	pdb.gimp_layer_set_edit_mask(dark_lay, True)

	#Blurs the mask and applies it to the layer..
	if blur > 0:
		pdb.plug_in_gauss_rle2(img, dark_mask, blur, blur)
	pdb.gimp_layer_remove_mask(dark_lay, MASK_APPLY)

	#Creates the clear layer.
	clear_lay = pdb.gimp_layer_copy(active_lay, True)
	pdb.gimp_image_add_layer(img, clear_lay, -1)
	pdb.gimp_drawable_set_name(clear_lay, "clear")

	#Clears the levels.
	pdb.gimp_levels(clear_lay, HISTOGRAM_VALUE, 0, 255, clear, 0, 255)

	#Creates the mask.
	clear_mask = pdb.gimp_layer_create_mask(clear_lay, ADD_COPY_MASK)
	pdb.gimp_layer_add_mask(clear_lay, clear_mask)
	pdb.gimp_layer_set_edit_mask(clear_lay, True)
	pdb.gimp_invert(clear_mask)

	#Blurs the mask and applies it to the layer..
	if blur > 0:
		pdb.plug_in_gauss_rle2(img, clear_mask, blur, blur)
	pdb.gimp_layer_remove_mask(clear_lay, MASK_APPLY)

	#Merges the created layers.
	if merge == MERGE_NONE:
		#Duplicates the normal, dark and clear layers.
		normal_dup_lay = pdb.gimp_layer_copy(active_lay, True)
		pdb.gimp_image_add_layer(img, normal_dup_lay, -1)

		dark_dup_lay = pdb.gimp_layer_copy(dark_lay, True)
		pdb.gimp_image_add_layer(img, dark_dup_lay, -1)

		clear_dup_lay = pdb.gimp_layer_copy(clear_lay, True)
		pdb.gimp_image_add_layer(img, clear_dup_lay, -1)

		#Creates a temporal HDR layer and a tone map based on it.
		hdr_lay = pdb.gimp_image_merge_down(img, dark_dup_lay, CLIP_TO_IMAGE)
		hdr_lay = pdb.gimp_image_merge_down(img, clear_dup_lay, CLIP_TO_IMAGE)
		tone_map(img, hdr_lay, tm_blur)

		#Removes the HDR layer.
		pdb.gimp_image_remove_layer(img, hdr_lay)
	elif merge == MERGE_HDR:
		hdr_lay = pdb.gimp_image_merge_down(img, dark_lay, CLIP_TO_IMAGE)
		hdr_lay = pdb.gimp_image_merge_down(img, clear_lay, CLIP_TO_IMAGE)
		tone_map(img, hdr_lay, tm_blur)
	elif merge == MERGE_ALL:
		hdr_lay = pdb.gimp_image_merge_down(img, dark_lay, CLIP_TO_IMAGE)
		hdr_lay = pdb.gimp_image_merge_down(img, clear_lay, CLIP_TO_IMAGE)
		tm_lay = tone_map(img, hdr_lay, tm_blur)
		hdr_lay = pdb.gimp_image_merge_down(img, tm_lay, CLIP_TO_IMAGE)
	else:
		return

	pdb.gimp_image_undo_group_end(img)

try:
	from gimpfu import *
	register("HDR",
	         "Simulates a HDR image",
	         "Simulates a High Dynamic Range image using levels",
	         "David Moreno",
	         "2013 David Moreno",
	         "2013",
				"<Image>/Filters/Enhance/_Simulate HDR...",
				"*",
				[
					(PF_SPINNER, "dark", "_Dark", 0.20, (0.10, 10.00, 0.10)),
					(PF_SPINNER, "clear", "_Clear", 1.80, (0.10, 10.00, 0.10)),
					(PF_SPINNER, "blur", "_Blur", 3.0, (0.0, 6400.0, 10.0)),
					(PF_SPINNER, "tm_blur", "_Tone map blur", 100.0, (0.0, 6400.0, 10.0)),
					(PF_OPTION, "merge", "Merge", 1, ["None", "HDR", "All"]),
					(PF_TOGGLE, "tm_only", "Create a tone\nmap only", 0)
				],
				[],
				plugin_main)

	main()

except:
	pass

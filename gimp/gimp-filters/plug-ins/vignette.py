#!/usr/bin/env python

#   Gimp-Python - allows the writing of Gimp plugins in Python.
#   Copyright (C) 2012  Declan Bright <www.declanbright.com>
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.

#   v1.0 - 2012-08-12

from gimpfu import *
g = gimp.pdb

def create_vignette(image, drawable, layer_name, layer_mode, colour, opacity, blur) :
	g.gimp_context_push()
	g.gimp_image_undo_group_start(image)	
	
	# Save the original selection and foreground colour, these will be reset at the end	
	original_selection = g.gimp_selection_save(image)
	original_foreground_colour = g.gimp_context_get_foreground()
	
	w = g.gimp_image_width(image) 
	h = g.gimp_image_height(image) 
	
	g.gimp_progress_update(0.2)
	
	# If the selection is None or All then setup a default selection for the vignette
	is_selection, x1, y1, x2, y2 = g.gimp_selection_bounds(image)
	if (is_selection == FALSE) or (is_selection == TRUE and x1 == 0 and y1 == 0 and x2 == w and y2 == h):
		if (is_selection == TRUE):
			g.gimp_selection_none(image)
		wc = w * 0.05
		hc = h * 0.05
		wp = w + (2 * wc)
		hp = h + (2 * hc)
		g.gimp_image_select_ellipse(image, CHANNEL_OP_ADD, -wc, -hc, wp, hp)
		
	g.gimp_progress_update(0.4)
		
	# Add a new layer for the vignette and set it as the drawable
	vignette_layer = g.gimp_layer_new(image, w, h, RGBA_IMAGE, layer_name, opacity, layer_mode)
	image.add_layer(vignette_layer)
	drawable = vignette_layer
	
	# Set the foreground colour
	g.gimp_context_set_foreground(colour)
	
	g.gimp_progress_update(0.6)		
		
	# Invert the selection and fill with the foreground colur
	g.gimp_selection_invert(image)
	g.gimp_edit_bucket_fill(drawable, FG_BUCKET_FILL, NORMAL_MODE, 100, 255, FALSE, 0, 0)
		
	g.gimp_progress_update(0.8)
	
	# Blur the entire vignette layer
	if blur > 0:
		blur_radius = w * blur / 200 
		g.gimp_selection_all(image)
		g.plug_in_gauss(image, drawable, blur_radius, blur_radius, 1)
		
	# Reset the original selection and foreground colour
	g.gimp_selection_load(original_selection)
	g.gimp_context_set_foreground(original_foreground_colour)	
	
	g.gimp_progress_update(1.0)	
	
	g.gimp_image_undo_group_end(image)	
	g.gimp_context_pop()
	return

register(
    "python_fu_vignette",    
    "Create a vignette around a selection.",   
    "Create a vignette around a selection.\nThe vignette is created as a new layer, the opacity of the vignette can be altered by modifying the opacity of this layer.",
    "Declan Bright", 
    "Declan Bright <www.declanbright.com>", 
    "2012",
    "_Vignette...", 
    "RGB*, GRAY*",
    [	
        (PF_IMAGE, "image", "", None),
        (PF_DRAWABLE, "drawable", "", None),
		(PF_STRING, "layer_name", "_Layer Name", 'Vignette'),
		(PF_RADIO, "layer_mode", "_Mode", SOFTLIGHT_MODE, 
            (
                ("Hard", HARDLIGHT_MODE),
                ("Soft", SOFTLIGHT_MODE)
            )
        ),
        (PF_COLOR, "colour", "Colo_ur", (0.0, 0.0, 0.0)),
		(PF_SLIDER, "opacity", "O_pacity", 30, (0, 100, 5)),
		(PF_SLIDER, "blur", "_Blur", 30, (0, 100, 5))
	], 
    [],
    create_vignette, 
	menu="<Image>/Filters/Light and Shadow"
)

main()

; FU_sharpness-sharper_softfocus.scm
; version 2.8 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/15/2014 on GIMP-2.8.10
;
; Modified 12/18/2007 by Paul Sherman
; made compatible with GIMP-2.4
; (define(s), flatten at end and undo setup)
;
; Wed Oct 1 2008 by Paul S.
;     Modified deprecated to gimp-levels-stretch
;
; Tue Oct 14 2008 by Paul S.
;     Modified to remove passing of image parameter to gimp-layer-add-mask
;     (which only requires the layer and the mask) tested OK on GIMP-2.6
;
; 02/15/2014 - accommodated indexed images, merge option added
;==============================================================
;
; Installation:
; This script should be placed in the user or system-wide script folder.
;
;	Windows Vista/7/8)
;	C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;	or
;	C:\Users\YOUR-NAME\.gimp-2.8\scripts
;	
;	Windows XP
;	C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;	or
;	C:\Documents and Settings\yourname\.gimp-2.8\scripts   
;    
;	Linux
;	/home/yourname/.gimp-2.8/scripts  
;	or
;	Linux system-wide
;	/usr/share/gimp/2.0/scripts
;
;==============================================================
;
; LICENSE
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;==============================================================
; Original information 
; 
; copyright 2006, Y.Morikaku
; August 15, 2006
;==============================================================


(define (FU-soft-focus 
		theImage 
		theLayer
		edge 
		mode 
		opacity
		inMerge
	)

	(gimp-image-undo-group-start theImage)
    (if (not (= RGB (car (gimp-image-base-type theImage))))
			 (gimp-image-convert-rgb theImage))	
	
    (define addString (cond     ((= mode 0) "_super-softfocus_h.jpg" )
                                ((= mode 1) "_super-softfocus_a.jpg" )
                                ((= mode 2) "_super-softfocus_s.jpg" )
                                ((= mode 3) "_super-softfocus_dv.jpg" )
                                ('else "_super-softfocus_ds.jpg" ) ))
    
    (define theLayer2 (car(gimp-layer-copy theLayer 1)))
    (gimp-image-insert-layer theImage theLayer2 0 0)
    (plug-in-gauss-iir2 1 theImage theLayer2 10 10)

    (define theLayer3 (car(gimp-layer-copy theLayer2 1)))
    (gimp-image-insert-layer theImage theLayer3 0 0)
    (define mask (car (gimp-layer-create-mask theLayer3 ADD-COPY-MASK)))
    (gimp-layer-add-mask theLayer3 mask)

    (define theLayer4 (car(gimp-layer-copy theLayer3 1)))
    (gimp-image-insert-layer theImage theLayer4 0 0)

    (gimp-layer-set-mode theLayer2 SCREEN)
    (gimp-layer-set-mode theLayer3 MULTIPLY)
    (gimp-layer-set-mode theLayer4 OVERLAY)

    (define theLayer5 (car(gimp-layer-copy theLayer 1)))
    (gimp-image-insert-layer theImage theLayer5 0 0)
    (define mask5 (car (gimp-layer-create-mask theLayer5 ADD-COPY-MASK)))
    (gimp-layer-add-mask theLayer5 mask5)
    (plug-in-edge 1 theImage mask5 1 1 0)
    (plug-in-blur 1 theImage mask5)
    (gimp-levels-stretch mask5)
    (gimp-layer-set-opacity theLayer5 edge)
    (gimp-edit-copy-visible theImage)
    (define theLayer6 (car (gimp-edit-paste theLayer FALSE)))

    (define pastedLayer (car (gimp-image-get-active-layer theImage)))
    (define pastedLayerMode (cond ((= mode 0) HARDLIGHT-MODE )
                                ((= mode 1) ADDITION )
                                ((= mode 2) SCREEN )
                                ((= mode 3) DIVIDE )
                                ('else NORMAL ) ))
    (gimp-layer-set-mode pastedLayer pastedLayerMode )
    (if (>= mode 4)
        (begin 
            (gimp-desaturate pastedLayer)
            (define maskPasted (car (gimp-layer-create-mask pastedLayer ADD-COPY-MASK)))
            (gimp-layer-add-mask pastedLayer maskPasted )
        );end of begin
    );end of if 
    (gimp-layer-set-opacity pastedLayer opacity )    
    
	(if (= inMerge TRUE)(gimp-image-merge-visible-layers theImage EXPAND-AS-NECESSARY))
	(gimp-image-undo-group-end theImage)
	(gimp-displays-flush)

    
     
);end of define

(script-fu-register "FU-soft-focus"
    "<Image>/Script-Fu/Sharpness/Softer/Soft Focus"
    "Super Softfocus 1.1 / Layer mode select version.\n\nFlattens image if needed before doing its work."
    "Y Morikaku"
    "copyright 2006, Y.Morikaku"
    "August 15, 2006"
    "*"
    SF-IMAGE      "Image"     						0
    SF-DRAWABLE   "Drawable"  						0
    SF-ADJUSTMENT "Edge Strength"   				'(50 0 100 1 10 0 0)
    SF-OPTION     "Top Layer Mode" 					'( "HARDLIGHT-MODE" "ADDITION" "SCREEN" "DIVIDE" "Desaturation" )
    SF-ADJUSTMENT "Top Layer Opacity"   			'(30 0 100 1 10 0 0)
	SF-TOGGLE     "Merge layers when complete?" 	TRUE
)

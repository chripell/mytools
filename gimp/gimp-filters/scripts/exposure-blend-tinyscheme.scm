;; exposure-blend.scm: Blend 3 bracketed exposures.
;; Copyright (C) 2006 by J.D. Smith <jdsmith@as.arizona.edu>
;;
;; Version 1.3b (Mar, 2007)
;;
;; http://turtle.as.arizona.edu/jdsmith/exposure_blend.php
;;
;; Exposure Blend: Prompt for 3 images in a bracketed exposure series
;; (e.g. 0,-2,+2 EV), and blend these into a contrast enhanced image,
;; roughly based on the GIMP masking prescription of Daniel Schwen:
;;
;;   http://www.schwen.de/wiki/Exposure_blending.
;;
;; Also, provides an image alignment mode, layer overlap cropping, and
;; several options for setting blend masks.  Smoothed masks are cached
;; for quick recovery, and any of the three images can be used as a
;; mask for any layer.
;;
;; Version 1.3b: - Converted by Alan Stewart to work with the new
;;                 TinyScheme scripting system of Gimp v2.3 and later.
;; Version 1.3:  - Fixed accumulating shift mismatch issues with cached masks.
;;               - New tattoo labelling scheme for cached masks.
;;               - Added "edge protection" options using selective
;;                 Gaussian blur.
;; Version 1.2:  - First release into the wild
;;
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2006 J.D. Smith
;
;  exposure-blend is free software; you can redistribute it and/or
;  modify it under the terms of the GNU General Public License as
;  published by the Free Software Foundation; either version 2, or (at
;  your option) any later version.
;
;  exposure-blend is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with exposure-blend; see the file COPYING.  If not, write to
;  the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;  Floor, Boston, MA 02110-1301, USA.
;
;##############################################################################


;; Common parameters
(define script-fu-exposure-blend-options
      (list SF-ADJUSTMENT _"Blend Mask Blur Radius" '(8 1 1024 1 15 0 1)
	    SF-OPTION     _"Blur Type/Edge Protection"
	    '("Gaussian/None" "Selective/Low" "Selective/Medium" "Selective/High")
	    SF-OPTION     _"Dark Mask Grayscale" '("Dark" "Normal" "Bright")
	    SF-OPTION     _"Bright Mask Grayscale" '("Bright (inverted)"
						     "Normal (inverted)"
						     "Dark (inverted)")
        SF-TOGGLE     _"Dark Takes Precedence"      FALSE
        SF-TOGGLE     _"Auto-Trim Mask Histograms"  FALSE))

(define script-fu-exposure-blend-copyright
      (list "J.D. Smith <jdsmith@as.arizona.edu>"
	    "J.D. Smith"
	    "2006"))

;; Tattoo constants
(define EXP-IM-TYPE-MASK 4)              ; || 00 0000000000
(define EXP-IM-TYPE-OFF  131072)

(define EXP-BLUR-THRESHOLD-MASK 4)   ; 00 || 0000000000
(define EXP-BLUR-THRESHOLD-OFF  524288)

(define EXP-BLUR-RAD-MASK 1024)          ; 00 00 ||||||||||
(define EXP-BLUR-RAD-OFF 2097152)

;; Image Types values
(define EXP-OFFSET 32768)
(define EXP-NORMAL (+ EXP-OFFSET 0))
(define EXP-DARK   (+ EXP-OFFSET 1))
(define EXP-BRIGHT (+ EXP-OFFSET 2))

;; blend -- Main entry script for selecting dark/normal/bright files
;;          and configuring blend options.
(define (script-fu-exposure-blend img_f img_dark_f img_bright_f blur-rad
				  blur-thresh mask-dark mask-bright
				  dark-precedence auto-trim scale-image)
  (let* ((img (car (gimp-file-load RUN-INTERACTIVE img_f img_f)))
	 (img-dark (car (gimp-file-load RUN-INTERACTIVE
					img_dark_f img_dark_f)))

	 (img-bright (car (gimp-file-load RUN-INTERACTIVE
					  img_bright_f img_bright_f)))

	 (layer  (aref (cadr (gimp-image-get-layers img)) 0))
	 (layer-dark (car (gimp-layer-new-from-drawable
			   (aref
			    (cadr (gimp-image-get-layers img-dark)) 0)
			   img)))
	 (layer-bright (car (gimp-layer-new-from-drawable
			     (aref
			      (cadr (gimp-image-get-layers img-bright)) 0)
			     img)))
     (scale '()) (width '()) (height '()))

    (gimp-context-push)
    (gimp-image-undo-disable img)

    (gimp-image-delete img-dark)
    (gimp-image-delete img-bright)

    ;; Setup Normal Image
    (gimp-drawable-set-name
     layer
     (string-append "Normal Exp: "
		    (car (last (strbreakup img_f "/")))))
    (gimp-drawable-set-tattoo layer EXP-NORMAL)

    ;; Setup Bright Image
    (gimp-image-add-layer img layer-bright -1)
    (gimp-drawable-set-name
     layer-bright
     (string-append "Bright Exp: "
		    (car (last (strbreakup img_bright_f "/")))))
    (gimp-drawable-set-tattoo layer-bright EXP-BRIGHT)
    (gimp-layer-add-alpha layer-bright)

    ;; Setup Dark Image
    (gimp-image-add-layer img layer-dark (if (= dark-precedence TRUE) -1 1))
    (gimp-drawable-set-name
     layer-dark
     (string-append "Dark Exp: "
		    (car (last (strbreakup img_dark_f "/")))))
    (gimp-drawable-set-tattoo layer-dark EXP-DARK)
    (gimp-layer-add-alpha layer-dark)

    ;; Scale layers
    (if (not (equal? scale-image ""))
	(begin
	  (set! width (car (gimp-drawable-width layer)))
	  (set! height (car (gimp-drawable-height layer)))
	  (set! scale (/ (string->number scale-image) (max width height)))

	  ;; Re-normalize blur-rad
;; 	  (set! blur-rad (max 2 (+ 1 (trunc (* blur-rad scale)))))
;; 	  (gimp-message (string-append "Blur radius rescaled to "
;;				       (number->string blur-rad)))
      (gimp-layer-scale layer (* width scale) (* height scale) FALSE)
	  (gimp-layer-scale layer-dark
			    (* (car (gimp-drawable-width  layer-dark)) scale)
			    (* (car (gimp-drawable-height layer-dark)) scale)
                FALSE)
	  (gimp-layer-scale layer-bright
			    (* (car (gimp-drawable-width  layer-bright)) scale)
			    (* (car (gimp-drawable-height layer-bright)) scale)
                FALSE)
	  (gimp-image-resize-to-layers img)))

    ;; Set blend masks
    (script-fu-exposure-blend-set-masks img '() blur-rad blur-thresh
					mask-dark mask-bright
                    dark-precedence auto-trim FALSE)

    ;; Finish
    (gimp-image-undo-enable img)
    (gimp-display-new img)
    (gimp-context-pop)))

(apply script-fu-register
       (append
	(list
	 "script-fu-exposure-blend"
	 _"Exposure Blend..."
	 "Blend Bracketed Exposures")
	script-fu-exposure-blend-copyright
	(list
	 ""
	 SF-FILENAME   _"Normal Exposure"            "normal.jpg"
	 SF-FILENAME   _"Short Exposure (Dark)"      "dark.jpg"
	 SF-FILENAME   _"Long Exposure (Bright)"     "bright.jpg")
	script-fu-exposure-blend-options
	(list SF-STRING _"Scale Largest Image Dimension to"       "")))

(script-fu-menu-register "script-fu-exposure-blend"
                         _"<Toolbox>/Xtns/Photo")


;; Tattoo parsing

;; compose-tattoo -- create tattoo from image type, blur type,
;;                   selective blur threshold, and blur radius
(define (script-fu-exposure-blend-compose-tattoo img-type blur-rad blur-thresh)
  (+  (* (- img-type EXP-OFFSET) EXP-IM-TYPE-OFF)
      (* blur-thresh EXP-BLUR-THRESHOLD-OFF)
      (* blur-rad        EXP-BLUR-RAD-OFF)))

;; decompose-tattoo
(define (script-fu-exposure-blend-decompose-tattoo tattoo)
  (list
   (+ EXP-OFFSET
      (modulo (quotient tattoo EXP-IM-TYPE-OFF) ; im type
	       EXP-IM-TYPE-MASK))
   (modulo (quotient tattoo EXP-BLUR-RAD-OFF) ; blur-rad
	    EXP-BLUR-RAD-MASK)
   (modulo (quotient tattoo EXP-BLUR-THRESHOLD-OFF) ; selective blur thresh
	    EXP-BLUR-THRESHOLD-MASK)))

;; name -- Return a name matching the given exposure type
(define (script-fu-exposure-blend-name type)
  (cond
   ((= type EXP-NORMAL)   "Normal Exp")
   ((= type EXP-BRIGHT)   "Bright Exp")
   ((= type EXP-DARK)     "Dark Exp")
   ('else                 "Unknown")))

(define (script-fu-exposure-blend-copy from to offset)
  (let* ((float (begin
		  (gimp-edit-copy from)
          (car (gimp-edit-paste to FALSE))))
	 (offs (gimp-drawable-offsets from)))
    (if (and (equal? offset 'offset) (or (not (= (car offs) 0)) (not (= (cadr offs) 0))))
	(gimp-layer-set-offsets float (car offs) (cadr offs)))
    (gimp-floating-sel-anchor float)))


;; mask -- Locate an appropriate mask from the mask cache, or create
;;         and blur a new mask with given RADIUS TYPE
;;         (e.g. EXP-NORMAL), and blur threshold (for selective
;;         blurring). If REGEN is non-nil, regenerate the masks, even
;;         if cached (useful if shifted).
(define (script-fu-exposure-blend-mask img layer type blur-rad blur-thresh regen)
  (let* ((img-channels (gimp-image-get-channels img))
	 (num-channels (car img-channels))
	 (channels (cadr img-channels))
	 (target-tattoo (script-fu-exposure-blend-compose-tattoo
			 type blur-rad blur-thresh))
	 (source-layer (car (gimp-image-get-layer-by-tattoo img type)))

	 (cnt 0)
	 (channel
      (catch
		  (while (< cnt num-channels)
			 (set! channel (aref channels cnt))
			 (if (= (car (gimp-drawable-get-tattoo channel))
				target-tattoo)
                 (throw channel))
             (set! cnt (+ 1 cnt)))))
	 (mask (car (gimp-layer-get-mask layer)))
	 float)

    ;; Do we have a mask already in place?
    (if (= mask -1)
	(begin
	  (set! mask (car (gimp-layer-create-mask layer ADD-WHITE-MASK)))
	  (gimp-layer-add-mask layer mask)))

    (if (or (null? channel) regen)
	;; We must create and store a new channel from the source layer
	(begin
      (if (not (null? channel))
	      (gimp-image-remove-channel img channel))
	  ;; Create a new channel for this combo
	  (let* ((width  (car (gimp-drawable-width layer)))
		 (height (car (gimp-drawable-height layer)))
		 (name (string-append
			(script-fu-exposure-blend-name type)
			" ("
			(number->string blur-rad)
			"pix "
			(if (= blur-thresh 0)
			    "gauss"
			    (string-append "edges "
					   (cond
					    ((= blur-thresh 1) "low")
					    ((= blur-thresh 2) "med")
					    ((= blur-thresh 3) "high"))))
			")")))
	    (set! channel
		  (car (gimp-channel-new img width height name 100 '(0 0 0)))))

	  ;; Copy the layer to a channel and mark with tattoo ID
      (gimp-drawable-set-visible channel FALSE)
	  (gimp-drawable-set-tattoo  channel target-tattoo)
	  (gimp-image-add-channel img channel -1)
	  (script-fu-exposure-blend-copy source-layer channel 'offset)

	  ;; Blur the channel image
	  (if (= blur-thresh 0)
	      (plug-in-gauss-iir RUN-NONINTERACTIVE img channel
                 blur-rad TRUE TRUE)
	      (plug-in-sel-gauss RUN-NONINTERACTIVE img channel
				 blur-rad
				 (cond
				  ((= blur-thresh 1) 100)
				  ((= blur-thresh 2) 30)
				  ((= blur-thresh 3) 10))))))
    ;; Copy the channel's data over
    (script-fu-exposure-blend-copy channel mask FALSE)
    mask))

;; set-masks -- (Re)set the masks as requested.  Also called
;;              interactively, and by the main level "blend" routine.
(define (script-fu-exposure-blend-set-masks img draw blur-rad blur-thresh
					    mask-dark mask-bright
					    dark-precedence
					    auto-trim regen)
  (let* ((img-layers (gimp-image-get-layers img))
	 (num-layers (car img-layers))
	 (dark-mask-type
	  (cond ((= mask-dark 0) EXP-DARK)
		((= mask-dark 1) EXP-NORMAL)
		((= mask-dark 2) EXP-BRIGHT)))
	 (bright-mask-type ;reversed order
	  (cond ((= mask-bright 0) EXP-BRIGHT)
		((= mask-bright 1) EXP-NORMAL)
		((= mask-bright 2) EXP-DARK)))
     (regen (= regen TRUE))
     (dark-precedence (= dark-precedence TRUE))
     (auto-trim (= auto-trim TRUE))
	 (layers (cadr img-layers))
     (cnt 0)
     (layer '()) (mask '()) (tattoo '()) (dark '()) (bright '()))

    (gimp-context-push)
    (gimp-image-undo-group-start img)

    (while (< cnt num-layers)
	   (set! layer (aref layers cnt))
	   (set! tattoo (car (gimp-drawable-get-tattoo layer)))
	   (set! dark   (= tattoo EXP-DARK))
	   (set! bright (= tattoo EXP-BRIGHT))

	   (if (or dark bright)
	       (begin
		 ;; Ensure appropriate layer is on top
		 (if (or (and dark dark-precedence)
			 (and bright (not dark-precedence)))
		     (begin
		       (if (not (= cnt 0))
			   (gimp-image-raise-layer-to-top img layer))
		       (gimp-image-set-active-layer img layer)))


		 ;; Setup and blur the mask (or recover from channel cache)
		 (set! mask
		       (script-fu-exposure-blend-mask
			img layer
			(if bright bright-mask-type dark-mask-type)
			blur-rad blur-thresh regen))

		 ;; Stretch the mask, if requested
		 (if auto-trim (gimp-levels-stretch mask))

		 ;; Invert the bright mask
		 (if bright (gimp-invert mask))))
	   (set! cnt (+ cnt 1)))

    (gimp-image-undo-group-end img)
    (gimp-context-pop)
    (gimp-displays-flush)))

(apply script-fu-register
       (append
	(list
	 "script-fu-exposure-blend-set-masks"
	 _"Set Blend Masks..."
	 _"Set Blended Exposure Masks")
	script-fu-exposure-blend-copyright
	(list
	 "RGB* GRAY*"
	 SF-IMAGE   _"IMAGE"            0
	 SF-DRAWABLE    "(unused) Drawable" 0)
	script-fu-exposure-blend-options
	(list
     SF-TOGGLE  _"Regenerate Masks"  FALSE)))

(script-fu-menu-register "script-fu-exposure-blend-set-masks"
                         _"<Image>/Filters/Photo")


;; link-channels -- Linking all channel masks associated with a given
;;                  layer type, so they move together (for alignment)
(define (script-fu-exposure-blend-link-channels img type)
  (let* ((img-channels (gimp-image-get-channels img))
	 (num-channels (car img-channels))
	 (channels (cadr img-channels))
     (cnt 0)
     (tattoo '()) (channel '())(mask-type '()))
    (while (< cnt num-channels)
	   (set! channel (aref channels cnt))
	   (set! tattoo (car (gimp-drawable-get-tattoo channel)))
	   (set! mask-type
		 (car (script-fu-exposure-blend-decompose-tattoo tattoo)))
	    ;; Link if it belongs to this exposure type
	   (gimp-drawable-set-linked
	    channel
        (if (or (and (equal? type 'bright) (= mask-type EXP-BRIGHT))
            (and (equal? type 'dark)   (= mask-type EXP-DARK)))
        TRUE FALSE))
	   (set! cnt (+ 1 cnt)))))

;; align -- Enter or exit the layer differencing alignment mode
;;          ALIGN-SET ('dark 'bright 'off).
(define (script-fu-exposure-blend-align img align-set)
  ;; Set the alignment mode.
  (let* ((img-layers (gimp-image-get-layers img))
	 (num-layers (car img-layers))
	 (layers (cadr img-layers))
     (cnt 0)
     (tattoo '())(layer '())(dark '())(bright '())(do-align '()))
    (gimp-image-undo-group-start img)
    (while (< cnt num-layers)
	   (set! layer (aref layers cnt))
	   (set! tattoo (car (gimp-drawable-get-tattoo layer)))
	   (set! dark (= tattoo EXP-DARK))
	   (set! bright (= tattoo EXP-BRIGHT))
       (set! do-align (or (and dark   (equal? align-set 'dark))
                  (and bright (equal? align-set 'bright))))
       (gimp-drawable-set-visible
        layer (if (or (equal? align-set 'off)  ; show everything when off
			  (= tattoo EXP-NORMAL)
			  do-align)
              TRUE FALSE))

       (gimp-drawable-set-linked layer FALSE) ; no links by default

	   (if (or dark bright)
	       (begin
		 (gimp-layer-set-mode layer (if do-align
						DIFFERENCE-MODE NORMAL-MODE))
         (gimp-layer-set-apply-mask layer (if do-align FALSE TRUE))
		 (if do-align
		     (begin
		       (gimp-layer-set-opacity layer 100)
               (gimp-drawable-set-linked layer TRUE)
               (gimp-layer-set-edit-mask layer FALSE)
		       (gimp-image-set-active-layer img layer)))))
	   (set! cnt (+ cnt 1)))
    ;; Link the channels with this layer.
    (script-fu-exposure-blend-link-channels img align-set)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)))

(define (script-fu-exposure-blend-align-off img draw)
  (script-fu-exposure-blend-align img 'off))
(define (script-fu-exposure-blend-align-bright img draw)
  (script-fu-exposure-blend-align img 'bright))
(define (script-fu-exposure-blend-align-dark img draw)
  (script-fu-exposure-blend-align img 'dark))

(map
 (lambda (x)
   (apply script-fu-register
	  (append
	   (list
	    (string-append "script-fu-exposure-blend-align-" x)
        (string-append "Align Exposure Mode: "  x)
        (string-append "Differencing Mode for Aligning Exposures: " x))
	   script-fu-exposure-blend-copyright
	   (list
	    "RGB* GRAY*"
	    SF-IMAGE      _"IMAGE"             0
	    SF-DRAWABLE    "(unused) Drawable" 0)))
   (script-fu-menu-register
    (string-append "script-fu-exposure-blend-align-" x)
    _"<Image>/Filters/Photo"))
 '("dark" "bright" "off"))

;; crop-image - crop the image to the intersection region of all the
;;              layers.
(define (script-fu-exposure-blend-crop-image img draw)
  (let* ((img-layers (gimp-image-get-layers img))
	 (num-layers (car img-layers))
	 (layers (cadr img-layers))
 	 (cnt 0)
     (x '())(y '())(layer '())(offs '())(xt '())(yt '())(x2 '())(y2 '()))
    (while (< cnt num-layers)
	   (set! layer (aref layers cnt))
	   (set! offs (gimp-drawable-offsets layer))
       (set! x (if (not (null? x)) (max x (car offs)) (car offs)))
       (set! y (if (not (null? y)) (max y (cadr offs)) (cadr offs)))
	   (set! xt (- (+ (car (gimp-drawable-width layer)) (car offs)) 1))
	   (set! yt (- (+ (car (gimp-drawable-height layer)) (cadr offs)) 1))
       (set! y2 (if (not (null? y2)) (min y2 yt) yt))
       (set! x2 (if (not (null? x2)) (min x2 xt) xt))
	   (set! cnt (+ cnt 1)))
    (gimp-message (string-append "Trimmed image to: "
				 (number->string (+ (- x2 x) 1))
				 "x"
				 (number->string (+ (- y2 y) 1))
				 " ["
				 (number->string x)
				 ", "
				 (number->string y) "]"))
    (gimp-image-crop img (+ (- x2 x) 1) (+ (- y2 y) 1) x y)))

(apply script-fu-register
       (append
	(list
	 "script-fu-exposure-blend-crop-image"
	 _"Trim Image to Overlap Area"
	 _"Trim image to combined layer overlap")
	script-fu-exposure-blend-copyright
	(list
	 "RGB* GRAY*"
	 SF-IMAGE      _"IMAGE"             0
	 SF-DRAWABLE    "(unused) Drawable" 0)))

(script-fu-menu-register
 "script-fu-exposure-blend-crop-image"
 _"<Image>/Filters/Photo")


;; save-mask -- save the mask of the current layer to a channel
(define (script-fu-exposure-blend-save-mask img draw)
  (let* ((layer (car (gimp-image-get-active-layer img)))
	 (mask (car (gimp-layer-get-mask layer)))
	 (width  (car (gimp-drawable-width layer)))
	 (height (car (gimp-drawable-height layer)))
	 (name (string-append "EB: " (car (gimp-drawable-get-name layer))))
	 (channel (car (gimp-channel-new img width height name 100 '(0 0 0)))))
    (gimp-drawable-set-visible channel FALSE)
    (gimp-image-add-channel img channel -1)
    (script-fu-exposure-blend-copy mask channel FALSE)))

(apply script-fu-register
       (append
	(list
	 "script-fu-exposure-blend-save-mask"
	 _"Save Layer's Mask as Channel"
	 _"Save the mask of the current layer as a new channel")
	script-fu-exposure-blend-copyright
	(list
	 "RGB* GRAY*"
	 SF-IMAGE      _"IMAGE"             0
	 SF-DRAWABLE    "(unused) Drawable" 0)))

(script-fu-menu-register
 "script-fu-exposure-blend-save-mask"
 _"<Image>/Filters/Photo/Masks")

;; apply-mask -- apply the first *visible* channel in the channel list
;;               as mask to the selected layer
(define (script-fu-exposure-blend-apply-mask img draw)
  (let* ((layer (car (gimp-image-get-active-layer img)))
	 (mask (if (not (= layer -1)) (car (gimp-layer-get-mask layer))))
	 (img-channels (gimp-image-get-channels img))
	 (num-channels (car img-channels))
	 (channels (cadr img-channels))
	 (cnt 0)
	 (channel
      (catch
		  (while (< cnt num-channels)
			 (if (= (car (gimp-drawable-get-visible
                      (aref channels cnt))) TRUE)
                 (throw (aref channels cnt)))
			 (set! cnt (+ 1 cnt))))))
    (gimp-image-undo-group-start img)
    (if (= layer -1)
	(gimp-message "No layer selected, aborting.")
	(if channel
	    (begin
	      (if (= mask -1)
		  (begin
		    (set! mask
			  (car (gimp-layer-create-mask layer ADD-WHITE-MASK)))
		    (gimp-layer-add-mask layer mask)))
          (script-fu-exposure-blend-copy channel mask FALSE)
          (gimp-drawable-set-visible channel FALSE)
	      (gimp-displays-flush))
	    (gimp-message "No visible channel found, aborting.")))
    (gimp-image-undo-group-end img)))

(apply script-fu-register
       (append
	(list
	 "script-fu-exposure-blend-apply-mask"
	 _"Apply First Visible Channel as Layer Mask"
	 _"Use the first visible channel as the mask for the selected layer")
	script-fu-exposure-blend-copyright
	(list
	 "RGB* GRAY*"
	 SF-IMAGE      _"IMAGE"             0
	 SF-DRAWABLE    "(unused) Drawable" 0)))

(script-fu-menu-register
 "script-fu-exposure-blend-apply-mask"
 _"<Image>/Filters/Photo/Masks")

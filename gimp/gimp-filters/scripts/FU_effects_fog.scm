; FU_effects_fog.scm
; version 2.8 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/14/2014 on GIMP-2.8.10
;
; 02/14/2014 - convert to RGB if needed, option to merge layers
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
; fog script-fu,
; original by ~kward1979uk (on Deviantart, see url below:)
; http://kward1979uk.deviantart.com/art/fog-script-fu-42504446?q=boost%3Apopular%20in%3Aresources%2Fapplications%2Fgimpactions&qo=72
; 
;==============================================================


(define (FU-fog 
		inImage 
		inLayer 
		color 
		turb 
		opac 
		inMerge
		)
	(gimp-image-undo-group-start inImage)
	(if (not (= RGB (car (gimp-image-base-type inImage))))
			 (gimp-image-convert-rgb inImage))
	(define flat (car (gimp-image-flatten inImage)))
	(gimp-context-set-background color)
	(define width (car (gimp-drawable-width flat)))
	(define height (car (gimp-drawable-height flat)))
	(define white-layer (car (gimp-layer-new inImage width height 1 "white" opac 0)))
	(gimp-drawable-fill white-layer 1)
	(gimp-image-add-layer inImage white-layer -1)

	(define mask (car (gimp-layer-create-mask white-layer 0)))
	(gimp-layer-add-mask white-layer mask)
	(plug-in-plasma 1 inImage mask (rand 4294967295) turb)

	(if (= inMerge TRUE)(gimp-image-merge-visible-layers inImage EXPAND-AS-NECESSARY))
	(gimp-image-undo-group-end inImage)
	(gimp-displays-flush)

); end DEFINE

(script-fu-register "FU-fog"
	"<Image>/Script-Fu/Effects/Fog"
	"Applies a fog of chosen colour over image"
	"Karl Ward"
	"Karl Ward"
	"Nov 2006"
	"*"
	SF-IMAGE      	"SF-IMAGE" 						0
	SF-DRAWABLE   	"SF-DRAWABLE" 					0
	SF-COLOR     	"Fog Colour" 					'(255 255 255)
	SF-ADJUSTMENT 	"Turbulance" 					'(1.0 0 10 0.1 1 1 0)
	SF-ADJUSTMENT 	"Opacity" 						'(100 0 100 1 5 1 0)
	SF-TOGGLE     	"Merge layers when complete?" 	FALSE
)
				
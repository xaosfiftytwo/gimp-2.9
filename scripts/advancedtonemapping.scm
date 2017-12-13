;; From Gimp Plugin Registry: http://registry.gimp.org/node/5980
;;
;; This Advanced Tone Mapping is a script for The GIMP
;;
;; Reduce global contrast while increasing local contrast and shadow/highlight
;; detail.
;;
;; Advanced Tone Mapping is a fork/continuation of Tonemapping script
;; originally written by David Meiklejohn and Harry Phillips.
;; The 2.0 version was created by Vit 'tasuki' Brunner.
;;
;; The script is located in menu "<Image> / Filters / Enhance"
;; Last changed: 13 June 2008
;;
;; --------------------------------------------------------------------
;;  
;; Changelog:
;;  Version 3.0 ('xaos52', December 2017)
;;    - Made the script compatible with GIMP 2.9
;;    - Consistent indentation
;;    - Renamed variables
;;
;;  Version 2.0 codename 'tasuki' (Friday the 13th, June 2008)
;;    - Moved the script to Filters/Enhance where it IMHO belongs
;;    - Removed result flattening
;;    - Changed the amount of Gauss-blur to be in % of image size
;;    - Added option to set the original Gauss-blurred layer's opacity
;;      (default staying 75)
;;    - Added option to set the number of times the mergedLayer layer is copied
;;    - The original layer is preserved. The script adds one non-transparent
;;      layer with all the changes in it, which is named according to
;;      the parameters the script was called with (looks messy but is handy)
;;
;;  Version 1.4 (8th August 2007)
;;    - Added option to flatten the result
;;
;;  Version 1.3 (5th August 2007)
;;    - Added GPL3 licence 
;;    - Menu location at the top of the script
;;    - Removed the "script-fu-menu-register" section
;; 
;;  Version 1.2
;;    - Made the script compatible with GIMP 2.3
;;
;; --------------------------------------------------------------------
;;
;; This program is free software;; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation;; either version 3 of the License, or
;; (at your option) any later version.


(define (my-duplicate-layer image layer)
  (let ((dupLayer (car (gimp-layer-copy layer 0))))
    (gimp-image-insert-layer image dupLayer 0 0)
    dupLayer))

(define (script-fu-advanced-tonemapping
         inImage
         inLayer
         inBlurPercent
         inOpacityBlurPercent
         inOpacityMergePercent
         inStrength)


  (let (
        (theImage inImage)
        (theLayer inLayer)
        (theBlurPercent inBlurPercent)
        (theOpacityBlurPercent inOpacityBlurPercent)
        (theOpacityMergePercent inOpacityMergePercent)
        (theStrength 0)
        (theBlurAmount 0)
        (dupLayer 0)
        )
    
                                        ;Start an undo group so the process can be undone with one undo
    (gimp-image-undo-group-start inImage)
                                        ;Create temporary layers
                                        ;We only need a reference to the last duplicate layer
    (set! dupLayer (my-duplicate-layer theImage theLayer))
    (set! dupLayer (my-duplicate-layer theImage theLayer))
    (set! dupLayer (my-duplicate-layer theImage theLayer))
    
                                        ;Apply the desaturate and invert to the top layer
    (gimp-drawable-desaturate dupLayer DESATURATE-LUMINANCE)
    (gimp-drawable-invert dupLayer FALSE)

                                        ;Apply the blur with the supplied blur amount (in percents!)
                                        ;(width + height) / 200 * theBlurPercent
    (set! theBlurAmount (round (* theBlurPercent (/ (+ (car (gimp-drawable-width theLayer))
                                                (car (gimp-drawable-height theLayer))) 200))))
    (plug-in-gauss-rle RUN-NONINTERACTIVE theImage dupLayer theBlurAmount TRUE TRUE)

                                        ;Set the layer's opacity
    (gimp-layer-set-opacity dupLayer theOpacityBlurPercent)

                                        ;Merge the top layer down and keep track of the newly mergedLayer layer
    (let
        (
         (mergedLayer (car (gimp-image-merge-down theImage dupLayer EXPAND-AS-NECESSARY)))
         (theStrength 0)
         )

                                        ;Change the mergedLayer layers mode to SOFT LIGHT (19)
      ;; (gimp-layer-set-mode mergedLayer 19)
      (gimp-layer-set-mode mergedLayer LAYER-MODE-SOFTLIGHT)

                                        ;Change the mergedLayer layers opacity
      (gimp-layer-set-opacity mergedLayer theOpacityMergePercent)

                                        ;Copy and merge the layer the necessary number of times
      (set! theStrength 1)
      (while (< theStrength inStrength)
             (let ((tmpLayer (my-duplicate-layer theImage mergedLayer)))
               (gimp-image-lower-item theImage tmpLayer)
               (gimp-image-merge-down theImage tmpLayer EXPAND-AS-NECESSARY)
               (set! theStrength (+ theStrength 1))))

                                        ;Merge the last layer down
      (let ((finalLayer (car (gimp-image-merge-down theImage mergedLayer EXPAND-AS-NECESSARY))))
                                        ;Name the layer so that we can see the parameters in its name
                                        ;We show the blur amount in stead of the blur percentage
        (gimp-item-set-name finalLayer
                            (string-append
                             "tonemapped with parameters: "
                             (number->string theBlurAmount)
                             ", "
                             (number->string theOpacityBlurPercent)
                             ", "
                             (number->string theOpacityMergePercent)
                             ", "
                             (number->string theStrength)))
        )
      )

                                        ;Finish the undo group for the process
    (gimp-image-undo-group-end theImage)

                                        ;Ensure the updated image is displayed now
    (gimp-displays-flush)
    )
  )

(script-fu-register "script-fu-advanced-tonemapping"
                    _"_Advanced Tonemapping..."
                    "Performs a tone mapping operation with a specified blur on the open image"
                    "David Meiklejohn, Harry Phillips, Vit 'tasuki' Brunner"
                    "2006-2008, David Meiklejohn, Harry Phillips, Vit 'tasuki' Brunner"
                    "2008-06-13"
                    "RGB* GRAY*"
                    SF-IMAGE		"The image"                           0
                    SF-DRAWABLE		"The layer"                           0
                    SF-ADJUSTMENT	_"Gauss. Blur (Percent of img size)"  '(10 1 100 1 10 0 0)
                    SF-ADJUSTMENT	_"Opacity of blurred layer (Percent)" '(75 0 100 1 10 0 0)
                    SF-ADJUSTMENT	_"Opacity of merged layer (Percent)"  '(90 0 100 1 10 0 0)
                    SF-ADJUSTMENT	_"Strength (1-10)"                    '(1 1 10 1 10 0 0)
                    )

(script-fu-menu-register "script-fu-advanced-tonemapping"
                         "<Image>/Filters/Enhance"
                         )

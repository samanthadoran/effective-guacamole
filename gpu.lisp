(defpackage #:psx-gpu
  (:nicknames #:gpu)
  (:use :cl
        :cepl
        :cepl.sdl2
        :swank
        :livesupport
        :cepl.skitter.sdl2
        :cepl.devil
        :memory)
  (:export #:gpu #:make-gpu #:gpu-gpu-stat #:gpu-stat-to-word
           #:read-gpu #:write-gpu #:gpu-exception-callback))

(in-package :psx-gpu)
(declaim (optimize (speed 3) (safety 1)))

(defparameter *debug-gpu* nil)

; TODO(Samantha): Convert (unsigned-byte 1) to boolean when it makes sense.
(defstruct gpu-stat
  (texture-page-x-base 0 :type (unsigned-byte 4))
  (texture-page-y-base 0 :type (unsigned-byte 1))
  (semi-transparency 0 :type (unsigned-byte 2))
  (texture-page-colors 0 :type (unsigned-byte 2))
  (dither-24-to-15-bit 0 :type (unsigned-byte 1))
  (draw-to-display-area 0 :type (unsigned-byte 1))
  (set-mask-bit 0 :type (unsigned-byte 1))
  (draw-pixels 0 :type (unsigned-byte 1))
  (interlace-field 0 :type (unsigned-byte 1))
  ; According to nocash, this bit just causes strange effects on the
  ; display on real hardware; ignore?
  (reverse-flag 0 :type (unsigned-byte 1))
  (texture-disable 0 :type (unsigned-byte 1))
  (horizontal-resolution-2 0 :type (unsigned-byte 1))
  (horizontal-resolution-1 0 :type (unsigned-byte 2))
  (vertical-resolution 0 :type (unsigned-byte 1))
  (video-mode 0 :type (unsigned-byte 1))
  (display-area-color-depth 0 :type (unsigned-byte 1))
  (vertical-interlace 0 :type (unsigned-byte 1))
  (display-disabled 0 :type (unsigned-byte 1))
  (irq1 0 :type (unsigned-byte 1))
  (dma 0 :type (unsigned-byte 1))
  ; Set to avoid a hang in the bios
  (ready-to-receive-command 1 :type (unsigned-byte 1))
  (ready-to-send-vram-to-cpu 1 :type (unsigned-byte 1))
  (ready-to-receive-dma-block 1 :type (unsigned-byte 1))
  (dma-direction 0 :type (unsigned-byte 2))
  (even-odd-line 0 :type (unsigned-byte 1)))

(declaim (ftype (function (gpu-stat) (unsigned-byte 32)) gpu-stat-to-word))
(defun gpu-stat-to-word (gpu-stat)
  (logior
   (gpu-stat-texture-page-x-base gpu-stat)
   (ash (gpu-stat-texture-page-y-base gpu-stat) 4)
   (ash (gpu-stat-semi-transparency gpu-stat) 5)
   (ash (gpu-stat-texture-page-colors gpu-stat) 7)
   (ash (gpu-stat-dither-24-to-15-bit gpu-stat) 9)
   (ash (gpu-stat-draw-to-display-area gpu-stat) 10)
   (ash (gpu-stat-set-mask-bit gpu-stat) 11)
   (ash (gpu-stat-draw-pixels gpu-stat) 12)
   (ash (gpu-stat-interlace-field gpu-stat) 13)
   (ash (gpu-stat-reverse-flag gpu-stat) 14)
   (ash (gpu-stat-texture-disable gpu-stat) 15)
   (ash (gpu-stat-horizontal-resolution-2 gpu-stat) 16)
   (ash (gpu-stat-horizontal-resolution-1 gpu-stat) 17)
   ; TODO(Samantha): Remove this hack once we properly emulate bit31
   (logand 0 (ash (gpu-stat-vertical-resolution gpu-stat) 19))
   (ash (gpu-stat-video-mode gpu-stat) 20)
   (ash (gpu-stat-display-area-color-depth gpu-stat) 21)
   (ash (gpu-stat-vertical-interlace gpu-stat) 22)
   (ash (gpu-stat-display-disabled gpu-stat) 23)
   (ash (gpu-stat-irq1 gpu-stat) 24)
   (ash (gpu-stat-dma gpu-stat) 25)
   (ash (gpu-stat-ready-to-receive-command gpu-stat) 26)
   (ash (gpu-stat-ready-to-send-vram-to-cpu gpu-stat) 27)
   (ash (gpu-stat-ready-to-receive-dma-block gpu-stat) 28)
   (ash (gpu-stat-dma-direction gpu-stat) 29)
   (ash (gpu-stat-even-odd-line gpu-stat) 31)))

; TODO(Samantha): word-to-gpu-stat function.

(defstruct gp0-operation
  (function (lambda (gpu &rest values) (declare (ignore gpu values)) 0)
            ; TODO(Samantha): Try to fix this to have it look a bit less hacky?
            ; Although you could just use an &rest argument to consolidate the
            ; twelve possible arguments of this slot, in practice, sbcl will
            ; cause compile failures whenever this file changes due to functions
            ; with &rest not being subtypes of one another. So, until another
            ; workaround can be found that doesn't involve compiling twice and
            ; making sure :force is nil, use the optional.
            :type (function (gpu &optional
                                 ; These 12 possible arguments represent the max
                                 ; number of arguments a gp0 operation could
                                 ; ever have which is in the case of GP0(#x3E).
                                 (unsigned-byte 32) (unsigned-byte 32)
                                 (unsigned-byte 32) (unsigned-byte 32)
                                 (unsigned-byte 32) (unsigned-byte 32)
                                 (unsigned-byte 32) (unsigned-byte 32)
                                 (unsigned-byte 32) (unsigned-byte 32)
                                 (unsigned-byte 32) (unsigned-byte 32))
                            (unsigned-byte 32)))
  (required-number-of-arguments 0 :type (unsigned-byte 8))
  (current-number-of-arguments 0 :type (unsigned-byte 8))
  (remaining-image-words 0 :type (unsigned-byte 32))
  (arguments nil :type list)
  (arguments-tail nil :type list))

(defvar *viewport* (make-viewport '(1024 512)))
; TODO(Samantha): Move this into a gpu power on?
(cepl:repl 1024 512)

(defstruct gpu
  "A model psx gpu"
  (gpu-stat (make-gpu-stat) :type gpu-stat)
  ; Not sure how to group the following variables. Maybe some sort of
  ; render-settings struct?
  (textured-rectangle-x-flip nil :type boolean)
  (textured-rectangle-y-flip nil :type boolean)
  (drawing-area-left 0 :type (unsigned-byte 32))
  (drawing-area-top 0 :type (unsigned-byte 32))
  (drawing-area-right 0 :type (unsigned-byte 32))
  (drawing-area-bottom 0 :type (unsigned-byte 32))
  (drawing-offset-x 0 :type (signed-byte 11))
  (drawing-offset-y 0 :type (signed-byte 11))
  (texture-window-x-mask 0 :type (unsigned-byte 5))
  (texture-window-y-mask 0 :type (unsigned-byte 5))
  (texture-window-x-offset 0 :type (unsigned-byte 5))
  (texture-window-y-offset 0 :type (unsigned-byte 5))
  (start-of-display-area-in-vram-x 0 :type (unsigned-byte 10))
  (start-of-display-area-in-vram-y 0 :type (unsigned-byte 9))
  (display-start-x 0 :type (unsigned-byte 12))
  (display-end-x 0 :type (unsigned-byte 12))
  (display-start-y 0 :type (unsigned-byte 10))
  (display-end-y 0 :type (unsigned-byte 10))
  (gp0-op (make-gp0-operation) :type gp0-operation)
  (exception-callback
   (lambda () 0)
   :type (function () (unsigned-byte 8))))

; TODO(Samantha): Move these into the gpu.
(defparameter *gpu-list* (list))
(defparameter *gpu-list-len* 0)

(declaim (ftype (function (gpu) (unsigned-byte 32)) read-gpu-read))
(defun read-gpu-read (gpu)
  (declare (ignore gpu))
  (when *debug-gpu*
    (format t "Reading from gpu-read is unimplemented! Returning 0.~%"))
  0)

(declaim (ftype (function (gpu (unsigned-byte 32))
                          (unsigned-byte 32))
                draw-mode-settings))
(defun draw-mode-settings (gpu value)
  (let ((gpu-stat (gpu-gpu-stat gpu)))
    (setf (gpu-stat-texture-page-x-base gpu-stat) (ldb (byte 4 0) value))
    (setf (gpu-stat-texture-page-y-base gpu-stat) (ldb (byte 1 4) value))
    (setf (gpu-stat-semi-transparency gpu-stat) (ldb (byte 2 5) value))
    (setf (gpu-stat-texture-page-colors gpu-stat) (ldb (byte 2 7) value))
    (setf (gpu-stat-dither-24-to-15-bit gpu-stat) (ldb (byte 1 9) value))
    (setf (gpu-stat-draw-to-display-area gpu-stat) (ldb (byte 1 10) value))
    (setf (gpu-stat-texture-disable gpu-stat) (ldb (byte 1 11) value)))
  (setf (gpu-textured-rectangle-x-flip gpu) (ldb-test (byte 1 12) value))
  (setf (gpu-textured-rectangle-y-flip gpu) (ldb-test (byte 1 13) value))
  value)

(declaim (ftype (function (gpu (unsigned-byte 32))
                          (unsigned-byte 32))
                set-texture-window))
(defun set-texture-window (gpu value)
  (setf (gpu-texture-window-x-mask gpu) (ldb (byte 5 0) value))
  (setf (gpu-texture-window-y-mask gpu) (ldb (byte 5 5) value))
  (setf (gpu-texture-window-x-offset gpu) (ldb (byte 5 10) value))
  (setf (gpu-texture-window-y-offset gpu) (ldb (byte 5 15) value)))

(declaim (ftype (function (gpu (unsigned-byte 32))
                          (unsigned-byte 32))
                set-drawing-area-top-left))
(defun set-drawing-area-top-left (gpu value)
  (setf (gpu-drawing-area-left gpu) (ldb (byte 10 0) value))
  (setf (gpu-drawing-area-top gpu) (ldb (byte 10 10) value)))

(declaim (ftype (function (gpu (unsigned-byte 32))
                          (unsigned-byte 32))
                set-drawing-area-bottom-right))
(defun set-drawing-area-bottom-right (gpu value)
  (setf (gpu-drawing-area-right gpu) (ldb (byte 10 0) value))
  (setf (gpu-drawing-area-bottom gpu) (ldb (byte 10 10) value)))

(declaim (ftype (function (gpu (unsigned-byte 32))
                          (unsigned-byte 32))
                set-drawing-offset))
(defun set-drawing-offset (gpu value)
  ; TODO(Samantha): Move this into VBLANK when implemented.
  (when (car *gpu-list*)
    (funcall (gpu-exception-callback gpu))
    (draw gpu))
  (let ((x (ldb (byte 11 0) value)) (y (ldb (byte 11 11) value)))
    ; Offsets are (signed-byte 11), peform the conversions if necessary.
    (when (ldb-test (byte 1 10) x)
      (setf x (* (the (signed-byte 32) -1) (logand #x7FF (1+ (lognot x))))))
    (when (ldb-test (byte 1 10) y)
      (setf y (* (the (signed-byte 32) -1) (logand #x7FF (1+ (lognot y))))))
    (setf (gpu-drawing-offset-x gpu) x)
    (setf (gpu-drawing-offset-y gpu) y))
  value)

(declaim (ftype (function (gpu (unsigned-byte 32))
                          (unsigned-byte 32))
                set-mask-bits))
(defun set-mask-bits (gpu value)
  (setf (gpu-stat-set-mask-bit (gpu-gpu-stat gpu)) (ldb (byte 1 0) value))
  (setf (gpu-stat-draw-pixels (gpu-gpu-stat gpu)) (ldb (byte 1 1) value)))

(declaim (ftype (function (gpu (unsigned-byte 32)
                               (unsigned-byte 32)
                               (unsigned-byte 32)
                               (unsigned-byte 32)
                               (unsigned-byte 32))
                          (unsigned-byte 32))
                render-opaque-monochromatic-quadrilateral))
(defun render-opaque-monochromatic-quadrilateral (gpu color v1 v2 v3 v4)
  (declare (ignore gpu))
  ; TODO(Samantha): Use an index-array instead of copying vertices.
  (setf *gpu-list* (list* (list (word-to-position v3) (word-to-color color))
                          (list (word-to-position v2) (word-to-color color))
                          (list (word-to-position v1) (word-to-color color))
                          (list (word-to-position v2) (word-to-color color))
                          (list (word-to-position v3) (word-to-color color))
                          (list (word-to-position v4) (word-to-color color))
                          *gpu-list*))
  (incf *gpu-list-len* 6)
  0)

(declaim (ftype (function (gpu (unsigned-byte 32))
                          (unsigned-byte 32))
                clear-texture-cache))
(defun clear-texture-cache (gpu value)
  (declare (ignore gpu))
  (when *debug-gpu*
    (format t "GP0(#x01): clear-texture-cache is unimplemented ~
             (because texture cache is not implemented)!~%"))
  value)

(declaim (ftype (function (gpu (unsigned-byte 32))
                          (unsigned-byte 32))
                load-image-word))
(defun load-image-word (gpu value)
  (declare (ignore value))
  (decf (gp0-operation-remaining-image-words (gpu-gp0-op gpu)))
  (if (not (zerop
            (gp0-operation-remaining-image-words
             (gpu-gp0-op gpu))))
    ; Only resetting the current number of arguments and the actual arguments
    ; list allows us to just reuse the existing gp0-operation struct, including
    ; the remaining-image-words
    (progn
     (setf (gp0-operation-current-number-of-arguments (gpu-gp0-op gpu)) 0)
     (setf (gp0-operation-required-number-of-arguments (gpu-gp0-op gpu)) 1))
    ; Setting this to 0 allows for the next word sent
    ; to GP0 to resume normally opcode decoding.
    (setf
     (gp0-operation-required-number-of-arguments
      (gpu-gp0-op gpu))
     0))
  0)

(declaim (ftype (function (gpu (unsigned-byte 32) (unsigned-byte 32) (unsigned-byte 32))
                          (unsigned-byte 32))
                load-image))
(defun load-image (gpu command coordinates size)
  (declare (ignore command coordinates))
  (when *debug-gpu*
    (format t "GP0(#xA0): load-image is not fully implemented!~%"))
  (setf (gp0-operation-remaining-image-words (gpu-gp0-op gpu))
        (* (ldb (byte 16 0) size) (ldb (byte 16 16) size)))
  (unless (zerop (mod (gp0-operation-remaining-image-words (gpu-gp0-op gpu)) 2))
    (incf (gp0-operation-remaining-image-words (gpu-gp0-op gpu))))
  (setf (gp0-operation-remaining-image-words (gpu-gp0-op gpu))
        (/ (gp0-operation-remaining-image-words (gpu-gp0-op gpu)) 2))
  ; TODO(Samantha): This is absolutely hideous and there is no way it's
  ; performant. Consider a better method of loading the image into vram.
  ; Create a fake GP0-operation that will load the pixels into vram one by one.
  ; This bypasses needing to inspect GP0(#xA0) to determine the number of
  ; arguments we would need.
  (setf (gpu-gp0-op gpu)
        (make-gp0-operation
         :function #'load-image-word
         :required-number-of-arguments 1
         :current-number-of-arguments 0
         :remaining-image-words (gp0-operation-remaining-image-words (gpu-gp0-op gpu))
         :arguments (list)))
  0)

(declaim (ftype (function (gpu (unsigned-byte 32) (unsigned-byte 32) (unsigned-byte 32))
                          (unsigned-byte 32))
                save-image))
(defun save-image (gpu command coordinates size)
  (declare (ignore gpu command coordinates size))
  (when *debug-gpu*
    (format t "GP0(#xC0): save-image-from-vram is unimplemented!~%"))
  0)

(declaim (ftype (function (gpu (unsigned-byte 32) (unsigned-byte 32)
                               (unsigned-byte 32) (unsigned-byte 32)
                               (unsigned-byte 32) (unsigned-byte 32)
                               (unsigned-byte 32) (unsigned-byte 32))
                          (unsigned-byte 32))
                render-opaque-shaded-quadrilateral))
(defun render-opaque-shaded-quadrilateral (gpu color1 v1 color2 v2 color3 v3 color4 v4)
  (declare (ignore gpu))
  (setf *gpu-list* (list* (list (word-to-position v3) (word-to-color color3))
                          (list (word-to-position v2) (word-to-color color2))
                          (list (word-to-position v1) (word-to-color color1))
                          (list (word-to-position v2) (word-to-color color2))
                          (list (word-to-position v3) (word-to-color color3))
                          (list (word-to-position v4) (word-to-color color4))
                          *gpu-list*))
  (incf *gpu-list-len* 6)
  0)

(declaim (ftype (function (gpu (unsigned-byte 32) (unsigned-byte 32)
                               (unsigned-byte 32) (unsigned-byte 32)
                               (unsigned-byte 32) (unsigned-byte 32)
                               (unsigned-byte 32) (unsigned-byte 32)
                               (unsigned-byte 32))
                          (unsigned-byte 32))
                render-opaque-texture-blended-quadrilateral))
(defun render-opaque-texture-blended-quadrilateral (gpu color1
                                                        v1 texture-coordinate1-and-palette
                                                        v2 texture-coordinate2-and-texture-page
                                                        v3 texture-coordinate3
                                                        v4 texture-coordinate4)
  (declare (ignore gpu color1 texture-coordinate1-and-palette
                   texture-coordinate2-and-texture-page texture-coordinate3
                   texture-coordinate4))
  (setf *gpu-list* (list* (list (word-to-position v3) (word-to-color #xFF))
                          (list (word-to-position v2) (word-to-color #xFF))
                          (list (word-to-position v1) (word-to-color #xFF))
                          (list (word-to-position v2) (word-to-color #xFF))
                          (list (word-to-position v3) (word-to-color #xFF))
                          (list (word-to-position v4) (word-to-color #xFF))
                          *gpu-list*))
  (incf *gpu-list-len* 6)
  (when *debug-gpu*
    (format t "GP0(#x2C): render-opaque-texture-blended-quadrilateral ~
             is not fully implemented!~%"))
  0)

(declaim (ftype (function (gpu (unsigned-byte 32) (unsigned-byte 32) (unsigned-byte 32))
                          (unsigned-byte 32))
                fill-rectangle))
(defun fill-rectangle (gpu color top-left size)
  (when *debug-gpu*
    (format t "GP0(#x02): fill-rectangle  is not implemented correctly!~%"))
  (let* ((left (logand #x3F0 (ldb (byte 16 0) top-left)))
         (right (+ left (logand #x3F0 (+ #xF (ldb (byte 16 0) size)))))
         (top (logand #x1FF (ldb (byte 16 16) top-left)))
         (bottom (logand #x1FF (logand #xFFFF (+ top (ldb (byte 16 16) size))))))
    (render-opaque-shaded-triangle gpu
                                   color (logior left (ash top 16))
                                   color (logior right (ash top 16))
                                   color (logior right (ash bottom 16)))
    (render-opaque-shaded-triangle gpu
                                   color (logior right (ash bottom 16))
                                   color (logior left (ash top 16))
                                   color (logior left (ash bottom 16))))
  0)

(declaim (ftype (function (gpu (unsigned-byte 32) (unsigned-byte 32)
                               (unsigned-byte 32) (unsigned-byte 32)
                               (unsigned-byte 32) (unsigned-byte 32))
                          (unsigned-byte 32))
                render-opaque-shaded-triangle))
(defun render-opaque-shaded-triangle (gpu color1 v1 color2 v2 color3 v3)
  (declare (ignore gpu))
  ; TODO(Samantha): The psx seems to send the vertices in whichever winding
  ; order it so desires. Until a more elegant fix can be figured out, just
  ; render both faces so that it's visible no matter what. Does this mean the
  ; quads might need 12 vertices..?
  (setf *gpu-list* (list* (list (word-to-position v1) (word-to-color color1))
                          (list (word-to-position v2) (word-to-color color2))
                          (list (word-to-position v3) (word-to-color color3))
                          (list (word-to-position v3) (word-to-color color3))
                          (list (word-to-position v2) (word-to-color color2))
                          (list (word-to-position v1) (word-to-color color1))
                          *gpu-list*))
  (incf *gpu-list-len* 6)
  0)

; TODO(Samantha): These probably should be different types. Offload the type
; conversion to the shaders. Also, we should use the interal g-pc type.
(defstruct-g our-vert
  (position :vec2)
  (color :vec3))

(defun-g vert-stage ((vert our-vert) &uniform (offset :vec2))
  (let ((pos (+ offset (our-vert-position vert))))
    (values
     (v!
      (- (/ (aref pos 0) 512.0) 1)
      (- 1 (/ (aref pos 1) 256.0))
      0
      1f0)
     (our-vert-color vert))))

(defun-g frag-stage ((color :vec3))
  (v! (/ (aref color 0) 255.0)
      (/ (aref color 1) 255.0)
      (/ (aref color 2) 255.0)
      0))

(def-g-> some-pipeline ()
  :vertex (vert-stage our-vert)
  :fragment (frag-stage :vec3))

(defun draw (gpu)
  (with-viewport *viewport*
    (clear)
    (map-g #'some-pipeline (make-buffer-stream
                            (make-gpu-array
                             *gpu-list*
                             :element-type 'our-vert)
                            :length *gpu-list-len*
                            :index-array (make-gpu-array
                                          (loop for i from 0 to (- *gpu-list-len* 1) collect i)
                                          :element-type :uint8)
                            )
           :offset (v! (gpu-drawing-offset-x gpu) (gpu-drawing-offset-y gpu)))
    (swap)
    (setf *gpu-list* (list))
    (setf *gpu-list-len* 0)))

(declaim (ftype (function (gpu (unsigned-byte 32)) (unsigned-byte 32))))
(defun assign-new-gp0-op (gpu value)
  (let ((operation (lambda ()))
        (required-arguments 0))
    (case (ldb (byte 8 24) value)
      (#x00
        (setf required-arguments 1)
        (setf operation (lambda (gpu value) (declare (ignore gpu value)) 0)))
      (#x02
        (setf required-arguments 3)
        (setf operation #'fill-rectangle))
      (#x30
        (setf required-arguments 6)
        (setf operation #'render-opaque-shaded-triangle))
      (#x38
        (setf required-arguments 8)
        (setf operation #'render-opaque-shaded-quadrilateral))
      (#x2C
        (setf required-arguments 9)
        (setf operation #'render-opaque-texture-blended-quadrilateral))
      (#xA0
        (setf required-arguments 3)
        (setf operation #'load-image))
      (#xC0
        (setf required-arguments 3)
        (setf operation #'save-image))
      (#x01
        (setf required-arguments 1)
        (setf operation #'clear-texture-cache))
      (#xE1
        (setf required-arguments 1)
        (setf operation #'draw-mode-settings))
      (#xE2
        (setf required-arguments 1)
        (setf operation #'set-texture-window))
      (#xE3
        (setf required-arguments 1)
        (setf operation #'set-drawing-area-top-left))
      (#xE4
        (setf required-arguments 1)
        (setf operation #'set-drawing-area-bottom-right))
      (#xE5
        (setf required-arguments 1)
        (setf operation #'set-drawing-offset))
      (#xE6
        (setf required-arguments 1)
        (setf operation #'set-mask-bits))
      (#x28
        (setf required-arguments 5)
        (setf operation #'render-opaque-monochromatic-quadrilateral))
      (otherwise
       (error "Unrecognized GP0 opcode 0x~2,'0x. Full word: 0x~8,'0x"
              (ldb (byte 8 24) value)
              value)))
    (setf (gpu-gp0-op gpu)
          (make-gp0-operation
           :function operation
           :required-number-of-arguments required-arguments
           :current-number-of-arguments 0
           :arguments (list)))
    (when *debug-gpu*
      (format t "GP0(#x~2,'0x)~%" (ldb (byte 8 24) value))))
  0)

(declaim (ftype (function ((unsigned-byte 32))
                          (simple-array single-float (3)))
                word-to-color))
(defun word-to-color (word)
  (v!
   (ldb (byte 8 0) word)
   (ldb (byte 8 8) word)
   (ldb (byte 8 16) word)))

(declaim (ftype (function ((unsigned-byte 32))
                          (simple-array single-float (2)))
                word-to-position))
(defun word-to-position (word)
  (let ((x (if (ldb-test (byte 1 10) word)
             (* -1 (logand #x7FF (1+ (lognot (ldb (byte 11 0) word)))))
             (ldb (byte 11 0) word)))
        (y (if (ldb-test (byte 1 26) word)
             (* -1 (logand #x7FF (1+ (lognot (ldb (byte 11 16) word)))))
             (ldb (byte 11 16) word))))
    (v! x y)))

(declaim (ftype (function (gpu (unsigned-byte 32))
                          (unsigned-byte 32))
                write-gp0))
(defun write-gp0 (gpu value)
  (when (zerop (gp0-operation-required-number-of-arguments (gpu-gp0-op gpu)))
    (assign-new-gp0-op gpu value))
  (let ((gp0-op (gpu-gp0-op gpu)))
    (if (zerop (gp0-operation-current-number-of-arguments (gpu-gp0-op gpu)))
      (progn
       (incf (gp0-operation-current-number-of-arguments gp0-op))
       (setf (gp0-operation-arguments gp0-op) (list value))
       (setf (gp0-operation-arguments-tail gp0-op) (gp0-operation-arguments gp0-op)))
      (progn
       (incf (gp0-operation-current-number-of-arguments gp0-op))
       (setf (cdr (gp0-operation-arguments-tail gp0-op))
             (list value))
       (setf (gp0-operation-arguments-tail gp0-op)
             (cdr (gp0-operation-arguments-tail gp0-op)))))
    (when (= (gp0-operation-current-number-of-arguments (gpu-gp0-op gpu))
             (gp0-operation-required-number-of-arguments (gpu-gp0-op gpu)))
      (setf (gp0-operation-required-number-of-arguments gp0-op) 0)
      (apply (gp0-operation-function gp0-op)
        (cons gpu (gp0-operation-arguments gp0-op)))))
  0)

(declaim (ftype (function (gpu) (unsigned-byte 32)) gpu-soft-reset))
(defun gpu-soft-reset (gpu)
  ; GP1(#x01)
  (reset-command-buffer gpu)
  ; GP1(#x02)
  (acknowledge-irq gpu)
  ; GP1(#x03)
  (set-display-disable gpu #x00000001)
  ; GP1(#x04)
  (set-dma-direction gpu #x00000000)
  ; GP1(#x05)
  (set-start-of-display-area-in-vram gpu #x00000000)
  ; GP1(#x06) [#x200, #xC00]
  (set-display-bounds-horizontal gpu (logior #x200 (ash #xC00 12)))
  ; GP1(#x07) [#x10, #x100]
  (set-display-bounds-vertical gpu (logior #x10 (ash #x100 10)))
  ; GP1(#x08)
  (set-display-mode gpu #x00000000)
  ; GP0(#xE1)
  (draw-mode-settings gpu #x00000000)
  ; GP0(#xE2)
  (set-texture-window gpu #x00000000)
  ; GP0(#xE3)
  (set-drawing-area-top-left gpu #x00000000)
  ; GP0(#xE4)
  (set-drawing-area-bottom-right gpu #x00000000)
  ; GP0(#xE5)
  (set-drawing-offset gpu #x00000000)
  ; GP0(#xE6)
  (set-mask-bits gpu #x00000000))

(declaim (ftype (function (gpu) (unsigned-byte 32)) reset-command-buffer))
(defun reset-command-buffer (gpu)
  (setf (gpu-gp0-op gpu)
        (make-gp0-operation :current-number-of-arguments 0
                            :required-number-of-arguments 0))
  (when *debug-gpu*
    (format t "GP1(#x01) Is not yet implemented! ~
             (Because the command buffer isn't implemented.)~%"))
  0)

(declaim (ftype (function (gpu) (unsigned-byte 32)) acknowledge-irq))
(defun acknowledge-irq (gpu)
  (setf (gpu-stat-irq1 (gpu-gpu-stat gpu)) 0))

(declaim (ftype (function (gpu (unsigned-byte 32))
                          (unsigned-byte 32))
                set-display-disable))
(defun set-display-disable (gpu value)
  (setf (gpu-stat-display-disabled (gpu-gpu-stat gpu)) (ldb (byte 1 0) value)))

(declaim (ftype (function (gpu (unsigned-byte 32))
                          (unsigned-byte 32))
                set-dma-direction))
(defun set-dma-direction (gpu value)
  (setf (gpu-stat-dma-direction (gpu-gpu-stat gpu)) (ldb (byte 2 0) value)))

(declaim (ftype (function (gpu (unsigned-byte 32))
                          (unsigned-byte 32))
                set-start-of-display-area-in-vram))
(defun set-start-of-display-area-in-vram (gpu value)
  (setf (gpu-start-of-display-area-in-vram-x gpu) (ldb (byte 10 0) value))
  (setf (gpu-start-of-display-area-in-vram-y gpu) (ldb (byte 9 10) value)))

(declaim (ftype (function (gpu (unsigned-byte 32))
                          (unsigned-byte 32))
                set-display-bounds-horizontal))
(defun set-display-bounds-horizontal (gpu value)
  (setf (gpu-display-start-x gpu) (ldb (byte 12 0) value))
  (setf (gpu-display-end-x gpu) (ldb (byte 12 12) value)))

(declaim (ftype (function (gpu (unsigned-byte 32))
                          (unsigned-byte 32))
                set-display-bounds-vertical))
(defun set-display-bounds-vertical (gpu value)
  (setf (gpu-display-start-y gpu) (ldb (byte 10 0) value))
  (setf (gpu-display-end-y gpu) (ldb (byte 10 10) value)))

(declaim (ftype (function (gpu (unsigned-byte 32))
                          (unsigned-byte 32))
                set-display-mode))
(defun set-display-mode (gpu value)
  (let ((gpu-stat (gpu-gpu-stat gpu)))
    (setf (gpu-stat-horizontal-resolution-1 gpu-stat) (ldb (byte 2 0) value))
    (setf (gpu-stat-vertical-resolution gpu-stat) (ldb (byte 1 2) value))
    (setf (gpu-stat-video-mode gpu-stat) (ldb (byte 1 3) value))
    (setf (gpu-stat-display-area-color-depth gpu-stat) (ldb (byte 1 4) value))
    (setf (gpu-stat-vertical-interlace gpu-stat) (ldb (byte 1 5) value))
    (setf (gpu-stat-horizontal-resolution-2 gpu-stat) (ldb (byte 1 6) value))
    (setf (gpu-stat-reverse-flag gpu-stat) (ldb (byte 1 7) value))))

(declaim (ftype (function (gpu (unsigned-byte 32))
                          (unsigned-byte 32))
                write-gp1))
(defun write-gp1 (gpu value)
  (when *debug-gpu*
    (format t "GP1(#x~2,'0x)~%" (ldb (byte 8 24) value)))
  (case (ldb (byte 8 24) value)
    (#x00 (gpu-soft-reset gpu))
    (#x01 (reset-command-buffer gpu))
    (#x02 (acknowledge-irq gpu))
    (#x03 (set-display-disable gpu value))
    (#x04 (set-dma-direction gpu value))
    (#x05 (set-start-of-display-area-in-vram gpu value))
    (#x06 (set-display-bounds-horizontal gpu value))
    (#x07 (set-display-bounds-vertical gpu value))
    (#x08 (set-display-mode gpu value))
    (otherwise
     (error "Unrecognized GP1 opcode 0x~2,'0x. Full word: 0x~8,'0x"
            (ldb (byte 8 24) value)
            value)))
  0)

(declaim (ftype (function (gpu (unsigned-byte 4) (unsigned-byte 32))
                          (unsigned-byte 32))
                write-gpu))
(defun write-gpu (gpu offset value)
  (case offset
    (0 (write-gp0 gpu value))
    (4 (write-gp1 gpu value))
    (otherwise (error "Invalid GPU write offset 0x~8,'0x~%" offset))))

(declaim (ftype (function (gpu (unsigned-byte 4))
                          (unsigned-byte 32))
                read-gpu))
(defun read-gpu (gpu offset)
  (case offset
    (0 (read-gpu-read gpu))
    (4 (gpu-stat-to-word (gpu-gpu-stat gpu)))
    (otherwise (error "Invalid GPU read offset 0x~8,'0x~%" offset))))

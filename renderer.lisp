(defpackage #:psx-renderer
  (:nicknames #:renderer)
  (:use :cl
        #:cepl
        #:cepl.skitter.sdl2
        #:rtg-math
        :psx-gpu)
  (:export #:draw #:initialize))

(in-package :psx-renderer)

(declaim (optimize (speed 3) (safety 1)))

; TODO(Samantha): These probably should be different types. Offload the type
; conversion to the shaders.
(defstruct-g our-vert
  (position :vec2)
  (uv :vec2)
  (color :vec3))

(defun-g vert-stage ((vert our-vert) &uniform (offset :vec2))
  (let ((pos (+ offset (our-vert-position vert))))
    (values
     (v!
      (- (/ (aref pos 0) 512.0) 1)
      (- 1 (/ (aref pos 1) 256.0))
      0
      1f0)
     (our-vert-color vert) (our-vert-uv vert))))

(defun-g frag-stage ((color :vec3) (uv :vec2)
                     &uniform (vram :sampler-2d))
  ; TODO(Samantha): This is just here to test textures. REMOVE ME.
  (if (= (aref color 0) 255)
    (texture vram uv)
    (v! (/ (aref color 0) 255.0)
        (/ (aref color 1) 255.0)
        (/ (aref color 2) 255.0)
        0)))

(defpipeline-g some-pipeline ()
  :vertex (vert-stage our-vert)
  :fragment (frag-stage :vec3 :vec2))

(defparameter *time* (get-internal-real-time))

(defun initialize ()
  (cepl:repl 1024 512)
  (setf (surface-title (current-surface (cepl-context)))
        "Effective Guacamole"))

(declaim (ftype (function (gpu))
                draw))
(defun draw (gpu)
  (let ((old-time *time*))
    (setf *time* (get-internal-real-time))
    (setf (surface-title (current-surface (cepl-context)))
          (format nil "Effective Guacamole  FPS: ~f" (/ internal-time-units-per-second (- *time* old-time)))))
  (when (car (gpu-render-list gpu))
    ; TODO(Samantha): Handle events from skitter... Not sure if that's going to
    ; work from a separate file or if the cepl instance is somehow
    ; automagically global?
    (step-host)
    (when (mouse-button (mouse) mouse.left)
      (format t "Pressed the mouse!~%"))
    (when (window-closing (window 0))
      (error "Finally.~%"))
    (decay-events)
    (clear)
    (let* ((vao-indices (make-gpu-array
                         (loop for i from 0 to (- (gpu-render-list-length gpu) 1) collect i)
                         :element-type :USHORT))
           (vao (make-gpu-array
                 (gpu-render-list gpu)
                 :element-type 'our-vert))
           (vram-tex (make-texture (gpu-vram gpu) :element-type :ushort))
           (vram-sampler (sample vram-tex))
           (buffer-stream (make-buffer-stream
                           vao
                           :length (gpu-render-list-length gpu)
                           :index-array vao-indices)))
      (map-g #'some-pipeline buffer-stream
             :offset (v! (gpu-drawing-offset-x gpu)
                         (gpu-drawing-offset-y gpu))
             :vram vram-sampler)
      (free-buffer-stream buffer-stream)
      (free-gpu-array vao)
      (free-texture vram-tex)
      (free-sampler vram-sampler)
      (free-gpu-array vao-indices))
    (swap))
  (values))

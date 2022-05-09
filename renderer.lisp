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
  (if (= (aref uv 0) (aref uv 1) 0)
    (v! (/ (aref color 0) 255.0)
        (/ (aref color 1) 255.0)
        (/ (aref color 2) 255.0)
        0)
    (texture vram (v! (/ (aref uv 0) 2048.0) (/ (aref uv 1) 512.0)))))

(defpipeline-g some-pipeline ()
  :vertex (vert-stage our-vert)
  :fragment (frag-stage :vec3 :vec2))

(defparameter *time* (get-internal-real-time))
(declaim ((integer 0 *) *time*))

(defun initialize ()
  ; Don't redo this if we already have a cepl window.
  (when (= (length +windows+) 0)
    (cepl:repl 1024 512)
    (setf (surface-title (current-surface (cepl-context)))
          "Effective Guacamole"))
  (setf *fbo*
    (make-fbo '(0 :dimensions (512 1024) :element-type :rgba8) '(:d :dimensions (512 1024)))))

(defparameter *tex* (make-texture nil :dimensions '(512 1024) :element-type :ushort))
(defparameter *fbo* nil)

(defun-g passthrough-vert ((vert g-pt))
  (values (v! (pos vert) 1f0) (tex vert)))

(defun-g passthrough-frag ((uv :vec2)
                           &uniform (sam :sampler-2d))
  (texture sam uv))

(defpipeline-g passthrough-pipeline ()
  :vertex (passthrough-vert g-pt)
  :fragment (passthrough-frag :vec2))

(declaim (ftype (function (gpu))
                draw))
(defun draw (gpu)
  (let ((old-time *time*))
    (setf *time* (get-internal-real-time))
    (setf (surface-title (current-surface (cepl-context)))
          (format nil "Effective Guacamole  FPS: ~f" (/ internal-time-units-per-second (- *time* old-time)))))

  ; TODO(Samantha): Handle events from skitter... Not sure if that's going to
  ; work from a separate file or if the cepl instance is somehow
  ; automagically global?
  (step-host)
  (when (window-manager-quitting +window-manager+)
    (cepl:quit)
    (cl-user::quit))
  (decay-events)

  (when (car (gpu-render-list gpu))
    (free *tex*)
    (setf *tex* (make-texture (gpu-vram gpu) :element-type :ushort))
    (let* ((vao-indices (make-gpu-array
                         (loop for i from 0 to (- (gpu-render-list-length gpu) 1) collect i)
                         :element-type :UNSIGNED-INT))
           (vao (make-gpu-array
                 (gpu-render-list gpu)
                 :element-type 'our-vert))
           (vram-sampler (sample *tex*))
           (buffer-stream (make-buffer-stream
                           vao
                           :length (gpu-render-list-length gpu)
                           :index-array vao-indices)))
      (clear)
      (map-g #'some-pipeline buffer-stream
             :offset (v! (gpu-drawing-offset-x gpu)
                         (gpu-drawing-offset-y gpu))
             :vram vram-sampler)
     (map-g #'passthrough-pipeline
            (make-buffer-stream
             (make-gpu-array (list (list (v! -1 1 0) (v! 0 0))
                                   (list (v! -1 -1 0) (v! 0 1))
                                   (list (v! 1 -1 0) (v! 1 1))
                                   (list (v! -1 1 0) (v! 0 0))
                                   (list (v! 1 -1 0) (v! 1 1))
                                   (list (v! 1 1 0) (v! 1 0)))
                             :element-type 'g-pt))
            :sam vram-sampler)
     (free-buffer-stream buffer-stream)
     (free-gpu-array vao)
     (free-sampler vram-sampler)
     (free-gpu-array vao-indices)
     (swap)))
  (values))

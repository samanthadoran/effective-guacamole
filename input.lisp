(defpackage #:psx-input
  (:nicknames #:input)
  (:use :cl
        #:cepl.skitter.sdl2)
  (:export #:init-pads #:controller-callback))

(in-package :psx-input)

(declaim (optimize (speed 3) (safety 1)))

(defparameter *sdl2-pads* nil)

(defun init-pads ()
  (unless *sdl2-pads*
    (sdl2-game-controller-db:load-db)
    (setf *sdl2-pads*
          (make-array 10 :initial-element nil))
    (loop for i from 0 to (1- (sdl2:joystick-count)) do
      (unless (aref *sdl2-pads* i)
        (when (sdl2:game-controller-p i)
          (setf (aref *sdl2-pads* i)
                (sdl2:game-controller-open i)))))
    (skitter.sdl2:enable-background-joystick-events)))

; From http://problemkaputt.de/psx-spx.htm#controllersandmemorycards
; __Halfword 0 (Controller Info)_______________________________________________
; 0-15  Controller Info  (5A41h=digital, 5A73h=analog/pad, 5A53h=analog/stick)
; __Halfword 1 (Digital Switches)______________________________________________
; 0   Select Button    (0=Pressed, 1=Released)
; 1   L3/Joy-button    (0=Pressed, 1=Released/None/Disabled) ;analog mode only
; 2   R3/Joy-button    (0=Pressed, 1=Released/None/Disabled) ;analog mode only
; 3   Start Button     (0=Pressed, 1=Released)
; 4   Joypad Up        (0=Pressed, 1=Released)
; 5   Joypad Right     (0=Pressed, 1=Released)
; 6   Joypad Down      (0=Pressed, 1=Released)
; 7   Joypad Left      (0=Pressed, 1=Released)
; 8   L2 Button        (0=Pressed, 1=Released) (Lower-left shoulder)
; 9   R2 Button        (0=Pressed, 1=Released) (Lower-right shoulder)
; 10  L1 Button        (0=Pressed, 1=Released) (Upper-left shoulder)
; 11  R1 Button        (0=Pressed, 1=Released) (Upper-right shoulder)
; 12  /\ Button        (0=Pressed, 1=Released) (Triangle, upper button)
; 13  () Button        (0=Pressed, 1=Released) (Circle, right button)
; 14  >< Button        (0=Pressed, 1=Released) (Cross, lower button)
; 15  [] Button        (0=Pressed, 1=Released) (Square, left button)
; __Halfword 2 (Right joystick) (analog pad/stick in analog mode only)_________
; 0-7   adc0 RightJoyX (00h=Left, 80h=Center, FFh=Right)
; 8-15  adc1 RightJoyY (00h=Up,   80h=Center, FFh=Down)
; __Halfword 3 (Left joystick) (analog pad/stick in analog mode only)__________
; 0-7   adc2 LeftJoyX  (00h=Left, 80h=Center, FFh=Right)
; 8-15  adc3 LeftJoyY  (00h=Up,   80h=Center, FFh=Down)

(declaim (ftype (function ((unsigned-byte 8)) (unsigned-byte 16))
                controller-callback))
(defun controller-callback (index)
  (logior
   (ash (if (gamepad-button (gamepad index) 4) 0 1) 0)
   (ash 1 1)
   (ash 1 2)
   (ash (if (gamepad-button (gamepad index) 6) 0 1) 3)
   (ash (if (gamepad-button (gamepad index) 11) 0 1) 4)
   (ash (if (gamepad-button (gamepad index) 14) 0 1) 5)
   (ash (if (gamepad-button (gamepad index) 12) 0 1) 6)
   (ash (if (gamepad-button (gamepad index) 13) 0 1) 7)
   (ash 1 8)
   (ash 1 9)
   (ash (if (gamepad-button (gamepad index) 9) 0 1) 10)
   (ash (if (gamepad-button (gamepad index) 10) 0 1) 11)
   (ash (if (gamepad-button (gamepad index) 3) 0 1) 12)
   (ash (if (gamepad-button (gamepad index) 1) 0 1) 13)
   (ash (if (gamepad-button (gamepad index) 0) 0 1) 14)
   (ash (if (gamepad-button (gamepad index) 2) 0 1) 15)))

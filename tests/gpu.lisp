(defpackage #:psx-gpu-tests
  (:nicknames #:gpu-tests)
  (:use :cl
        :rove
        :psx-gpu))

(in-package :psx-gpu-tests)

(deftest scanline-update-test
  (let* ((gpu (make-gpu))
         (video-mode (psx-gpu::gpu-stat-video-mode
                      (psx-gpu::gpu-gpu-stat gpu))))
    (power-on gpu)

    (testing "One ahead."
      (psx-gpu::update-scanline gpu 1)
      (ok (= (psx-gpu::gpu-current-scanline-cycles gpu) 1))
      (ok (zerop (psx-gpu::gpu-current-scanline gpu))))

    (setf (psx-gpu::gpu-current-scanline-cycles gpu)
          0)
    (setf (psx-gpu::gpu-current-scanline gpu)
          0)

    (testing "Ahead one scanline"
      (psx-gpu::update-scanline gpu (psx-gpu::clocks-per-scanline video-mode))
      (ok (= (psx-gpu::gpu-current-scanline-cycles gpu) 0))
      (ok (= (psx-gpu::gpu-current-scanline gpu) 1)))

    (setf (psx-gpu::gpu-current-scanline-cycles gpu)
          0)
    (setf (psx-gpu::gpu-current-scanline gpu)
          0)

    (testing "Ahead one frame"
      (psx-gpu::update-scanline gpu (* (psx-gpu::clocks-per-scanline video-mode)
                                       (psx-gpu::lines-per-frame video-mode)))
      (ok (= (psx-gpu::gpu-current-scanline-cycles gpu) 0))
      (ok (zerop (psx-gpu::gpu-current-scanline gpu))))

    (setf (psx-gpu::gpu-current-scanline-cycles gpu)
          0)
    (setf (psx-gpu::gpu-current-scanline gpu)
          0)))

(deftest time-until-nex-vblank-test
  (let* ((gpu (make-gpu))
         (video-mode (psx-gpu::gpu-stat-video-mode
                      (psx-gpu::gpu-gpu-stat gpu))))
    (power-on gpu)
    (testing "Next line."
      (setf (psx-gpu::gpu-current-scanline gpu)
            256)
      (ok (= (psx-gpu::gpu-cycles-until-next-vsync gpu)
             (psx-gpu::clocks-per-scanline video-mode))))
    (testing "Last line in frame."
      (setf (psx-gpu::gpu-current-scanline gpu)
            (1- (psx-gpu::lines-per-frame video-mode)))
      (ok (= (psx-gpu::gpu-cycles-until-next-vsync gpu)
             (* (1+ 257)
                (psx-gpu::clocks-per-scanline video-mode)))))
    (testing "Next cycle"
      (setf (psx-gpu::gpu-current-scanline gpu)
            256)
      (setf (psx-gpu::gpu-current-scanline-cycles gpu)
            (1- (psx-gpu::clocks-per-scanline video-mode)))
      (ok (= (psx-gpu::gpu-cycles-until-next-vsync gpu) 1)))))

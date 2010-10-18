;;;;; simple step sequencer
;;;;; 
;;;;; 

(in-package #:sdl-gfx-examples)

(defparameter *padding* 30)
(defparameter *width* 600)
(defparameter *height* 300)
(defparameter *sample* nil)
(defparameter *mixer-opened* nil)
(defparameter *sample* 0)
(defparameter *samples* '())
(defparameter *tick* 0)
(defparameter *lispbuilder-path* "/home/hb/src/lispbuilder/")
(dolist (sample (directory
                 (make-pathname :name :wild
                                :type "wav"
                                :directory '(:relative "samples"))))
  (push sample *samples*))

(defun load-libs ()
  (require 'asdf)
  (dolist (component (list "sdl" "sdl-mixer" "sdl-gfx"
                           "sdl-gfx-examples" "sdl-image" "sdl-ttf"))
    (pushnew (format nil "~Alispbuilder-~A/"
                     *lispbuilder-path* component)
             asdf:*central-registry* :test #'equal)
    (asdf:oos 'asdf:load-op (format nil "lispbuilder-~A" component))))

(defclass seq-row ()
  ((w :accessor w :initform 16 :initarg :w)
   (pos :accessor pos :initform 0 :initarg :pos)
   (color :accessor color :initform sdl:*yellow* :initarg :color)
   (code :accessor code :initform (make-sequence 'list 16) :initarg :code)
   (sample :accessor sample :initarg :sample)
   (sample-instance :accessor sample-instance :initarg :sample-instance)
   ))

(defmethod load-sample ((row seq-row))
  (setf (sample-instance row) (sdl-mixer:load-sample (sample row))))
  

(defmethod display ((row seq-row) &key (surface sdl:*default-display*))
  (let ((pos-y (* (pos row) *padding*))
        (i 0)
        )
    (dolist (note (code row))
      (if note
          (if (equal *tick* i)
              (sdl-gfx:draw-filled-circle-*
               (+ (* i *padding*) 20)
               pos-y
               (round (* *padding* 0.4))
               :surface surface
               :color sdl:*red*
               )
              (sdl-gfx:draw-filled-circle-*
               (+ (* i *padding*) 20)
               pos-y
               (round (* *padding* 0.4))
               :surface surface
               :color (color row)
               ))
          (if (equal *tick* i)
              (sdl-gfx:draw-circle-*
               (+ (* i *padding*) 20)
               pos-y
               (round (* *padding* 0.4))
               :surface surface
               :color sdl:*red*
               )
              (sdl-gfx:draw-circle-*
               (+ (* i *padding*) 20)
               pos-y
               (round (* *padding* 0.4))
               :surface surface
               :color (color row)
               )))
          (incf i))))

(defmethod play ((row seq-row))
  (if (nth *tick* (code row))
      (sdl-mixer:play-sample (sample-instance row))))

(defun clean-up ()
  (when *samples*
    (when (sdl-mixer:sample-playing-p nil)
      (sdl-mixer:pause-sample t)
       (sdl-mixer:Halt-sample :channel t)))
    ;;   (dolist (sample *samples*)
    ;;     (sdl:Free sample))
    ;;   (setf *samples* nil))
  (when *mixer-opened*
    (sdl-mixer:Close-Audio t)
    (setf *mixer-opened* nil))
  (sdl-mixer:quit-mixer))

(defun handle-key(key)
  "handle key presses"
  (cond
    ((sdl:key= key :SDL-KEY-ESCAPE)
     (sdl:push-quit-event))))
;; ((sdl:key= key :sdl-key-a)
;;  (sdl-mixer:play-sample
;;   (nth 
;;    (setf *sample* (round (* (/ (xpos *r1*) *width*)
;;                             (length *samples*))))
;;    *samples*)))))

(defun handle-mouse (x y row)
  (let ((index (floor (/ x *padding*))))
    (if (< index (w row))
        (if (nth index (code row))
            (setf (nth index (code row)) nil)
            (setf (nth index (code row)) t)))
        (print (code row))
    ))

(defun sample-finished-action ()
  (sdl-mixer:register-sample-finished
   (lambda (channel)
     (declare (ignore channel))
     nil)))

(defun cltx ()
  (let ((row1 (make-instance 'seq-row
                              :w 16
                              :pos 1
                              :sample (first *samples*)))
        (status "")
        (timeout 1)      
        (100-frames-p (every-n-frames 100)))
    (sdl:with-init ()
      (sdl:window *width* *height*
                  :title-caption "cltx")
      (setf (sdl:frame-rate) 60)
      (sdl-gfx:initialise-default-font)
      
      (sdl:clear-display (sdl:color))
      (setf status "Opening Audio Mixer.....")
      (draw-fps status 10 150 sdl:*default-font* sdl:*default-display* t)
      (sdl:enable-key-repeat 500 50)
      (sdl-mixer:init-mixer :mp3)
      (setf *mixer-opened*
            (sdl-mixer:OPEN-AUDIO :chunksize 1024 :enable-callbacks nil))
      (when *mixer-opened*
        (setf status "Opened Audio Mixer!")
        (sample-finished-action)
        (load-sample row1)
        (sdl-mixer:allocate-channels 16))
      (print status)
        
      (sdl:with-events ()
        (:quit-event ()
                     (clean-up)
                     t)
        (:video-expose-event () (sdl:update-display))
        (:mouse-button-down-event (:x x :y y)
                                  (handle-mouse x y row1))
        (:key-down-event (:key key)
                         (when *mixer-opened*
                           (handle-key key)))
        
        (:idle ()
               (sdl:clear-display (sdl:color))
               (incf timeout (sdl:dt))
               (if (> timeout 1)
                   (progn (setf *tick* (mod (+ *tick* 1) 16))
                          (play row1)
                          (setf timeout (mod timeout 1))))
               (display row1)

               (draw-fps status
                         10 150 sdl:*default-font* sdl:*default-display*
                         (funcall 100-frames-p))
               
               (sdl:update-display))))))

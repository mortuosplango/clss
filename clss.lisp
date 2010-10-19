;;;;; simple step sequencer
;;;;; 
;;;;; 

(in-package :sdl-gfx-examples)

(defparameter *padding* 30)
(defparameter *sample* nil)
(defparameter *top-padding* 20)
(defparameter *width* 800)
(defparameter *height* 300)
(defparameter *mixer-opened* nil)
(defparameter *sample* 0)
(defparameter *samples* '())
(defparameter *tick* 0)
(defparameter *length* 0.3)
(defparameter *lispbuilder-path* "/home/hb/src/lispbuilder/")
(defparameter *sample-path* "/home/hb/src/clss/samples/")
(defparameter *rows* '())

(dolist (sample (directory
                 (make-pathname :name :wild
                                :type "wav"
                                :directory (pathname-directory
                                            (pathname *sample-path*)))))
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

(defgeneric load-sample (seq-row))
(defmethod load-sample ((row seq-row))
  (setf (sample-instance row) (sdl-mixer:load-sample (nth (sample row) *samples*))))

(defmethod display ((row seq-row) &key (surface sdl:*default-display*))
  (let ((pos-y (+ (* (pos row) *padding*) *top-padding*))
        (i 0))
    (dolist (note (code row))
      (let ((pos-x (+ (* i *padding*) 20))
            (radius (round (* *padding* 0.4))))
        (if note
            (if (equal *tick* i)
                (sdl-gfx:draw-filled-circle-* pos-x pos-y radius
                                              :surface surface :color sdl:*red*)
                (sdl-gfx:draw-filled-circle-* pos-x pos-y radius
                                              :surface surface :color (color row)))
            (if (equal *tick* i)
                (sdl-gfx:draw-circle-* pos-x pos-y radius
                                       :surface surface :color sdl:*red*)
                (sdl-gfx:draw-circle-* pos-x pos-y radius
                                       :surface surface :color (color row))))
        (incf i)))
    (sdl:draw-string-solid-* (pathname-name (nth (sample row) *samples*))
                             (+ (* i *padding*) 20)
                             pos-y
                             :color (color row)
                             :surface surface
                             )
    ))

(defgeneric play (seq-row))
(defmethod play ((row seq-row))
  (if (nth *tick* (code row))
      (sdl-mixer:play-sample (sample-instance row))))

(defgeneric change-sample (seq-row direction))
(defmethod change-sample ((row seq-row) direction)
  (setf (sample row) (mod (+ (sample row) direction) (length *samples*)))
  (sdl:Free (sample-instance row))
  (load-sample row))

(defun clean-up ()
  (when *samples*
    (when (sdl-mixer:sample-playing-p nil)
      (sdl-mixer:pause-sample t)
      (sdl-mixer:Halt-sample :channel t)))
  (dolist (row *rows*)
    (sdl:Free (sample-instance row)))
  (when *mixer-opened*
    (sdl-mixer:Close-Audio t)
    (setf *mixer-opened* nil))
  (sdl-mixer:quit-mixer))

(defun handle-key(key)
  "handle key presses"
  (cond
    ((sdl:key= key :SDL-KEY-ESCAPE)
     (sdl:push-quit-event))))


(defun handle-mouse (x y)
  (let ((index (floor (/ x *padding*)))
        (rownum (floor (/ y *padding*)))
        )
    (if (< rownum (length *rows*))
        (let ((row (nth rownum *rows*)))
          (if (< index (w row))
              (progn
                (if (nth index (code row))
                    (setf (nth index (code row)) nil)
                    (setf (nth index (code row)) t))
                (print (code row)))
              (change-sample row 1))))))

(defun sample-finished-action ()
  (sdl-mixer:register-sample-finished
   (lambda (channel)
     (declare (ignore channel))
     nil)))

(defun clss ()
  (let (
        (status "")
        (timeout 1)      
        (100-frames-p (every-n-frames 100)))
    (dotimes (i 8)
      (push (make-instance 'seq-row
                           :w 16
                           :pos i
                           :color (nth (mod i 2) (list sdl:*blue* sdl:*cyan*))
                           :sample i)
                           *rows*))
    (setf *rows* (nreverse *rows*))
    (sdl:with-init ()
      (sdl:window *width* *height*
                  :title-caption "clss")
      (setf (sdl:frame-rate) 60)
      (sdl-gfx:initialise-default-font)
      
      (sdl:clear-display (sdl:color))
      (setf status "Opening Audio Mixer.....")
      (draw-fps status 10 150 sdl:*default-font* sdl:*default-display* t)
      (sdl:enable-key-repeat 500 50)
      (sdl-mixer:init-mixer :mp3)
      (setf *mixer-opened*
            (sdl-mixer:OPEN-AUDIO :frequency 48000
                                  :chunksize 1024
                                  :enable-callbacks nil))
      (when *mixer-opened*
        (setf status "Opened Audio Mixer!")
        (sample-finished-action)
        (dolist (row *rows*)
          (load-sample row))
        (sdl-mixer:allocate-channels 16))
      (print status)
      
      (sdl:with-events ()
        (:quit-event ()
                     (clean-up)
                     t)
        (:video-expose-event () (sdl:update-display))
        (:mouse-button-down-event (:x x :y y)
                                  (handle-mouse x y))
        (:key-down-event (:key key)
                         (when *mixer-opened*
                           (handle-key key)))
        
        (:idle ()
               (sdl:clear-display (sdl:color))
               (incf timeout (sdl:dt))
               (if (> timeout *length*)
                   (progn (setf *tick* (mod (+ *tick* 1) 16))
                          (when *mixer-opened*
                            (dolist (row *rows*)
                              (play row)))
                          (setf timeout (mod timeout *length*))))
               (dolist (row *rows*)
                 (display row))

               (draw-fps status
                         10 (- *height* 20) sdl:*default-font* sdl:*default-display*
                         (funcall 100-frames-p))
               
               (sdl:update-display))))))

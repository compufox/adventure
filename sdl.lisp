(in-package :adventure)

(defvar *ticks-per-frame* (/ 1000 60))

(defmacro scancode-case (symbol &rest forms)
  "allows scancodes to be checked in a format similar to CASE

SYMBOL is the SDL event key symbol
FORMS are of the form (:scancode-KEY SEXP).
  SEXP gets called when the scancode value of SYMBOL matches the :scancode-KEY"
  (let ((sym-val (gensym)))
    `(let ((,sym-val (scancode-value ,symbol)))
       (cond
	 ,@(loop for f in forms
		 collect
		 `((scancode= ,sym-val ,(car f))
		   ,(cadr f)))))))

(defmacro with-sdl ((name width height) &body body)
  "sets up SDL window with NAME, WIDTH, and HEIGHT"
  `(with-init (:video)
     (sdl2-ttf:init)
     (with-window (window :title ,name :w ,width :h ,height
			  :flags '(:shown))
       (with-renderer (renderer window :flags '(:presentvsync :accelerated))
	 (start-text-input)
	 (let ((current-line "> ")
	       (window-surface (get-window-surface window))
	       (font (sdl2-ttf:open-font (merge-pathnames "fonts/Minimal5x7.ttf"
							  (asdf:system-source-directory :adventure))
					 25))
	       (updated t))
	   (with-event-loop (:method :poll)
	     (:textinput (:text text)
			 (setf updated t)
			 (setf current-line
			       (concatenate 'string current-line
					    (string (code-char text)))))
	     (:keyup (:keysym sym)
		     (scancode-case sym
		       (:scancode-escape (push-quit-event))
		       ;; this is where we need to start firing off events like "on-enter"
		       (:scancode-backspace
			(setf current-line (subseq current-line 0
						   (1- (length current-line)))))
		       (:scancode-return
			(progn
			  (princ current-line)
			  (princ #\newline)
			  (setf current-line "> ")
			  (force-output)))))
	     (:idle ()
		    ;; this is where we would need to redraw our backlog buffer
		    ;; some kind of loop
		    (set-render-draw-color renderer 0 0 0 255)
		    (render-clear renderer)
		    (unless (str:emptyp current-line)
		      (let* ((surf (sdl2-ttf:render-utf8-solid font current-line 255 255 255 255))
			     (text (create-texture-from-surface renderer surf))
			     (rect (make-rect 0
					      ;; ensures that we print the text
					      ;; right at the bottom of the window
					      (- (surface-height window-surface)
						 (texture-height text))
					      (texture-width text)
					      (texture-height text))))
			(render-copy renderer text
				     :source-rect (cffi:null-pointer)
				     :dest-rect rect)
			(destroy-texture text)))
		    (render-present renderer))
	     (:quit ()
		    (sdl2-ttf:quit)
		    t)))))))

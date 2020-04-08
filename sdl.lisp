(in-package :adventure)

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
       (with-renderer (renderer window)
	 (set-render-draw-color renderer 0 0 0 1)
	 (start-text-input)
	 (let ((current-line ""))
	   (with-event-loop (:method :poll)
	     (:textinput (:text text)
			 (setf current-line
			       (concatenate 'string current-line
					    (string (code-char text)))))
	     (:keyup (:keysym sym)
		     (scancode-case sym
		       (:scancode-escape (push-quit-event))
		       ;; this is where we need to start firing off events like "on-enter"
		       (:scancode-return
			(progn
			  (princ current-line)
			  (princ #\newline)
			  (setf current-line "")
			  (force-output)))))
	     (:idle ()
		    ;; this is where we would need to redraw our backlog buffer
		    ;; some kind of loop
		    (render-clear renderer)
		    (render-present renderer))
	     (:quit () t))))
       (sdl2-ttf:quit))))

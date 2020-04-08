;;;; adventure.lisp

(in-package #:adventure)

(defvar *hooks* (make-hash-table))
(defvar *commands* (make-hash-table :test 'equal))
(defvar *rooms* nil)
(defvar *buffer* nil)
(defvar *items* nil)

(defstruct item
  name
  description)

(defstruct room
  short-description
  long-description
  items
  x
  y
  exits)

(defstruct exit
  locked)

(defstruct player
  (x 0 :type integer)
  (y 0 :type integer)
  (inventory nil :type list))

(defstruct hook
  function
  (priority 0 :type integer))

(defun add-command (cmd function)
  (setf (gethash cmd *commands*) function))

(defun add-hook (type function &key (priority 0))
  (let ((hook (make-hook :function function :priority priority))
	(lst (gethash type *hooks*)))
    (setf (gethash type *hooks*) (sort (append lst (list hook)) #'<
				       :key #'hook-priority))))

(defun input-line (line)
  (push line *buffer*))

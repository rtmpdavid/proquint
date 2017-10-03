;;;; proquint.lisp

(in-package #:proquint)

;;; "proquints" goes here. Hacks and glory await!

(defgeneric make-proquint (datum))

(defun get-con (value)
  (declare (type (unsigned-byte 4) value))
  (char "bdfghjklmnprstvz" value))

(defun get-vo (value)
  (declare (type (unsigned-byte 2) value))
  (char "aiou" value))

(defun get-proquint (quint)
  (+ (ash (from-con (char quint 0)) 12)
     (ash (from-vo (char quint 1)) 10)
     (ash (from-con (char quint 2)) 6)
     (ash (from-vo (char quint 3)) 4)
     (ash (from-con (char quint 4)) 0)))

(defun from-con (con)
  (case con
    (#\b 0)
    (#\d 1)
    (#\f 2)
    (#\g 3)
    (#\h 4)
    (#\j 5)
    (#\k 6)
    (#\l 7)
    (#\m 8)
    (#\n 9)
    (#\p #xA)
    (#\r #xB)
    (#\s #xC)
    (#\t #xD)
    (#\v #xE)
    (#\z #xF)))

(defun from-vo (con)
  (case con
    (#\a 0)
    (#\i 1)
    (#\o 2)
    (#\u 3)))

(defun get-proquint (quint)
  (+ (ash (from-con (char quint 0)) 12)
     (ash (from-vo (char quint 1)) 10)
     (ash (from-con (char quint 2)) 6)
     (ash (from-vo (char quint 3)) 4)
     (ash (from-con (char quint 4)) 0)))

(defmacro set-proquint (string value proquint-index)
  `(let ((offset (* ,proquint-index 6)))
     (when (> offset 0)
       (setf (char ,string (- offset 1)) #\-))
     (setf (char ,string (+ offset 0)) (get-con (ldb (byte 4 12) ,value))
	   (char ,string (+ offset 1)) (get-vo (ldb (byte 2 8) ,value))
	   (char ,string (+ offset 2)) (get-con (ldb (byte 4 6) ,value))
	   (char ,string (+ offset 3)) (get-vo (ldb (byte 2 4) ,value))
	   (char ,string (+ offset 4)) (get-con (ldb (byte 4 0) ,value)))))

(defun from-integer (integer)
  (declare (type (integer 0) integer))
  (let* ((size (ceiling (/ (log (max 2 integer) 2) 16)))
	 (result (make-string (1- (* 6 size)))))
    (loop for i from 0 to (1- size)
	  do (set-proquint result (ldb (byte 16 (* 16 i)) integer) (- size i 1)))
    result))

(defun from-octets (array)
  (declare (type (array (unsigned-byte 8) 1) array))
  (let* ((size (ceiling (/ (length array) 2)))
  	 (result (make-string (1- (* 6 size)))))
    (loop for i from 0 to (1- (length array)) by 2
	  for value = (+ (ash (aref array i) 8) (aref array (1+ i))) 
	  do (set-proquint result value (/ i 2)))
    result))

(defun to-integer (proquint)
  (let ((size (ceiling (length proquint) 6)))
    (loop for i from 0 to (length proquint) by 6
	  for s from (* (1- size) 16) downto 0 by 16
	  summing (ash (get-proquint (subseq proquint i (+ i 5))) s))))

(defun to-octets (proquint)
  (let* ((size (ceiling (length proquint) 6))
	 (octets (make-array (* size 2) :element-type '(unsigned-byte 8))))
    (loop for i from 0 to (length proquint) by 6
	  for j from 0 by 2
	  for value = (get-proquint (subseq proquint i (+ i 5)))
	  do (setf (aref octets j) (ldb (byte 8 8) value)
	  	   (aref octets (1+ j)) (ldb (byte 8 0) value))
	  )
    octets))

(defvar *psym-counter* 0)
(defun genpsym ()
  (make-symbol (string-upcase (from-integer (1- (incf *psym-counter*))))))

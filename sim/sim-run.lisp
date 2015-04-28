(load "sim.lisp")
(use-package :retrokitchen-sim)

(unless (= (length *posix-argv*) 2)
  (format *error-output* "usage: sim-run.lisp <input>~%" (first *posix-argv*))
  (sb-ext:exit))

(defun read-word (stream)
  (let ((word 0))
    (dotimes (i 9)
      (let ((byte (read-byte stream nil nil)))
        (if byte
            (+ word (ash byte (* i 8)))
            (return nil))))
    word))

(destructuring-bind (_ input) *posix-argv*
  (with-open-file (stream input
                          :direction :input
                          :element-type 'unsigned-byte
                          :if-does-not-exist nil)
    (unless stream
      (format *error-output* "error: file cannot open~%")
      (sb-ext:exit))

    (let ((code (loop for word = (read-word stream)
                     while word
                     (collect word))))
      (simulate code *standard-input* *standard-output*))))

(defpackage retrokitchen-asm
  (:use :common-lisp)
  (:export :assemble
           :microcode))

(in-package :retrokitchen-asm)

(defvar *pc*)
(defvar *label-table*)
(defvar *stream*)
(defvar *preprocess*)

;;; instruction
(defvar *instructions* (make-hash-table))

(defmacro define-instruction (name args &body body)
  `(setf (gethash ,name *instructions*)
         (lambda ,args ,@body)))

(defun label (key)
  (when *preprocess* (push (cons key *pc*) *label-table*)))


;;; assembler
(defun run1 (code)
  (dolist (inst code)
    (if (symbolp inst)
        (label inst)
        (destructuring-bind (op &rest args) inst
          (multiple-value-bind (fn valid) (gethash op *instructions*)
            (unless valid
              (error "error: invalid instruction"))
            (apply fn args))))))

(defun microcode (code)
  (run1 code))

(defun assemble (os offset code)
  (let ((*label-table* nil)
        (*stream* os))
    (let ((*pc* offset) (*preprocess* t))
      (run1 code))
    (let ((*pc* offset) (*preprocess* nil) (*stream* os))
      (run1 code))))


(defun write-word (word stream)
  (when stream
    (write-byte (logand 255 word) stream)
    (write-byte (logand 255 (ash word -8)) stream)
    (write-byte (logand 255 (ash word -16)) stream)
    (write-byte (logand 255 (ash word -24)) stream)
    (write-byte (logand 255 (ash word -32)) stream)))

(defun check-width% (num width)
  (<= 0 num (1- (ash 1 width))))

(defmacro check-args (&rest rest)
  `(progn ,@(mapcar (lambda (x)
                      (destructuring-bind (name width) x
                        `(assert (and (integerp ,name) (check-width% ,name ,width)))))
                    rest)))

;;; formatter
(defun fmt-mem (tag opcd ra rb disp)
  (check-args (tag 4) (opcd 6) (ra 5) (rb 5) (disp 16))
  (let ((word (logior (ash tag 32)
                      (ash opcd 25)
                      (ash ra 20)
                      (ash rb 15)
                      disp)))
    (write-word word *stream*)
    word))

(defun fmt-bra (tag opcd ra disp)
  (check-args (tag 4) (opcd 6) (ra 5) (disp 21))
  (let ((word (logior (ash tag 32)
                      (ash opcd 25)
                      (ash ra 20)
                      disp)))
    (write-word word *stream*)
    word))

(defun fmt-op (tag opcd func ra rb rc)
  (check-args (tag 4) (opcd 6) (func 7) (ra 5) (rb 5) (rc 5))
  (let ((word (logior (ash tag 32)
                      (ash opcd 25)
                      (ash ra 20)
                      (ash rb 15)
                      (ash func 5)
                      rc)))
    (write-word word *stream*)
    word))

(defun fmt-op/i (tag opcd func ra ib rc)
  (check-args (tag 4) (opcd 6) (func 7) (ra 5) (ib 8) (rc 5))
  (let ((word (logior (ash tag 32)
                      (ash opcd 25)
                      (ash ra 20)
                      (ash ib 15)
                      (ash func 5)
                      rc)))
    (write-word word *stream*)
    word))

(macrolet ((define-memop (name opcode)
             `(define-instruction ,name (ra rb disp &key (tag 15))
                (unless *preprocess*
                  (let ((word (fmt-mem tag ,opcode ra rb disp)))
                    (format *error-output* "~5,'0X: ~9,'0X ;; (~a ~a ~a ~a):~X~%"
                            *pc* word ',name ra rb disp tag)))
                (incf *pc*))))
  (define-memop :lda  #x08)
  (define-memop :ldah #x09)
  (define-memop :ldl  #x28)
  (define-memop :stl  #x2c)
  (define-memop :jmp  #x1a))

(macrolet ((define-braop (name opcode)
             `(define-instruction ,name (ra disp &key (tag 15))
                (unless *preprocess*
                  (let ((word (if (keywordp disp)
                                  (let ((disp (assoc disp *label-table*)))
                                    (unless (and disp (numberp (cdr disp)))
                                      (error "invalid label"))
                                    (fmt-bra tag ,opcode ra
                                             (logand (- (cdr disp) *pc*)
                                                     (1- (ash 1 21)))))
                                  (fmt-bra tag ,opcode ra disp))))
                    (format *error-output* "~5,'0X: ~9,'0X ;; (~a ~a ~a):~X~%"
                            *pc* word ',name ra disp tag)))
                (incf *pc*))))
  (define-braop :beq #x39)
  (define-braop :bne #x3d)
  (define-braop :br  #x30))

(macrolet ((define-opop (name opcode opfunc)
             (let ((name/i (intern (concatenate 'string (symbol-name name) (symbol-name :i)) :keyword)))
               `(progn
                  (define-instruction ,name (ra rb rc &key (tag 15))
                    (unless *preprocess*
                      (let ((word (fmt-op tag ,opcode ,opfunc ra rb rc)))
                        (format *error-output* "~5,'0X: ~9,'0X ;; (~a ~a ~a ~a):~X~%"
                                *pc* word ',name ra rb rc tag)))
                    (incf *pc*))
                  (define-instruction ,name/i (ra ib rc &key (tag 15))
                    (unless *preprocess*
                      (let ((word (fmt-op/i tag ,opcode ,opfunc ra ib rc)))
                        (format *error-output* "~5,'0X: ~9,'0X ;; (~a ~a ~a ~a):~X~%"
                                *pc* word ',name/i ra ib rc tag)))
                    (incf *pc*))))))
  (define-opop :addl   #x10 #x00)
  (define-opop :subl   #x10 #x09)
  (define-opop :cmpeq  #x10 #x2d)
  (define-opop :cmple  #x10 #x6d)
  (define-opop :cmplt  #x10 #x4d)
  (define-opop :band   #x11 #x00)
  (define-opop :bis    #x11 #x20)
  (define-opop :xor    #x11 #x40)
  (define-opop :eqv    #x11 #x48)
  (define-opop :sll    #x12 #x39)
  (define-opop :srl    #x12 #x34)
  (define-opop :sra    #x12 #x3c)
  (define-opop :tcmpeq #x13 #x01)
  (define-opop :tcmpne #x13 #x02))

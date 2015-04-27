(defpackage retrokitchen-asm
  (:use :common-lisp)
  (:export :with-assemble
           :lda
           :ldah
           :ldl
           :ldah
           :jmp
           :beq
           :bne
           :br
           :addl
           :subl
           :cmpeq
           :cmple
           :cmplt
           :and
           :bis
           :xor
           :eqv
           :sll
           :srl
           :sra
           :tcmpeq
           :tcmpne
           :addl/i
           :subl/i
           :cmpeq/i
           :cmple/i
           :cmplt/i
           :and/i
           :bis/i
           :xor/i
           :eqv/i
           :sll/i
           :srl/i
           :sra/i
           :tcmpeq/i
           :tcmpne/i))

(in-package :retrokitchen-asm)

(defvar *pc*)
(defvar *label-table*)
(defvar *stream*)
(defvar *preprocess*)

(defmacro with-assemble (stream offset &rest body)
  (let ((body (mapcar (lambda (x) (if (keywordp x) `(label ,x) x))
                      body)))
    `(let ((*label-table* nil)
           (*stream* ,stream))
       (let ((*pc* ,offset) (*preprocess* t))   ,@body)
       (let ((*pc* ,offset) (*preprocess* nil)) ,@body))))

(defun label (key)
  (when *preprocess* (push (cons key *pc*) *label-table*)))

(defun write-word (word stream)
    (write-byte (logand 255 word) stream)
    (write-byte (logand 255 (ash word -8)) stream)
    (write-byte (logand 255 (ash word -16)) stream)
    (write-byte (logand 255 (ash word -24)) stream)
    (write-byte (logand 255 (ash word -32)) stream))

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
  (write-word (logior (ash tag 32)
                      (ash opcd 25)
                      (ash ra 20)
                      (ash rb 15)
                      disp)
              *stream*))

(defun fmt-bra (tag opcd ra disp)
  (check-args (tag 4) (opcd 6) (ra 5) (disp 21))
  (write-word (logior (ash tag 32)
                      (ash opcd 25)
                      (ash ra 20)
                      disp)
              *stream*))

(defun fmt-op (tag opcd func ra rb rc)
  (check-args (tag 4) (opcd 6) (func 7) (ra 5) (rb 5) (rc 5))
  (write-word (logior (ash tag 32)
                      (ash opcd 25)
                      (ash ra 20)
                      (ash rb 15)
                      (ash func 5)
                      rc)
              *stream*))

(defun fmt-op/i (tag opcd func ra ib rc)
  (check-args (tag 4) (opcd 6) (func 7) (ra 5) (ib 8) (rc 5))
  (write-word (logior (ash tag 32)
                      (ash opcd 25)
                      (ash ra 20)
                      (ash ib 15)
                      (ash func 5)
                      rc)
              *stream*))

(macrolet ((define-memop (name opcode)
             `(defun ,name (ra rb disp &key (tag 15))
                (unless *preprocess* (fmt-mem tag ,opcode ra rb disp))
                (incf *pc*))))
  (define-memop lda  #x08)
  (define-memop ldah #x09)
  (define-memop ldl  #x28)
  (define-memop stl  #x2c)
  (define-memop jmp  #x1a))

(macrolet ((define-braop (name opcode)
             `(defun ,name (ra disp &key (tag 15))
                (incf *pc*)
                (unless *preprocess*
                  (if (keywordp disp)
                      (let ((disp (assoc disp *label-table*)))
                        (unless (and disp (numberp (cdr disp)))
                          (error "invalid label"))
                        (fmt-bra tag ,opcode ra
                                 (logand (- (cdr disp) *pc*)
                                         (1- (ash 1 21)))))
                      (fmt-bra tag ,opcode ra disp))))))
  (define-braop beq #x39)
  (define-braop bne #x3d)
  (define-braop br  #x30))

(macrolet ((define-opop (name opcode opfunc)
             `(progn
                (defun ,name (ra rb rc &key (tag 15))
                  (fmt-op tag ,opcode ,opfunc ra rb rc)
                  (incf *pc*))
                (defun ,(intern (concatenate 'string (symbol-name name) "/I"))
                    (ra ib rc &key (tag 15))
                  (fmt-op/i tag ,opcode ,opfunc ra ib rc)
                  (incf *pc*)))))
  (define-opop addl   #x10 #x00)
  (define-opop subl   #x10 #x09)
  (define-opop cmpeq  #x10 #x2d)
  (define-opop cmple  #x10 #x6d)
  (define-opop cmplt  #x10 #x4d)
  (define-opop band    #x11 #x00)
  (define-opop bis    #x11 #x20)
  (define-opop xor    #x11 #x40)
  (define-opop eqv    #x11 #x48)
  (define-opop sll    #x12 #x39)
  (define-opop srl    #x12 #x34)
  (define-opop sra    #x12 #x3c)
  (define-opop tcmpeq #x13 #x01)
  (define-opop tcmpne #x13 #x02))

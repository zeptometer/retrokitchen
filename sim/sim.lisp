(defpackage retrokitchen-sim
  (:use :common-lisp)
  (:export :simulate))

(in-package :retrokitchen-sim)

;; word
(defstruct (tword (:constructor make-tword (tag data)))
  tag data)

(defun validate-tword (tword)
  (assert (tword-p tword))
  (assert (<= 0 (tword-tag  tword) 15))
  (assert (<= 0 (tword-data tword) (1- (ash 1 32)))))

(defun to-tword (inst)
  (let ((tword (make-tword (ash inst 32) (mod inst (ash 1 32)))))
    (validate-tword tword)
    tword))

;; state
(defstruct (state (:constructor make-state (pc regfile mem input output)))
  pc regfile mem input output)

;;; simulate
(defun simulate (inst input output)
  (let ((mem (make-array (ash 1 20) :initial-element (make-tword 15 0)))
        (regfile (make-array 32 :initial-element (make-tword 15 0))))
    (dotimes (i (length inst))
      (setf (aref mem i) (to-tword (aref inst i))))
    (do-simulate (make-state 0 regfile mem input output))))

(defun opcode (inst) (extract inst 26 31))

(defun do-simulate (st)
   (let ((inst (aref (state-mem st) (state-pc st)))
         (prev-pc (state-pc st)))
     (case (opcode inst)
       (#x08 (lda  inst st))
       (#x09 (ldah inst st))
       (#x28 (ldl  inst st))
       (#x2c (stl  inst st))
       (#x39 (beq  inst st))
       (#x3d (bne  inst st))
       (#x30 (br   inst st))
       (#x1a (jmp  inst st))
       (#x10 (ia   inst st))
       (#x11 (lg   inst st))
       (#x12 (sh   inst st))
       (#x13 (tag  inst st)))

     (dotimes (i 31)
       (validate-tword (aref (state-regfile st) i)))
     (setf (aref (state-regfile st) 31) (make-tword 15 0))

     (if (= prev-pc (state-pc st))
         (dump-register st)
         (do-simulate st))))

(defun dump-register (st)
  (format t "PC: ~a~%" (state-pc st))
  (dotimes (i 32)
    (format t "R~a: ~a~%" i (aref (state-regfile st) i))))

;; utility for fixed-point number
(defun take32 (a) (logand a (1- (ash 1 32))))
(defun signed-extend32 (n width)
  (if (zerop (extract n (1- width) width))
      n
      (- n (ash 1 width))))

;; destructuring macro
(defun extract (word from to)
  (logand (1- (ash 1 (- to from))) (ash word (- from))))

(defmacro bind-mem ((tag ra rb disp) inst &body body)
  (let ((data (gensym "data")))
    `(let ((,tag  (tword-tag ,inst))
           (,data (tword-data ,inst)))
       (declare (ignorable ,tag))
       (let ((,ra (extract ,data 21 26))
             (,rb (extract ,data 16 21))
             (,disp (extract ,data 0 16)))
         ,@body))))

(defmacro bind-bra ((tag ra disp) inst &body body)
  (let ((data (gensym "data")))
    `(let ((,tag  (tword-tag ,inst))
           (,data (tword-data ,inst)))
       (declare (ignorable ,tag))
       (let ((,ra (extract ,data 21 26))
             (,disp (extract ,data 0 21)))
         ,@body))))

(defmacro bind-op ((tag ra rb rc lit lit-p func) inst &body body)
  (let ((data (gensym "data")))
    `(let ((,tag  (tword-tag  ,inst))
           (,data (tword-data ,inst)))
       (declare (ignorable ,tag))
       (let ((,ra   (extract ,data 21 26))
             (,rb   (extract ,data 16 21))
             (,rc   (extract ,data 0  5))
             (,lit  (extract ,data 13 21))
             (,lit-p (extract ,data 12 13))
             (,func (extract ,data 5  12)))
         ,@body))))


;;; Operations
(defun lda (inst st)
  (incf (state-pc st))
  (bind-mem (tag ra rb disp) inst
    (setf (aref (state-regfile st) ra)
          (make-tword tag
                      (take32 (+ (signed-extend32 disp 16)
                                 (aref (state-regfile st) rb)))))))

(defun ldah (inst st)
  (incf (state-pc st))
  (bind-mem (tag ra rb disp) inst
    (setf (aref (state-regfile st) ra)
          (make-tword tag
                      (take32 (+ (ash disp 16)
                                 (aref (state-regfile st) rb)))))))

(defun ldl (inst st)
  (incf (state-pc st))
  (bind-mem (tag ra rb disp) inst
    (let ((addr (take32 (+ (aref (state-regfile st) rb)
                           (signed-extend32 disp 16)))))
      (setf (aref (state-regfile st) ra)
            (aref (state-mem st) addr)))))

(defun stl (inst st)
  (incf (state-pc st))
  (bind-mem (tag ra rb disp) inst
    (let ((addr (take32 (+ (aref (state-regfile st) rb)
                           (signed-extend32 disp 16)))))
      (setf (aref (state-mem st) addr)
            (aref (state-regfile st) ra)))))

(defun beq (inst st)
  (bind-bra (tag ra disp) inst
    (let ((a (aref (state-regfile st) ra)))
      (incf (aref (state-pc st))
            (if (= a 0)
                (signed-extend32 disp 21)
                1)))))

(defun bne (inst st)
  (bind-bra (tag ra disp) inst
    (let ((a (aref (state-regfile st) ra)))
      (incf (aref (state-pc st))
            (if (= a 0)
                1
                (signed-extend32 disp 21))))))

(defun br (inst st)
  (bind-bra (tag ra disp) inst
    (setf (aref (state-regfile st) ra) 
          (make-tword tag (1+ (state-pc st))))
    (incf (aref (state-pc st))
          (signed-extend32 disp 21))))

(defun jmp (inst st)
  (bind-mem (tag ra rb disp) inst
    (declare (ignore disp))
    (setf (aref (state-regfile st) ra)
          (make-tword tag (1+ (state-pc st))))
    (incf (aref (state-pc st))
          (tword-data (aref (state-regfile st) rb)))))


(defun ia (inst st)
  (incf (state-pc st))
  (bind-op (tag ra rb rc lit lit-p func) inst
      (let ((a (tword-data (aref (state-regfile st) ra)))
            (b (if (zerop lit-p)
                   (tword-data (aref (state-regfile st) rb))
                   lit)))
        (setf (aref (state-regfile st) rc)
              (ecase func
                (#x00 (make-tword tag (take32 (+ a b))))
                (#x09 (make-tword tag (take32 (- a b))))
                (#x2d (make-tword tag (if (= a b) 1 0)))
                (#x4d (make-tword tag (if (< a b) 1 0)))
                (#x6d (make-tword tag (if (<= a b) 1 0))))))))

(defun lg (inst st)
  (incf (state-pc st))
  (bind-op (tag ra rb rc lit lit-p func) inst
      (let ((a (tword-data (aref (state-regfile st) ra)))
            (b (if (zerop lit-p)
                   (tword-data (aref (state-regfile st) rb))
                   lit)))
        (setf (aref (state-regfile st) rc)
              (ecase func
                (#x00 (make-tword tag (take32 (logand a b))))
                (#x20 (make-tword tag (take32 (logior a b))))
                (#x40 (make-tword tag (take32 (logxor a b))))
                (#x48 (make-tword tag (take32 (logxor a (lognot b))))))))))

(defun sh (inst st)
  (incf (state-pc st))
  (bind-op (tag ra rb rc lit lit-p func) inst
      (let ((a (aref (state-regfile st) ra))
            (b (if (zerop lit-p)
                   (tword-data (aref (state-regfile st) rb))
                   lit)))
        (setf (aref (state-regfile st) rc)
              (ecase func
                (#x39 (make-tword tag (take32 (ash a b))))
                (#x34 (make-tword tag (take32 (ash a (- b)))))
                (#x3c (make-tword tag (take32 (ash (signed-extend32 a 32) (- b))))))))))

(defun tag (inst st)
  (incf (state-pc st))
  (bind-op (tag ra rb rc lit lit-p func) inst
      (let ((at (tword-tag (aref (state-regfile st) ra)))
            (b (if (zerop lit-p)
                   (tword-data (aref (state-regfile st) rb))
                   lit)))
        (setf (aref (state-regfile st) rc)
              (ecase func
                (#x00 (make-tword tag (if (= at b) 1 0)))
                (#x01 (make-tword tag (if (= at b) 0 1))))))))

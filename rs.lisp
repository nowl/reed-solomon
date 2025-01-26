(in-package :rs)

;; x^3 + x + 1
(defparameter *prim-poly-3* #b1011)

;; x^8 + x^4 + x^3 + x^2 + 1
(defparameter *prim-poly-8* #b100011101)

(defun log-list (num-bits prim-poly)
  "generate the alphabet by multiplying and xoring the primitive
polynomial"
  (let ((max-val (expt 2 num-bits)))
    (loop with v = 1
          for n from 1 below max-val
          finally (return acc)
          collecting
          (prog1
              v
            (let* ((tmp (ash v 1))
                   (new (if (>= tmp max-val)
                            (logxor tmp prim-poly)
                            tmp)))
              (setf v new)))
            into acc)))

(defun generate-log-tables (log-list)
  "generate log and inverse log lookups for faster polynomial
multiplication"
  (let ((log-inv (make-array (1+ (length log-list))))
        (log-forward (make-array (1+ (length log-list)))))
    (loop for val in log-list for n from 0 do
      (setf (aref log-inv n) val)
      (setf (aref log-forward val) n))
    (values log-forward log-inv)))

(defclass poly ()
  ((prim-poly :initarg :prim-poly :type fixnum)
   (nbits :initarg :nbits :type fixnum)
   (nvalues :type fixnum)
   (logf :type simple-vector)
   (logr :type simple-vector))
  (:documentation "class to hold metadata for algebraic operations on polynomials"))

(defmethod initialize-instance :after ((instance poly) &key prim-poly nbits)
  (multiple-value-bind (logf logr)
      (generate-log-tables (log-list nbits prim-poly))
    (setf (slot-value instance 'logf) logf
          (slot-value instance 'logr) logr
          (slot-value instance 'nvalues) (expt 2 nbits))))

(defun zeros (n)
  "helper function that returns a sequence of n zeros"
  (declare (fixnum n))
  (loop for c below n collect 0))

(defun add (p1 p2)
  "adds two polynomials"
  (logxor p1 p2))

(defun add-poly (p1 p2)
  "adds two sequences of polynomials"
  (assert (= (length p1) (length p2)))
  (loop for a in p1
        for b in p2
        collect (add a b)))

(defun mult (o p1 p2)
  "multiply two polynomials"
  (declare (poly o))
  (with-slots (nvalues logf logr) o
    (if (or (zerop p1) (zerop p2))
        0
        (let* ((total (1- nvalues))
               (v1 (aref logf p1))
               (v2 (aref logf p2))
               (total (mod (+ v1 v2) total)))
          (aref logr total)))))

(defun mult-inv (o val)
  "calculates multiplicative inverse of the given polynomial"
  (declare (poly o))
  (with-slots (nvalues logf logr) o
    (if (= val 1)
        1
        (let ((v1 (aref logf val))
              (total (1- nvalues)))
          (aref logr (- total v1))))))

(defun seq-256 (seq)
  "helper type utility to check if sequence contains only values between
0 and 255"
  (declare (sequence seq))
  (every #'(lambda (n) (and (>= n 0) (< n 256))) seq))

(defun mult-polys (o p1 p2)
  "multiplies two sequences of polynomials"
  (declare (poly o) ((satisfies seq-256) p1 p2))
  (let ((r (make-sequence 'vector (1- (+ (length p1) (length p2))) :initial-element 0)))
    (loop for v2 in p2 for v2n from 0 do
      (loop for v1 in p1 for v1n from 0 do
        (let ((prev-val (aref r (+ v1n v2n)))
              (new-val (mult o v1 v2)))
          (setf (aref r (+ v1n v2n)) (add prev-val new-val)))))
    (coerce r 'list)))

(defun make-generator (poly size)
  "create a generator polynomial of a given size"
  (labels ((rec (size)
             (with-slots (nvalues logr) poly
               (if (= 1 size)
                   (list 1 1)
                   (let* ((size-minus-1-poly (rec (1- size)))
                          (shifted-poly (coerce (append size-minus-1-poly '(0)) 'vector))
                          (normalized-poly (coerce (append '(0) size-minus-1-poly) 'vector))
                          (alpha-val (aref logr (1- size))))
                     (loop for n below (length normalized-poly) do
                       (let ((new-val (add (mult poly (aref normalized-poly n) alpha-val)
                                           (aref shifted-poly n))))
                         (setf (aref normalized-poly n) new-val)))
                     (coerce normalized-poly 'list))))))
    (rec size)))

(defun divide-polys (o numerator denominator input-size)
  "polynomial division"
  (declare  (poly o) ((satisfies seq-256) numerator denominator))
  (assert (= (length numerator) (length denominator)))
  (let ((r nil)
        (num-tmp (copy-seq numerator))
        (inv (mult-inv o (car denominator))))
    (loop for n below input-size do
      (let* ((target-val (car num-tmp))
             (multiplier (mult o inv target-val))
             (tmp2 (mult-polys o denominator (list multiplier))))
        (assert (= (length tmp2) (length denominator)))
        (assert (zerop (add (car num-tmp) (car tmp2))))
        (setf r (cons multiplier r)
              num-tmp (add-poly (cdr num-tmp) 
                                (butlast (cdr tmp2) (- (length tmp2) (length num-tmp)))
                                ))))
    (append (nreverse r) num-tmp)))

(defclass encoder ()
  ((poly :initarg :poly :type poly)
   (num-input-symbols :initarg :num-input-symbols :type unsigned-byte)
   (num-error-check-symbols :initarg :num-error-check-symbols :type unsigned-byte)
   (gen-poly :initarg :gen-poly :type simple-vector)
   (input-multiplier :type simple-vector)
   (gen-multiplier :type simple-vector))
  (:documentation "Metadata to use for Reed Solomon encoder"))

(defun make-encoder (prim-poly num-input-symbols num-error-check-symbols)
  "Makes an instance of a Reed Solomon encoder that can encode blocks of
input symbols of length num-input-symbols and will return error check
symbols of length num-error-check-symbols."
  (declare ((member prim-poly-8 prim-poly-3) prim-poly)
           (unsigned-byte num-input-symbols num-error-check-symbols))
  (multiple-value-bind (prim-poly nbits)
      (ecase prim-poly
        (prim-poly-8 (values *prim-poly-8* 8))
        (prim-poly-3 (values *prim-poly-3* 3)))
    (let* ((poly (make-instance 'poly :nbits nbits :prim-poly prim-poly))
           (gen-poly (make-generator poly num-error-check-symbols))
           (encoder (make-instance 'encoder :poly poly
                                            :num-error-check-symbols num-error-check-symbols
                                            :num-input-symbols num-input-symbols
                                            :gen-poly gen-poly)))
      (setf (slot-value encoder 'input-multiplier) (zeros num-error-check-symbols)
            (slot-value encoder 'gen-multiplier) (zeros (1- num-input-symbols)))
      encoder)))

(defun encode (encoder input &optional prefix-input)
  "Encodes a block of input using the corresponding encoder."
  (declare (encoder encoder) ((satisfies seq-256) input))
  (with-slots (poly gen-poly input-multiplier gen-multiplier num-error-check-symbols num-input-symbols) encoder
    (let* ((poly-division (divide-polys poly
                                        (append input input-multiplier)
                                        (append gen-poly gen-multiplier)
                                        num-input-symbols))
           (error-check-symbols (last poly-division num-error-check-symbols)))
      (if prefix-input
          (append input error-check-symbols)
          error-check-symbols))))

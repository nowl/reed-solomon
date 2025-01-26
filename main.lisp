;; basic tests

(defparameter *test-input-data* (list #b00100000 #b01011011 #b00001011 #b01111000 #b11010001 #b01110010 #b11011100 #b01001101 #b01000011 #b01000000 #b11101100 #b00010001 #b11101100 #b00010001 #b11101100 #b00010001))

(defparameter *test-encoded-data* (list #b00100000 #b01011011 #b00001011 #b01111000 #b11010001 #b01110010 #b11011100 #b01001101 #b01000011 #b01000000 #b11101100 #b00010001 #b11101100 #b00010001 #b11101100 #b00010001 #b10100000 #b01001000 #b11111001 #b00100011 #b00001010 #b00000110 #b11000011 #b00011111 #b01011110 #b00011011 #b01110001 #b00100101 #b01111100 #b10010001 #b01000010 #b01011010 #b00110110 #b10101000 #b00111000 #b10100010 #b00000010 #b01001101 #b10100010 #b00101001 #b10100011 #b11110011 #b01110111 #b00100101))

(defparameter *num-input-symbols* 16)
(defparameter *num-check-symbols* 28)

(assert (= (length *test-input-data*) *num-input-symbols*))

(let* ((encoder (rs:make-encoder 'rs:prim-poly-8 16 28))
       (encoded-data-block (rs:encode encoder *test-input-data* t)))
  (assert (equal encoded-data-block *test-encoded-data*)))

(let* ((encoder (rs:make-encoder 'rs:prim-poly-8 16 28))
       (encoded-data-block (rs:encode encoder *test-input-data*)))
  (assert (equal encoded-data-block (last *test-encoded-data* 28))))



(let* ((encoder (rs:make-encoder 'rs:prim-poly-8 13 7))
       (input-data (loop for c across "Hello, world!" collect (char-code c))))
  (format nil "~{~2,'0x ~}" (rs:encode encoder input-data nil)))


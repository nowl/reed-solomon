(defpackage :reed-solomon
  (:use #:cl)
  (:nicknames #:rs)
  (:export #:prim-poly-3
           #:prim-poly-8
           #:make-encoder
           #:encode))

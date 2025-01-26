(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'asdf)
  (require 'uiop))

(push (uiop/os:getcwd) asdf:*central-registry*)
(asdf:oos 'asdf:load-op "reed-solomon")

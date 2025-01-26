(defsystem "reed-solomon"
  :description "Reed Solomon encoding implementation."
  :version "0.0.1"
  :author "John Process <esologic@gmail.com>"
  :licence "MIT"
  :components ((:file "packages")
               (:file "rs" :depends-on ("packages"))
               (:file "main" :depends-on ("rs"))))

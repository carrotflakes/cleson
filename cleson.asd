#|
  This file is a part of cleson project.
  Copyright (c) 2015 carrotflakes
|#

#|
  Author: carrotflakes
|#

(in-package :cl-user)
(defpackage cleson-asd
  (:use :cl :asdf))
(in-package :cleson-asd)

(defsystem cleson
  :version "0.1"
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on (:alexandria)
  :components ((:module "src"
                :components
                ((:file "cleson")))
               (:module "lib"
                :depends-on ("src")
                :components
                ((:module "core"
                  :components
                  ((:file "collection"))))))
  :description "Egison-like pattern matching library"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cleson-test))))

#|
  This file is a part of cleson project.
  Copyright (c) 2015 carrotflakes
|#

(in-package :cl-user)
(defpackage cleson-test-asd
  (:use :cl :asdf))
(in-package :cleson-test-asd)

(defsystem cleson-test
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on (:cleson
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cleson"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))

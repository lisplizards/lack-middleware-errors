;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(defsystem "foo.lisp.lack-middleware-errors"
  :version "1.0.0"
  :author "John Newton"
  :license "Apache-2.0"
  :depends-on ("foo.lisp.http-response")
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("package"))
                 (:file "util" :depends-on ("package"))
                 (:file "package"))))
  :description "Lack middleware to handle errors"
  :in-order-to ((test-op (test-op "foo.lisp.lack-middleware-errors/tests"))))

(defsystem "foo.lisp.lack-middleware-errors/tests"
  :author "John Newton"
  :license "Apache-2.0"
  :depends-on ("foo.lisp.lack-middleware-errors"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main" :depends-on ("package"))
                 (:file "package"))))
  :description "Test system for foo.lisp.lack-middleware-errors"
  :perform (test-op (op c) (symbol-call :rove :run c)))

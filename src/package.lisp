;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-user)

(defpackage #:lack/middleware/errors/util
  (:use #:cl)
  (:import-from #:foo.lisp.http-response
                #:http-error
                #:status-code
                #:status-code-error
                #:status-code-to-text)
  (:export #:parse-response-code
           #:collect-static-files))

(defpackage #:lack/middleware/errors
  (:use #:cl)
  (:import-from #:foo.lisp.http-response
                #:http-error
                #:status-code
                #:status-code-error
                #:status-text-clack-response)
  (:export #:*lack-middleware-errors*))

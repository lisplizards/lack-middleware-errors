;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-user)

(defpackage #:lack/middleware/errors/tests/main
  (:use #:cl #:rove)
  (:import-from #:lack/middleware/errors
                #:*lack-middleware-errors*))

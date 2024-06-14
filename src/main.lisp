;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:lack/middleware/errors)

(defparameter *lack-middleware-errors*
  (lambda (clack-app &key app intercept)
    (declare (type function clack-app))
    (check-type app function)
    (check-type intercept function)
    (lambda (env)
      (declare (type list env)
               (type function app intercept))
      (block nil
        (handler-bind ((error
                         (lambda (condition)
                           (let* ((intercept-result
                                    (funcall intercept condition))
                                  (response-code
                                    (typecase intercept-result
                                      (null 500)
                                      (status-code-error intercept-result)
                                      (t
                                       (warn "Invalid return result (~A) from intercept for condition ~A"
                                             intercept-result
                                             condition)
                                       500))))
                             (declare (type t intercept-result)
                                      (type status-code-error response-code))
                             (setf (getf env :handled-error) condition
                                   (getf env :original-request-method) (getf env :request-method)
                                   (getf env :request-method) :GET
                                   (getf env :path-info) (format nil "/~D" response-code))
                             (handler-bind ((error
                                              (lambda (condition)
                                                (warn "Error signalled from LACK/MIDDLEWARE/ERRORS: ~A" condition)
                                                (return
                                                  (status-text-clack-response 500)))))
                               (return
                                 (funcall app env)))))))
          (funcall clack-app env))))))

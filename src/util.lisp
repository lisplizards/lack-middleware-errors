;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:lack/middleware/errors/util)

(defun parse-response-code (env)
  (let ((path-info (getf env :path-info)))
    (let ((response-code-string (if (char= #\/ (char path-info 0))
                                    (setf path-info (subseq path-info 1))
                                    path-info)))
      (multiple-value-bind (response-code size)
          (parse-integer response-code-string :junk-allowed t)
        (unless (and response-code
                     (= 3 size)
                     (typep response-code 'status-code-error))
          (warn "failed to parse error response code from path-info: ~A" path-info)
          (return-from parse-response-code
            500))
        response-code))))

(defun collect-static-files (directory &key file-types)
  "Returns three values representing resources found in DIRECTORY:
1. static files (list of pathnames)
2. static-file namestrings (list of strings)
3. static-file media-types (list of strings)

Parameter FILE-TYPES is an association list: each cons pair has a
media-type string as the key and a file-type extension string as
the datum, ex. `'((\"text/html\" . \"html\"))'."
  (check-type directory pathname)
  (check-type file-types list)
  (assert (uiop:directory-pathname-p directory)
          nil
          "DIRECTORY is not a directory: ~A" directory)
  (dolist (pair file-types)
    (check-type pair cons)
    (destructuring-bind (media-type . file-type)
        pair
      (check-type media-type string)
      (check-type file-type string)))
  (let ((static-files (remove-if-not (lambda (pathname)
                                       (block nil
                                         (unless (rassoc (pathname-type pathname) file-types
                                                         :test #'string=)
                                           (return))
                                         (let ((response-code
                                                 (multiple-value-bind (int size)
                                                     (parse-integer (pathname-name pathname)
                                                                    :junk-allowed t)
                                                   (unless (and int (= 3 size))
                                                     (return))
                                                   int)))
                                           (typep response-code 'status-code-error))))
                                     (uiop:directory-files directory))))
    (values static-files
            (mapcar #'file-namestring static-files)
            (mapcar #'car file-types))))

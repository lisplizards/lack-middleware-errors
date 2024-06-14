# lack-middleware-errors

## Usage

Wrap app:

```lisp
(funcall lack/middleware/errors:*lack-middleware-errors*
         *app*
         :app (foo.lisp.vinland/errors-app/simple/basic:make-app
               :root demo-app/config:*static-errors-directory*
               :static-file-types demo-app/http-error:*static-file-types*
               :handlers demo-app/http-error:*http-errors*)
         :intercept (lambda (condition)
                      (declare (type error condition))
                      (typecase condition
                        (foo.lisp.http-response:http-error (slot-value condition 'foo.lisp.http-response:status-code))
                        (foo.lisp.raven:no-route-error 404))))
```

Lack Builder:

```lisp
(lack:builder
 (:errors :app (foo.lisp.vinland/errors-app/simple/basic:make-app
                :root demo-app/config:*static-errors-directory*
                :static-file-types demo-app/http-error:*static-file-types*
                :handlers demo-app/http-error:*http-errors*)
          :intercept (lambda (condition)
                       (declare (type error condition))
                       (typecase condition
                         (foo.lisp.http-response:http-error (slot-value condition 'foo.lisp.http-response:status-code))
                         (foo.lisp.raven:no-route-error 404))))
 *app*)
```

### Options

* `INTERCEPT`: required; function that takes an error condition and returns an HTTP error status response code; a NIL return value results in response code 500
* `APP`: required; function that takes an ENV list and returns a response (a Clack application); before forwarding to the errors app, the middleware re-writes the path-info to the status code returned by function `INTERCEPT` and re-writes the request-method to GET

## Development

Run tests:

```lisp
(asdf:test-system :foo.lisp.lack-middleware-errors)
```

## Installation

Not in Quicklisp, so clone to "local-projects/".

## Dependencies

* [foo.lisp.http-response](https://github.com/lisplizards/http-response)

### Tests

* [rove](https://github.com/fukamachi/rove)

## Author

* John Newton (<a href="mailto:jnewton@lisplizards.dev">jnewton@lisplizards.dev</a>)

## Copyright

Copyright (c) 2024 John Newton

## License

Apache-2.0

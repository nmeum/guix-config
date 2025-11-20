(define-module (mycfg transformations)
  #:use-module (gnu system))

(define-public (add-users users)
  (lambda (os)
    (operating-system
      (inherit os)
      (users
        (append
          (operating-system-user-accounts os)
          users)))))

(define-public (add-services services)
  (lambda (os)
    (operating-system
      (inherit os)
      (services
        (append
          (operating-system-user-services os)
          services)))))

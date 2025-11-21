(define-module (mycfg home)
  #:use-module (gnu home)
  #:use-module (gnu services))

(define (my-essential-services he)
  ;; I don't want Guix home to overwrite my ~/.profile.
  ;; Therefore, I remove the services responsible for that.
  (filter
    (lambda (s)
      ;; The home-shell-profile creates the ~/.profile file.
      (not (eqv? (service-type-name (service-kind s)) 'home-shell-profile)))
    (home-environment-essential-services he)))

(define-public (base-home packages)
  (let ((home-environment-base
          ;; Need to set packages here, otherwise the essential services
          ;; operate on an empty package set and no package is installed.
          (home-environment
            (packages packages))))
    (home-environment
      ;; Awful hack to change the default essential services.
      (inherit home-environment-base)
      (essential-services
        (my-essential-services home-environment-base)))))

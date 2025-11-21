(use-modules (gnu home)
             (gnu packages)

             (mycfg home)
             (mycfg home packages)

             (nmeum packages desktop)
             (nmeum packages misc))

(define my-packages
  (append
    packages-networking
    packages-programming
    packages-tools))

(home-environment
  (inherit
    (base-home
      (specifications->packages my-packages))))

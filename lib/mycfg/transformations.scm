(define-module (mycfg transformations)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module ((ice-9 optargs) #:select (define*-public)))

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

(define-public (add-file-systems fs)
  (lambda (os)
    (operating-system
      (inherit os)
      (file-systems
        (append
          (operating-system-file-systems os)
          fs)))))

(define*-public (btrfs-subvolumes* device subvols #:key (dependencies '()))
  (let* ((btrfs-subvol (lambda* (mnt flags #:optional (opts '()))
                        (file-system
                          (mount-point mnt)
                          (device device)
                          (type "btrfs")
                          (flags flags)
                          (options (alist->file-system-options
                                     (cons (cons "subvol" mnt) opts)))
                          (dependencies dependencies))))
         (file-systems (map (lambda (subvol)
                              (apply btrfs-subvol subvol))
                            subvols)))
    (add-file-systems file-systems)))

(define*-public (btrfs-subvolumes device #:rest rest)
  (apply
    btrfs-subvolumes*
    device
    '(("/"
       (no-atime)
       ("rw"
        "ssd"
        ("compress" . "lzo")
        ("space_cache" . "v2")))
      ("/home" (no-atime no-suid no-dev))
      ("/etc" (no-atime no-suid no-dev no-exec))
      ("/var" (no-atime no-suid no-dev))
      ("/var/log" (no-atime no-suid no-dev no-exec))
      ;; TODO: Consider using a tmpfs for /var/tmp
      ("/var/tmp" (no-atime no-suid no-dev no-exec)))
    rest))

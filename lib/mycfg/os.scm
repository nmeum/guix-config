(define-module (mycfg os)
  #:use-module (gnu)
  #:use-module (gnu services dns)
  #:use-module (gnu services linux)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services sysctl)
  #:use-module (gnu system locale)

  #:use-module ((nmeum packages desktop) #:select (font-terminus-patched)))

(define-public (base-os hostname bootloader bootfs rootfs)
  (operating-system
    (locale "en_US.utf8")
    (locale-definitions
      (list (locale-definition
              (name "en_US.utf8") (source "en_US"))))

    (timezone "Europe/Berlin")
    (keyboard-layout (keyboard-layout "de" "neo"))
    (host-name hostname)

    ;; Don't copy any skeleton files for newly created users.
    (skeletons '())

    ;; Allow sudo use without password authentication.
    (sudoers-file
      (plain-file "sudoers" "root ALL=(ALL) ALL\n%wheel ALL=(ALL) NOPASSWD: ALL\n"))

    (bootloader bootloader)
    (file-systems
      (cons* bootfs
             rootfs
             (file-system
               (check? #f)
               (mount-point "/tmp")
               (device "none")
               (type "tmpfs"))
             (file-system
               (check? #f)
               (mount-point "/proc")
               (type "proc")
               (device "none")
               (flags '(remount))
               (options "hidepid=invisible"))
             %base-file-systems))

    (services
      (append (list
                (service fstrim-service-type)

                (service unbound-service-type
                         (unbound-configuration
                           (forward-zone
                             (list
                               (unbound-zone
                                 (name ".")
                                 (forward-addr '("149.112.112.112#dns.quad9.net"
                                                 "2620:fe::9#dns.quad9.net"))
                                 (forward-tls-upstream #t))))))

                (service openssh-service-type
                         (openssh-configuration
                           (allow-agent-forwarding? #f)
                           (password-authentication? #f)))

                (service openntpd-service-type
                         (openntpd-configuration
                           (servers '("europe.pool.ntp.org"))
                           (constraint-from
                             ;; TODO: Consider adding IPv4/IPv6 addresses here
                             ;; to get constraints, even when DNS is broken atm.
                             '("dns.quad9.net" "www.google.com")))))

              (modify-services %base-services
                               ;; Set a custom console font.
                               (console-font-service-type config =>
                                                          (map (lambda (tty)
                                                                 (cons tty
                                                                       #~(string-append
                                                                           #+font-terminus-patched
                                                                           "/share/consolefonts/ter-v16n.psf.gz")))
                                                               '("tty1" "tty2" "tty3" "tty4" "tty5" "tty6")))

                               ;; Enable additional sysctls.
                               (sysctl-service-type config =>
                                                    (sysctl-configuration
                                                      (inherit config)
                                                      (settings
                                                        (append %default-sysctl-settings
                                                                '(("kernel.dmesg_restrict" . "1")
                                                                  ("kernel.kptr_restrict" . "1")))))))))))

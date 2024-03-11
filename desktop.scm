(use-modules (gnu)
             (gnu packages shells)
             (gnu packages dns)
             (gnu services desktop)
             (gnu services networking)
             (gnu services sound)
             (gnu services ssh)
             (gnu system locale)
             (gnu system pam)

             (nmeum packages desktop)
             (nmeum packages networking)
             (nmeum services networking)
             (nmeum services system)
             ((nongnu packages linux) #:select (linux linux-firmware)))

;; The signing key for the nonguix substitutes embedded as plain text.
;;
;; See: https://gitlab.com/nonguix/nonguix
(define nonguix-signkey
  "(public-key
     (ecc
       (curve Ed25519)
       (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))")

;; Guix configuration which enables nonguix substitutes.
;;
;; See https://guix.gnu.org/en/manual/devel/en/guix.html#index-guix_002dconfiguration
(define (nonguix-config config)
  (guix-configuration
    (inherit config)
    (substitute-urls
      (append (list "https://substitutes.nonguix.org")
              %default-substitute-urls))
    (authorized-keys
      (append (list (plain-file "non-guix.pub" nonguix-signkey))
              %default-authorized-guix-keys))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(operating-system
  (kernel linux)
  (firmware (list linux-firmware))

  (locale "en_US.utf8")
  (locale-definitions
    (list (locale-definition
            (name "en_US.utf8") (source "en_US"))))

  (timezone "Europe/Berlin")
  (keyboard-layout (keyboard-layout "de" "neo"))
  (host-name "hassium")

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "soeren")
                  (comment "Sören Tempel")
                  (group "users")
                  (shell (file-append loksh "/bin/ksh"))
                  (home-directory "/home/soeren")
                  (supplementary-groups '("wheel" "netdev" "audio" "video" "seat")))
                %base-user-accounts))

  ;; Allow sudo use without password authentication.
  ;;
  ;; XXX: A bit ugly since there is no declarative API for this yet.
  (sudoers-file
    (plain-file "sudoers" "root ALL=(ALL) ALL\n%wheel ALL=(ALL) NOPASSWD: ALL\n"))

  ;; Packages installed system-wide.  Users can also install packages
  ;; under their own account: use 'guix search KEYWORD' to search
  ;; for packages and 'guix install PACKAGE' to install a package.
  (packages (append (map specification->package
                         '("nss-certs"
                           "openntpd"
                           "dumb-runtime-dir"
                           "unbound"
                           "seatd"
                           "vim"
                           "git"
                           "loksh"))
                    %base-packages))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (append (list
             (service unbound-service-type
                      (unbound-configuration
                        (forward-zone
                          '((name . ".")
                            (forward-addr . "149.112.112.112#dns.quad9.net")
                            (forward-addr . "2620:fe::9#dns.quad9.net")
                            (forward-tls-upstream . yes)))))

             (service seatd-service-type)
             ; (service elogind-service-type
             ;          (elogind-configuration
             ;            (kill-user-processes? #f)))

             (service alsa-service-type
                      (alsa-configuration
                        (pulseaudio? #f)
                        ;; TODO: Add a declarative unbound-like configuration.
                        ;;
                        ;; XXX: Might also just need to blacklist some
                        ;; module. Have to look at my Alpine config.
                        (extra-options "defaults.pcm.card 1\ndefaults.ctl.card 1\n")))

             (service openntpd-service-type
                      (openntpd-configuration
                        (servers '("europe.pool.ntp.org"))
                        (constraint-from
                          '(;; Quad9 DNS (IPv4)
                            "9.9.9.9"
                            ;; Quad9 DNS (IPv6)
                            "2620:fe::fe"
                            ;; Google LLC (DNS)
                            "www.google.com"))))

             (service openssh-service-type
                      (openssh-configuration
                        (allow-agent-forwarding? #f)
                        (password-authentication? #f)))

             (service sane-service-type)

             ; (service dhcp-client-service-type
             ;          (dhcp-client-configuration
             ;            (package dhcpcd))))

             (service dhcpcd-service-type
                      (dhcpcd-configuration
                        (options
                          '((hostname)
                            (duid)
                            (persistent)
                            (vendorclassid)
                            (slaac private)
                            (require dhcp_server_identifier)

                            (option rapid_commit interface_mtu)
                            (nooption nd_rdnss)
                            (nooption dhcp6_name_servers)
                            (nooption domain_name_servers domain_name domain_search)

                            (static "domain_name_servers=127.0.0.1")
                            (nohook hostname))))))

           (cons*
             (service login-xdg-runtime-service-type)
             (modify-services %base-services
               (delete login-service-type)
               (guix-service-type config  => (nonguix-config config))))))

  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (extra-initrd "/key-file.cpio")
                (keyboard-layout keyboard-layout)))

  (mapped-devices (list (mapped-device
                          (source (uuid "1a4403d8-9b5e-4714-a8ce-9e4920035b72"))
                          (target "cryptroot")
                          (type (luks-device-mapping-with-options
                                  #:key-file "/key-file.bin")))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems (cons* (file-system
                         (check? #f)
                         (mount-point "/tmp")
                         (device "none")
                         (type "tmpfs"))
                       (file-system
                         (check? #f)
                         (mount-point "/run/user")
                         (device "none")
                         (type "tmpfs"))

                       (file-system
                         (mount-point "/")
                         (device "/dev/mapper/cryptroot")
                         (type "btrfs")
                         (dependencies mapped-devices))
                       (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "781B-DB19" 'fat32))
                         (type "vfat")) %base-file-systems)))

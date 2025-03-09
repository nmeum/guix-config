(use-modules (gnu)
             (gnu packages shells)
             (gnu services dbus)
             (gnu services desktop)
             (gnu services dns)
             (gnu services networking)
             (gnu services mcron)
             (gnu services ssh)
             (gnu services sysctl)
             (gnu system locale)
             (gnu system pam)

             (gnu services dns)

             (nmeum packages misc)
             (nmeum packages desktop)
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
                  (shell (file-append loksh-bracketed "/bin/ksh"))
                  (home-directory "/home/soeren")
                  ;; Note: Without elogind, it is neccessary to also be in both
                  ;; the audio and the video group as seatd doesn't mediated access
                  ;; to audio/video devices.
                  (supplementary-groups '("wheel" "netdev")))
                %base-user-accounts))

  ;; Allow sudo use without password authentication.
  ;;
  ;; XXX: A bit ugly since there is no declarative API for this yet.
  (sudoers-file
    (plain-file "sudoers" "root ALL=(ALL) ALL\n%wheel ALL=(ALL) NOPASSWD: ALL\n"))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
    (append (list
              (service elogind-service-type)
              (service dbus-root-service-type)

              ;; TODO: btrfs snapshots (see comment regarding subvolumes below).
              ;;
              ;; XXX: mcron has not been designed to run anachronistically.
              ;; See: https://www.gnu.org/software/mcron/manual/mcron.html#Behaviour-on-laptops
              (let ((guix-gc #~(job '(next-hour '(12)) "guix gc -F 100G")))
                (simple-service 'guix-gc-cron
                                mcron-service-type
                                (list guix-gc)))

              ;; TODO: Needs cryptdiscards enabled on the LUKS volume (see below).
              ;(service fstrim-service-type)

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
                           '(;; Quad9 DNS (IPv4)
                             "9.9.9.9"
                             ;; Quad9 DNS (IPv6)
                             "2620:fe::fe"
                             ;; Google LLC (DNS)
                             "www.google.com"))))

              (service dhcpcd-service-type
                       (dhcpcd-configuration
                         (vendor-class-id "MSFT")
                         (option '("rapid_commit" "interface_mtu"))
                         (no-option '("nd_rdnss"
                                      "dhcp6_name_servers"
                                      "domain_name_servers"
                                      "domain_name"
                                      "domain_search"))
                         (static '("domain_name_servers=127.0.0.1"))
                         (no-hook '("hostname")))))

            (modify-services %base-services
                             ;; Enable substitutes for nonguix.
                             (guix-service-type config => (nonguix-config config))

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
                                             ("kernel.kptr_restrict" . "1")))))))))

  (bootloader (bootloader-configuration
                ;; Use a removable bootloader configuration here to prevent
                ;; Grub from updating UEFI boot entries, thereby making Guix
                ;; (instead of Alpine) the default entry.
                ;;
                ;; See the --removable and --no-nvram option of grub-install.
                (bootloader grub-efi-removable-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)
                (extra-initrd "/key-file.cpio")))

  (mapped-devices (list (mapped-device
                          (source (uuid "d9bd4aa0-bd68-4fef-b6a5-0657bd69daef"))
                          (target "cryptroot")
                          ;; TODO: Need to enable cryptdiscards here for SSDs.
                          ;;
                          ;; See https://issues.guix.gnu.org/73654
                          (type (luks-device-mapping-with-options
                                  #:key-file "/key-file.bin")))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems
    (let ((btrfs-subvol (lambda (mnt flags opts)
                          (file-system
                            (mount-point mnt)
                            (device "/dev/mapper/cryptroot")
                            (type "btrfs")
                            (flags flags)
                            (options (alist->file-system-options
                                       (cons (cons "subvol" mnt) opts)))
                            (dependencies mapped-devices)))))
      (cons* (file-system
               (check? #f)
               (mount-point "/tmp")
               (device "none")
               (type "tmpfs"))

             ;; TODO: Subvolumes for /home, /var, /var/log, /var/tmp, …
             ;;
             ;; Note: Btrfs does presently not support filesystem-specific
             ;; mount options on subvolume-granularity, generic ones work.
             ;;
             ;; See https://btrfs.readthedocs.io/en/stable/btrfs-subvolume.html#mount-options
             (btrfs-subvol "/"
                           '(no-atime)
                           '("rw"
                             "ssd"
                             ("compress" . "lzo")
                             ("space_cache" . "v2")))
             ;; TODO: Consider using a tmpfs for /var/tmp
             (btrfs-subvol "/var/tmp"
                           '(no-atime no-suid no-dev no-exec)
                           '())

             (file-system
               (mount-point "/boot/efi")
               (device (uuid "04FA-08B2" 'fat32))
               (type "vfat")) %base-file-systems))))

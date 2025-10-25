(use-modules (gnu)
             (gnu packages shells)
             (gnu services dbus)
             (gnu services desktop)
             (gnu services dns)
             (gnu services linux)
             (gnu services networking)
             (gnu services mcron)
             (gnu services ssh)
             (gnu services sysctl)
             (gnu system locale)
             (gnu system pam)

             (nmeum bootloader grub)
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
    ;; Run guix-daemon in non-privileged mode. This may become the default in the future.
    ;;
    ;; See https://codeberg.org/guix/guix-mirror/commit/ba53ff9cc403c7f0388e2dc932cb46e665e81be7
    (privileged? #f)
    (substitute-urls
      (append (list "https://substitutes.nonguix.org")
              %default-substitute-urls))
    (authorized-keys
      (append (list (plain-file "non-guix.pub" nonguix-signkey))
              %default-authorized-guix-keys))))

;; Custom Grub configuration which copies all needed files to /boot, allowing
;; for it to remain unencrypted and doing the LUKS unlock via the initramfs.
(define grub-efi-removable-bootloader-copy
  (let ((grub grub-efi-removable-bootloader))
    (bootloader
      (inherit grub)
      (installer (grub-copy (bootloader-installer grub)))
      (configuration-file-generator
        (configuration-file-generator-without-crypto-devices
          (bootloader-configuration-file-generator grub))))))

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
              (service fstrim-service-type)

              ;; TODO: btrfs snapshots (see comment regarding subvolumes below).
              ;;
              ;; XXX: mcron has not been designed to run anachronistically.
              ;; See: https://www.gnu.org/software/mcron/manual/mcron.html#Behaviour-on-laptops
              (let ((guix-gc #~(job '(next-hour '(12)) "guix gc -F 100G")))
                (simple-service 'guix-gc-cron
                                mcron-service-type
                                (list guix-gc)))

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
                (bootloader grub-efi-removable-bootloader-copy)
                (targets (list "/boot"))
                (keyboard-layout keyboard-layout)))

  (mapped-devices (list (mapped-device
                          (source (uuid "7c636d9f-c393-4446-b708-c0bc58ecdd59"))
                          (target "root")
                          (type luks-device-mapping)
                          (arguments '(#:allow-discards? #t)))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems
    ;; XXX: Flags are generic and opts are filesystem-specific mount options.
    (let ((btrfs-subvol (lambda* (mnt flags #:key (opts #f))
                          (file-system
                            (mount-point mnt)
                            (device "/dev/mapper/root")
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

             ;; XXX: Btrfs does presently not support filesystem-specific
             ;; mount options on subvolume-granularity, generic ones work.
             ;;
             ;; See https://btrfs.readthedocs.io/en/stable/btrfs-subvolume.html#mount-options
             (btrfs-subvol "/" '(no-atime)
                           #:opts '("rw"
                                    "ssd"
                                    ("compress" . "lzo")
                                    ("space_cache" . "v2")))
             (btrfs-subvol "/home" '(no-atime no-suid no-dev))
             (btrfs-subvol "/etc" '(no-atime no-suid no-dev no-exec))
             (btrfs-subvol "/var" '(no-atime no-suid no-dev))
             (btrfs-subvol "/var/log" '(no-atime no-suid no-dev no-exec))
             ;; TODO: Consider using a tmpfs for /var/tmp
             (btrfs-subvol "/var/tmp" '(no-atime no-suid no-dev no-exec))

             (file-system
               (mount-point "/boot")
               (device (uuid "CA96-426F" 'fat32))
               (type "vfat"))

             %base-file-systems))))

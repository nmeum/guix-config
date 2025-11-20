;; -*- mode: scheme; -*-
;; This is an operating system configuration template
;; for a "bare bones" setup, with no X11 display server.

(use-modules (gnu))
(use-service-modules networking ssh)
(use-package-modules screen ssh)

(operating-system
  (host-name "scandium")
  (timezone "Europe/Berlin")
  (locale "en_US.utf8")

  ;; Boot in "legacy" BIOS mode, assuming /dev/sdX is the
  ;; target hard disk, and "my-root" is the label of the target
  ;; root file system.
  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets '("/dev/sda"))))

  (initrd-modules (append (list "virtio_scsi") 
                          %base-initrd-modules))

  ;; It's fitting to support the equally bare bones ‘-nographic’
  ;; QEMU option, which also nicely sidesteps forcing QWERTY.
  (kernel-arguments (list "console=ttyS0,115200"))
  (file-systems (cons* (file-system
                         (device (uuid "6b9c10fa-ca12-4008-a0cd-b08ce3b4d223"))
                         (mount-point "/")
                         (type "btrfs"))
                       (file-system
                         (device (uuid "acbca5c3-27e2-47b4-85ed-9779895986a5"))
                         (mount-point "/boot")
                         (type "ext4"))
                      %base-file-systems))

  ;; This is where user accounts are specified.  The "root"
  ;; account is implicit, and is initially created with the
  ;; empty password.
  (users (cons (user-account
                (name "alice")
                (comment "Bob's sister")
                (group "users")

                ;; Adding the account to the "wheel" group
                ;; makes it a sudoer.  Adding it to "audio"
                ;; and "video" allows the user to play sound
                ;; and access the webcam.
                (supplementary-groups '("wheel"
                                        "audio" "video")))
               %base-user-accounts))

  ;; Globally-installed packages.
  (packages (cons screen %base-packages))

  ;; Add services to the baseline: a DHCP client and an SSH
  ;; server.  You may wish to add an NTP service here.
  (services (append (list (service static-networking-service-type
                                   (list (static-networking
                                           (addresses
                                             (list (network-address
                                                     (device "eth0")
                                                     (value "2a01:4f8:1c1a:81ea::1/64"))))
                                           (routes
                                             (list (network-route
                                                     (device "eth0")
                                                     (destination "default")
                                                     (gateway "fe80::1"))))
                                           (name-servers '("2620:fe::9")))))
                          (service openssh-service-type
                                   (openssh-configuration
                                     (openssh openssh-sans-x)
                                     (permit-root-login #t)
                                     (port-number 22))))
                    %base-services)))

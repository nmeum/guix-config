(use-modules (gnu)
             (mycfg os)
             (mycfg transformations)
             (nmeum packages misc))

(define %os
  (operating-system
    (inherit
      (base-os
        "scandium"
        (bootloader-configuration
          (bootloader grub-bootloader)
          (targets '("/dev/sda")))
        (file-system
          (device (uuid "89d401cf-5ea4-4550-a4b8-7b56edc1323e"))
          (mount-point "/boot")
          (type "ext4"))
        (file-system
          (device (uuid "77309400-49ea-4ff6-9da7-38e7887e843e"))
          (mount-point "/")
          (type "btrfs"))))

    (initrd-modules (append (list "virtio_scsi")
                            %base-initrd-modules))

    (users (cons* (user-account
                    (name "soeren")
                    (comment "Sören Tempel")
                    (group "users")
                    (shell (file-append loksh-8pit "/bin/ksh"))
                    (home-directory "/home/soeren")
                    (supplementary-groups '("wheel" "netdev")))
                  %base-user-accounts))))

((compose
   (add-services
     (list (service static-networking-service-type
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
                            (name-servers '("2620:fe::9"))))))))
 %os)

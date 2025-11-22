(use-modules (gnu)
             (gnu services web)

             (mycfg os)
             (mycfg transformations))

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

    (users (cons* (base-account '("wheel"))
                  %base-user-accounts))))

((compose
   (btrfs-subvolumes (uuid "77309400-49ea-4ff6-9da7-38e7887e843e"))
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
                            (name-servers '("127.0.0.1")))))

           (simple-service 'guix-mycfg guix-service-type
                           (guix-extension
                             (authorized-keys
                               (list %mycfg-signing-key))))

           (service nginx-service-type
                    (nginx-configuration
                      (server-blocks
                        (list (nginx-server-configuration
                                (server-name '("scandium.8pit.net"))
                                (root "/srv/http/scandium.8pit.net")))))))))
 %os)

(use-modules (gnu)
             (gnu services certbot)
             (gnu services mail)
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

           ;; TODO: create /srv from a service startup script.
           (service nginx-service-type
                    (nginx-configuration
                      (server-blocks
                        (list
                          (nginx-server-configuration
                            (listen '("[::]:443 ssl"))
                            (server-name '("scandium.8pit.net"))
                            (root "/srv/http/scandium.8pit.net")
                            (ssl-certificate "/etc/letsencrypt/live/scandium.8pit.net/fullchain.pem")
                            (ssl-certificate-key "/etc/letsencrypt/live/scandium.8pit.net/privkey.pem"))
                          (nginx-server-configuration
                            (listen '("[::]:443 ssl"))
                            (server-name '("cal.8pit.net"))
                            (ssl-certificate "/etc/letsencrypt/live/cal.8pit.net/fullchain.pem")
                            (ssl-certificate-key "/etc/letsencrypt/live/cal.8pit.net/privkey.pem")
                            (locations
                              (list
                                (nginx-location-configuration
                                  (uri "/")
                                  (body '("proxy_pass http://127.0.0.1:8080;"))))))))))

           ;; TODO: deploy account key from Guix repo via guix-sops.
           (service certbot-service-type
                    (certbot-configuration
                      (email "postmaster@8pit.net")
                      (rsa-key-size 4096)
                      (webroot "/srv/acme")
                      (certificates
                        (list
                          (certificate-configuration
                            (domains '("cal.8pit.net" "scandium.8pit.net")))))))

           (service radicale-service-type
                    (radicale-configuration
                      (web-interface? #t)
                      (auth
                        (radicale-auth-configuration
                          (type 'htpasswd)
                          (htpasswd-encryption 'bcrypt)
                          ;; TODO: deploy users file from Guix
                          (htpasswd-filename "/etc/radicale/users")
                          (delay 3)))
                      (server
                        (radicale-server-configuration
                          (hosts '("127.0.0.1:8080"))
                          (ssl? #f))))))))
   %os)

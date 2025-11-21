(use-modules (guix gexp)
             (gnu home)
             (gnu home services)
             (gnu home services desktop)
             (gnu home services shells)
             (gnu home services shepherd)
             (gnu home services sound)
             (gnu packages)
             (gnu packages bash)
             (gnu packages linux)
             (gnu packages xdisorg)
             (gnu services)
             (gnu system shadow)

             (mycfg home)
             (mycfg home packages)

             (nmeum packages desktop)
             (nmeum packages misc))

(define my-packages
  (append
    packages-desktop
    packages-email
    packages-font
    packages-networking
    packages-multimedia
    packages-programming
    packages-security
    packages-tools
    packages-tools-extra))

(home-environment
  (inherit (base-home
             (specifications->packages my-packages)))

  (services
    (append
      (list
        ;; Needed to start services which use WAYLAND_DISPLAY.
        ;; See: https://issues.guix.gnu.org/76619
        (service home-shepherd-service-type
                 (home-shepherd-configuration
                   (auto-start? #f)))

        (simple-service 'wlsunset
          home-shepherd-service-type
          (list (shepherd-service
                  (provision '(wlsunset))
                  (start
                    #~(make-forkexec-constructor
                        (list
                          (string-append #$wlsunset "/bin/wlsunset")
                          "-l" "52.3" "-L" "11.1")))
                  (stop #~(make-kill-destructor)))))

        (simple-service 'dam
          home-shepherd-service-type
          (list (shepherd-service
                  (provision '(dam))
                  (start
                    ;; TODO: Supervise status text and status bar service separately.
                    #~(make-forkexec-constructor
                        (list (string-append #$bash-minimal "/bin/sh") "-c"
                              (format #f
                                      "~a | ~a ~a"
                                      (string-append #$ustatus "/bin/ustatus")
                                      (string-append #$dam "/bin/dam")
                                      (string-join
                                        '("-f Terminus:size=12"
                                          "-nb '#282828'"
                                          "-nf '#b8b8b8'"
                                          "-sb '#7cafc2'"
                                          "-sf '#181818'") " ")))))
                  (stop #~(make-kill-destructor)))))

        (service home-dbus-service-type)
        (service home-pipewire-service-type
                 (home-pipewire-configuration
                   (enable-pulseaudio? #t))))
      %base-home-services)))

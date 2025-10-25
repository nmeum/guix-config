;; This Guix Home configuration is intentionally very simple and
;; essentially limited to the installation of packages and the
;; configuration of user services. Configuration files are managed
;; without Guix Home.

(define-module (guix-home-config)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services sound)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (nmeum packages desktop)
  #:use-module (nmeum packages misc))

(define packages-desktop
  '("adwaita-icon-theme"
    "alacritty"
    "bemenu"
    "creek"
    "firefox"
    "river"
    "waylock"
    "wl-clipboard"
    "wlr-randr"
    "wlsunset"))

(define packages-documents
  '("mandoc"))

(define packages-font
  '("fontconfig"
    "font-terminus-patched"
    "font-terminus-patched:otb"
    "font-google-noto"
    "font-google-noto-emoji"
    "font-dejavu"))

(define packages-networking
  '("bind:utils"
    "iproute2"
    "mosh"
    "mtr"
    "whois"))

(define packages-multimedia
  '("ncmpc"
    "mpv"
    "pipewire"
    "wireplumber"
    "yt-dlp"))

(define packages-programming
  '("curl"
    "clang"
    "gcc-toolchain"
    "git"
    "make"
    "neovim"
    "python"
    "universal-ctags"))

(define packages-tools
  '("chimerautils"
    "cryptsetup"
    "file"
    "fzf"
    "gnupg"
    "htop"
    "pinentry-gtk2"
    "pwgen"
    "ripgrep"
    "strace"
    "tmux"
    "tpm"
    "tree"))

(define my-packages
  (append
    packages-desktop
    packages-documents
    packages-font
    packages-networking
    packages-multimedia
    packages-programming
    packages-tools))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (my-essential-services he)
  ;; I don't want Guix home to overwrite my ~/.profile.
  ;; Therefore, I remove the services responsible for that.
  (filter
    (lambda (s)
      ;; The home-shell-profile creates the ~/.profile file.
      (not (eqv? (service-type-name (service-kind s)) 'home-shell-profile)))
    (home-environment-essential-services he)))

(define home-config
  (let ((home-environment-base
          ;; Need to set packages here, otherwise the essential services
          ;; operate on an empty package set and no package is installed.
          (home-environment
            (packages (specifications->packages my-packages)))))
    (home-environment
      ;; Awful hack to change the default essential services.
      (inherit home-environment-base)
      (essential-services
        (my-essential-services home-environment-base))

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
          %base-home-services)))))

home-config

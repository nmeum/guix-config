;; This Guix Home configuration is intentionally very simple and
;; essentially limited to the installation of packages and the
;; configuration of user services. Configuration files are managed
;; without Guix Home.

(define-module (guix-home-config)
  #:use-module (guix records)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services sound)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu system shadow))

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
    "file"
    "fzf"
    "htop"
    "ripgrep"
    "strace"
    "tmux"
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
            (service home-dbus-service-type)
            (service home-pipewire-service-type
                     (home-pipewire-configuration
                       (enable-pulseaudio? #t))))
          %base-home-services)))))

home-config

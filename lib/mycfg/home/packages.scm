(define-module (mycfg home packages))

;; TODO:
;;  • archive-email
;;  • archive-logs
;;  • edward
;;  • fractal + gnome-keyring
;;  • khal
;;  • neo-layout (currently arrow keys don't work in framebuffer)
;;  • newer version of man-pages-posix
;;  • signal-desktop
;;  • tuba + gnome-keyring
;;  • vdirsyncer / pimsync
;;  • zk

(define-public packages-desktop
  '("adwaita-icon-theme"
    "alacritty"
    "bemenu"
    "bemenu-emoji"
    "dbus"
    "firefox"
    "libxcursor" ; for XCURSOR_PATH
    "mupdf" ; TODO: try zathura
    "river"
    "screen-message"
    "waylock"
    "wl-clipboard"
    "wlr-randr"
    "wlsunset"))

(define-public packages-email
  '("isync"
    "lynx" ; for viewing HTML mails
    "mblaze"
    "mblaze-ui"
    "msmtp"))

(define-public packages-font
  '("fontconfig"
    "font-terminus-patched"
    "font-terminus-patched:otb"
    "font-google-noto"
    "font-google-noto-emoji"
    "font-dejavu"))

(define-public packages-networking
  '("bind:utils"
    "curl"
    "iproute2"
    "mosh"
    "mtr"
    "rsync"
    "wget"
    "whois"))

(define-public packages-multimedia
  '("ncmpc"
    "ffmpeg"
    "imv"
    "mpv"
    "perl-image-exiftool"
    "pipewire"
    "qrencode"
    "snapcast"
    "wireplumber"
    "yt-dlp"))

(define-public packages-programming
  '("binutils"
    "ed"
    "gdb"
    "git"
    "git-shuffle"
    "guile"
    "guile-readline"
    "make"
    "neovim"
    "python"
    "universal-ctags"))

(define-public packages-security
  '("cryptsetup"
    "gnupg"
    "pinentry-gtk2"
    "pwgen"
    "tpm"))

(define-public packages-tools
  '("chimerautils"
    "entr"
    "file"
    "fzf"
    "htop"
    "less"
    "mandoc"
    "ripgrep"
    "rlwrap"
    "strace"
    "tmux"
    "tree"))

;; non-desktop packages that I don't need on every shell host.
(define-public packages-tools-extra
  '("discount"
    "man-pages"
    "man-pages-posix"
    "nmap"
    "poppler"
    "restic"
    "sshfs"
    "unison"))

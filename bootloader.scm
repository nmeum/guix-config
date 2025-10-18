(use-modules (gnu)
             (gnu bootloader)
             (gnu bootloader grub)
             (guix gexp)
             (guix import utils))

;; wraps an existing Grub 'bootloader-installer' in a procedure which copies
;; all files referenced in Grub's configuration file to the install directory.
(define (grub-copy installer)
  #~(lambda (bootloader device mount-point)
      (use-modules ((srfi srfi-1) #:select (fold lset-adjoin lset-difference))
                   ((guix import utils) #:select (read-lines))
                   ((ice-9 ftw) #:select (scandir))
                   (ice-9 regex))

      ;; regex for finding a path to the Store in the Grub configuration file.
      ;; Obviously a heurstic, ideally we would get this information from Grub.
      (define store-regexp (make-regexp "/gnu/store/[A-Za-z0-9@/._-]+"))

      ;; regex for finding the linux command in a Grub configuration file.
      ;; See https://www.gnu.org/software/grub/manual/grub/grub.html#linux
      (define linux-regexp (make-regexp "^[[:space:]]*linux[[:space:]]"))

      ;; expand a list of paths, which potentially includes directories to
      ;; a pure list of files (without including any paths to directories).
      (define (expand-paths paths)
        (fold
          (lambda (path acc)
            (append acc (find-files path)))
          '() paths))

      (define (required-paths lines)
        (fold
          (lambda (line acc)
            (let ((paths (map match:substring (list-matches store-regexp line))))
              (if (null? paths)
                acc
                (if (regexp-exec linux-regexp line) ; ignore paths in kernel cmline
                  (lset-adjoin equal? acc (car paths))
                  (apply lset-adjoin (cons* equal? acc paths))))))
          '() lines))

      (define (existing-paths store)
        (map
          (lambda (path)
            ;; remove the /boot prefix from every file path.
            (string-copy path (string-length "/boot")))
          (find-files store)))

      (let* ((install-dir (canonicalize-path (string-append mount-point "/boot")))
             (grub-cfg (string-append install-dir "/grub/grub.cfg"))
             (grub-lines (call-with-input-file grub-cfg read-lines))
             (required (required-paths grub-lines))
             (existing (existing-paths (string-append install-dir "/gnu/store"))))
        (format #t "removing: ~a ~%" (lset-difference equal? existing (expand-paths required)))
        (for-each
          (lambda (store-file)
            (let ((dest-file (string-append install-dir store-file)))
              (mkdir-p (dirname dest-file))
              (copy-recursively store-file dest-file)))
          required))

      ;; Invoke the 'bootloader-installer' that we are wrapping.
      (#$installer bootloader device mount-point)))

(define grub-copy-bootloader
  (bootloader
    (inherit grub-bootloader)
    (installer (grub-copy (bootloader-installer grub-bootloader)))))

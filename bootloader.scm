(use-modules (gnu)
             (gnu bootloader)
             (gnu bootloader grub)
             (guix gexp)
             (guix import utils))

;; wraps an existing Grub 'bootloader-installer' in a procedure which copies
;; all files referenced in Grub's configuration file to the install directory.
;;
;; TODO: make use of the new in-vicinity procedure (unreleased in GNU Guile).
;; See https://cgit.git.savannah.gnu.org/cgit/guile.git/commit/?id=755f703dcb3110e1920e42078edc6d9c88cc8b28
(define (grub-copy installer)
  #~(lambda (bootloader device mount-point)
      (use-modules ((srfi srfi-1) #:select (fold lset-adjoin lset-difference))
                   ((guix import utils) #:select (read-lines))
                   ((guix store) #:select (direct-store-path))
                   (ice-9 regex))

      ;; regex for finding a path to the Store in the Grub configuration file.
      ;; Obviously a heurstic, ideally we would get this information from Grub.
      (define store-regexp (make-regexp "/gnu/store/[A-Za-z0-9@/._-]+"))

      ;; regex for finding the linux command in a Grub configuration file.
      ;; See https://www.gnu.org/software/grub/manual/grub/grub.html#linux
      (define linux-regexp (make-regexp "^[[:space:]]*linux[[:space:]]"))

      ;; Takes a list of /gnu/store paths and returns a list of unique directory
      ;; entries in the /gnu/store directory (usually: hash + package + version).
      (define (store-entries paths)
        (fold
          (lambda (path acc)
            (lset-adjoin
              equal?
              acc
              (substring (direct-store-path path) (string-length "/gnu/store"))))
          '() paths))

      (define (required-paths lines)
        (fold
          (lambda (line acc)
            (let ((paths (map match:substring (list-matches store-regexp line))))
              (if (null? paths)
                acc
                (if (regexp-exec linux-regexp line) ; ignore kernel cmdline paths
                  (lset-adjoin equal? acc (car paths))
                  (apply lset-adjoin (cons* equal? acc paths))))))
          '() lines))

      (define (existing-paths store)
        (map
          (lambda (path)
            (substring path (string-length "/boot")))
          (find-files store)))

      (let* ((install-dir (canonicalize-path (string-append mount-point "/boot")))
             (grub-cfg (string-append install-dir "/grub/grub.cfg"))
             (grub-lines (call-with-input-file grub-cfg read-lines))
             (required (required-paths grub-lines))
             (existing (existing-paths (string-append install-dir "/gnu/store"))))
        (for-each ; remove leftovers from old generations
          (lambda (store-entry)
            (delete-file-recursively
              (string-append install-dir "/gnu/store/" store-entry)))
          (lset-difference equal? (store-entries existing) (store-entries required)))
        (for-each ; copy required files
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

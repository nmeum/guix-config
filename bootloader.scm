(use-modules (gnu)
             (gnu bootloader)
             (gnu bootloader grub)
             (guix gexp)
             (guix import utils))

;; wraps an existing Grub 'bootloader-installer' in a procedure which copies
;; all files referenced in Grub's configuration file to the install directory.
(define (grub-copy installer)
  #~(lambda (bootloader device mount-point)
      ;; TODO: Make /boot/gnu/store a GC root and ensure it is garbage-collected.
      (use-modules (srfi srfi-1)
                   (guix import utils)
                   (ice-9 regex))

      ;; regex for finding a path to the Store in the Grub configuration file.
      ;; Obviously a heurstic, ideally we would get this information from Grub.
      (define store-regexp (make-regexp "/gnu/store/[A-Za-z0-9@/._-]+"))

      ;; regex for finding the linux command in a Grub configuration file.
      ;; See https://www.gnu.org/software/grub/manual/grub/grub.html#linux
      (define linux-regexp (make-regexp "^[[:space:]]+linux[[:space:]]"))

      (define (filter-paths lines)
        (fold
          (lambda (line acc)
            (let ((paths (map
               match:substring
               (list-matches store-regexp line))))
        (if (null? paths)
          acc
          (append
            (if (regexp-exec linux-regexp line) ; ignore paths in kernel cmline
              (list (car paths))
              paths)
            acc)))) '() lines))

      (let* ((install-dir (string-append mount-point "/boot"))
             (grub-cfg (string-append install-dir "/grub/grub.cfg"))
             (grub-lines (call-with-input-file grub-cfg read-lines)))
        (for-each
          (lambda (store-file)
            (let ((dest-file (string-append install-dir store-file)))
              (mkdir-p (dirname dest-file))
              (copy-recursively store-file dest-file)))
          (delete-duplicates (filter-paths grub-lines))))

      ;; Invoke the 'bootloader-installer' that we are wrapping.
      (#$installer bootloader device mount-point)))

(define grub-copy-bootloader
  (bootloader
    (inherit grub-bootloader)
    (installer (grub-copy (bootloader-installer grub-bootloader)))))

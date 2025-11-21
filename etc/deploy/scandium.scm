(use-modules (gnu machine)
             (gnu machine ssh))

(list
 (machine
   (operating-system (load "../scandium.scm"))
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
      (host-name "scandium")
      (system "x86_64-linux")
      (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHsmeGdHXWfDSRULIcqILmfwfo2hexp/VknwbxyRdS2q")))))

(list (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (commit
          "a345ef84fbdf3b2491acb2c2b6665a4eb97bd4aa")
        (introduction
          (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
        (name 'nmeum)
        (url "https://github.com/nmeum/guix-channel.git")
        (branch "master")
        (commit
          "79de59b1910ee3c3418344224373024c287d84cf")
        (introduction
          (make-channel-introduction
            "808a00792c114c5c1662e8b1a51b90a2d23f313a"
            (openpgp-fingerprint
              "514E 833A 8861 1207 4F98  F68A E447 3B6A 9C05 755D"))))
      (channel
        (name 'guix)
        (url "https://codeberg.org/guix/guix.git")
        (branch "master")
        (commit
          "f1d240feb255a1832dde872b4d1a82abb9572f92")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))

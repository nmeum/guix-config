* mcron job to invoke mandoc's `makewhatis` instead of the man-db equivalent
* Configure btrfs subvolumes
	* Add set subvolume-specific options (e.g. force compression of `/var/log`)
	* Also configure an mcron job which creates snapshots of `/home`
* Support an encrypted root with LUKS2
	* Problem: Grub doesn't support LUKS2 and is generally slow
		* Hence, requires decrypting the volume from an initramfs
		* For example, by porting [booster] to Guix
	* Presupposes an unencrypted `/boot` partition
		* Problem: Guix currently loads the Kernel from `/gnu/store`
		* Need a derivation which copies it from the store to `/boot`

[booster]: https://github.com/anatol/booster

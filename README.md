# guix-config

Contains [Guix] operating system and home configurations.

## Usage

Reconfigure the system using:

	$ sudo guix time-machine -C channels.scm -- system reconfigure etc/config.scm

To setup a home environment run:

	$ guix time-machine -C channels.scm -- home reconfigure home/environment.scm

[Guix]: https://guix.gnu.org

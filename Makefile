.PHONY: ps erl all test clean distclean

.DEFAULT_GOAL := ps

all: test

ps:
	@spago build

clean:
	rm -rf output

distclean: clean
	chmod -R 700 nix
	rm -rf .spago nix

test:
	@spago -x test.dhall test


nix: nix/.nixcomplete
	./nix-user-chroot nix bash --login -c 'make nix-shell'

nix/.nixcomplete:  nix-user-chroot
	if [ ! -d nix ]; then mkdir -p nix; ./nix-user-chroot nix bash -c 'curl -L https://nixos.org/nix/install | sh'; touch nix/.nixcomplete; fi

nix-user-chroot:
	curl --output nix-user-chroot -L https://github.com/nix-community/nix-user-chroot/releases/download/1.2.2/nix-user-chroot-bin-1.2.2-x86_64-unknown-linux-musl
	sha256sum --quiet -c nix-user-chroot.sha256
	chmod +x nix-user-chroot

nix-shell:
	nix-shell --run 'echo "Nix build completed"'  shell.nix

nix-test: nix-user-chroot
	./nix-user-chroot nix .github/runtests.sh

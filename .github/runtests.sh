#!/usr/bin/env bash

. /nix/var/nix/profiles/per-user/${USER}/profile/etc/profile.d/nix.sh

if [ ! -d ~/.nix-profile ]; then
  ln -s /nix/var/nix/profiles/per-user/${USER}/profile ~/.nix-profile
fi

if [ -z "${NIX_CONF_DIR}" ]; then
  export NIX_CONF_DIR=/nix/etc/nix
fi

if [ -z "${NIX_PROFILES}" ]; then
  export NIX_PROFILES='/nix/var/nix/profiles/default /home/${USER}/.nix-profile'
fi

if [ -z "${NIX_SSL_CERT_FILE}"  ]; then
  export NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt
fi

nix-shell --run 'spago -x test.dhall test'

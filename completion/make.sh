#!/bin/bash

pacman_bash_completion='/usr/share/bash-completion/completions/pacman'
pacman_zsh_completion='/usr/share/zsh/site-functions/_pacman'

function error() {
  local code="$?"
  rm "$1"
  exit "$code"
}

function apply-patch() {
  patch -sNp1 -r - --no-backup-if-mismatch -i "$1"
}

function delete-shell-fn() {
  perl -0777 -pe 's/\n'"$1"'\(\) *\{\n([^}].*\n)*\}\n*/\n\n/g;s/\n{3,}/\n\n/g'
}

function delete-shell-array() {
  perl -0777 -pe 's/\n'"$1"'=\(\n([^)].*\n)*\)\n?//g'
}

[ "$1" = 'bash' ] && {
  cat "$pacman_bash_completion" |
  delete-shell-fn '_pacman_keyids' |
  delete-shell-fn '_pacman_key' |
  delete-shell-fn '_makepkg' |
  sed 's/^_pacman() {$/_pakku() {/' \
  > 'bash' ||
  error 'bash'

  apply-patch 'bash.patch' ||
  error 'bash'

  exit 0
}

[ "$1" = 'zsh' ] && {
  cat "$pacman_zsh_completion" |
  delete-shell-array '_key_shortopts' |
  delete-shell-array '_key_longopts' |
  delete-shell-array '_pacman_key_options' |
  delete-shell-fn '_pacman_key' |
  delete-shell-fn '_keys' |
  delete-shell-array '_makepkg_shortopts' |
  delete-shell-array '_makepkg_longopts' |
  delete-shell-fn '_makepkg_action_none' |
  delete-shell-fn '_makepkg' |
  sed 's/_pacman/_pakku/g' \
  > 'zsh' ||
  error 'zsh'

  apply-patch 'zsh.patch' ||
  error 'zsh'

  exit 0
}

exit 1

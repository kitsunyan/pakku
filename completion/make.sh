#!/bin/bash

pacman_bash_completion='/usr/share/bash-completion/completions/pacman'

function delete-shell-fn() {
  perl -0777 -pe 's/\n'"$1"'\(\) *\{\n([^}].*\n)*\}\n*/\n\n/g;s/\n{3,}/\n\n/g'
}

cat "$pacman_bash_completion" |
delete-shell-fn '_pacman_keyids' |
delete-shell-fn '_pacman_key' |
delete-shell-fn '_makepkg' |
sed 's/^_pacman() {$/_pakku() {/' \
> 'bash' || {
  rm 'bash'
  exit 1
}
patch -sNp1 -r - --no-backup-if-mismatch -i 'bash.patch' || {
  rm 'bash'
  exit 1
}

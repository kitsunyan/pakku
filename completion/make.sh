#!/bin/bash

pacman_bash_completion='/usr/share/bash-completion/completions/pacman'

(IFS=; while read -r line; do
  [ "${line:0:12}" = 'make_import ' ] && {
    grep -Poz '(?<=\n)'"${line:12}"'\(\) \{\n(.*\n)*?\}' "$pacman_bash_completion" |
    xargs -0
  } || {
    echo "$line"
  }
done) < 'bash.in' > 'bash'

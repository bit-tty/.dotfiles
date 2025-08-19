#!/bin/bash
sudo bash -c 'pgrep --full --euid=0 "pacman" | while read -r -- pid; do
    cat "/proc/${pid}/cmdline" | awk '"'"'{ gsub(/\x00/, " "); print }'"'"'
    echo
done'

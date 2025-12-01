#!/bin/bash
echo "--------------------------------------Updating system---------------------------------"
echo "--------------------------------------Pacman updates----------------------------------"
sudo pacman -Syyu
echo "--------------------------------------AUR updates-------------------------------------"
yay -Syu

echo "--------------------------------------Clearing pacman cache---------------------------------------"
sudo pacman_cache_space_used="$(du -sh /var/cache/pacman/pkg/)"
sudo paccache -r 
echo "--------------------------------------Space saved: $pacman_cache_space_used-----------------------" 

echo "--------------------------------------Removing orphan packages------------------------------------"
yay -Qdtq | yay -Rns -

echo "--------------------------------------Clearing ~/.cache-------------------------------------------"
home_cache_used="$(du -sh ~/.cache)"
sudo rm -rf ~/.cache/
echo "--------------------------------------Spaced saved: $home_cache_used------------------------------"

echo "--------------------------------------Clearing system logs----------------------------------------"
sudo journalctl --vacuum-time=7d

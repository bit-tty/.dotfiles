#!/bin/bash

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Log file
LOG_FILE="$HOME/maintenance-$(date +%Y%m%d-%H%M%S).log"

# Function to prompt user
prompt_user() {
    local message="$1"
    local response
    echo -e "${YELLOW}${message} [y/N]:${NC} "
    read -r response
    case "$response" in
        [yY][eE][sS]|[yY]) 
            return 0
            ;;
        *)
            echo -e "${BLUE}Skipped.${NC}"
            return 1
            ;;
    esac
}

# Function to calculate directory size in KB
get_size_kb() {
    du -sk "$1" 2>/dev/null | awk '{print $1}'
}

# Function to format size
format_size() {
    local size_kb=$1
    if [ "$size_kb" -ge 1048576 ]; then
        echo "$(awk "BEGIN {printf \"%.2f\", $size_kb/1048576}") GB"
    elif [ "$size_kb" -ge 1024 ]; then
        echo "$(awk "BEGIN {printf \"%.2f\", $size_kb/1024}") MB"
    else
        echo "${size_kb} KB"
    fi
}

echo -e "${GREEN}======================================${NC}"
echo -e "${GREEN}   System Maintenance Script${NC}"
echo -e "${GREEN}   $(date)${NC}"
echo -e "${GREEN}======================================${NC}"
echo ""

# Start logging header
{
    echo "======================================"
    echo "   System Maintenance Script"
    echo "   $(date)"
    echo "======================================"
    echo ""
} >> "$LOG_FILE"

# 1. Pacman updates
echo -e "\n${BLUE}--------------------------------------${NC}"
echo -e "${BLUE}Pacman System Updates${NC}"
echo -e "${BLUE}--------------------------------------${NC}"
if prompt_user "Update system packages with pacman?"; then
    echo "Pacman System Updates - $(date)" >> "$LOG_FILE"
    sudo pacman -Syyu
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✓ Pacman update completed${NC}"
        echo "✓ Pacman update completed" >> "$LOG_FILE"
    else
        echo -e "${RED}✗ Pacman update failed${NC}"
        echo "✗ Pacman update failed" >> "$LOG_FILE"
    fi
fi

# 2. AUR updates
echo -e "\n${BLUE}--------------------------------------${NC}"
echo -e "${BLUE}AUR Updates${NC}"
echo -e "${BLUE}--------------------------------------${NC}"
if prompt_user "Update AUR packages with yay?"; then
    echo "AUR Updates - $(date)" >> "$LOG_FILE"
    yay -Syu
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✓ AUR update completed${NC}"
        echo "✓ AUR update completed" >> "$LOG_FILE"
    else
        echo -e "${RED}✗ AUR update failed${NC}"
        echo "✗ AUR update failed" >> "$LOG_FILE"
    fi
fi

# 3. Flatpak updates (if installed)
if command -v flatpak &> /dev/null; then
    echo -e "\n${BLUE}--------------------------------------${NC}"
    echo -e "${BLUE}Flatpak Updates${NC}"
    echo -e "${BLUE}--------------------------------------${NC}"
    if prompt_user "Update Flatpak packages?"; then
        echo "Flatpak Updates - $(date)" >> "$LOG_FILE"
        flatpak update -y
        if [ $? -eq 0 ]; then
            echo -e "${GREEN}✓ Flatpak update completed${NC}"
            echo "✓ Flatpak update completed" >> "$LOG_FILE"
        else
            echo -e "${RED}✗ Flatpak update failed${NC}"
            echo "✗ Flatpak update failed" >> "$LOG_FILE"
        fi
    fi
fi

# 4. Clear pacman cache
echo -e "\n${BLUE}--------------------------------------${NC}"
echo -e "${BLUE}Clearing Pacman Cache${NC}"
echo -e "${BLUE}--------------------------------------${NC}"
if prompt_user "Clear pacman cache (keeps last 3 versions)?"; then
    cache_before=$(get_size_kb "/var/cache/pacman/pkg/")
    echo "Cache size before: $(format_size $cache_before)"
    
    sudo paccache -r
    
    cache_after=$(get_size_kb "/var/cache/pacman/pkg/")
    cache_saved=$((cache_before - cache_after))
    echo -e "${GREEN}✓ Space freed: $(format_size $cache_saved)${NC}"
fi

# 5. Remove orphan packages
echo -e "\n${BLUE}--------------------------------------${NC}"
echo -e "${BLUE}Removing Orphan Packages${NC}"
echo -e "${BLUE}--------------------------------------${NC}"
orphans=$(yay -Qdtq)
if [ -n "$orphans" ]; then
    echo "Orphan packages found:"
    echo "$orphans"
    if prompt_user "Remove these orphan packages?"; then
        yay -Rns --noconfirm $(yay -Qdtq)
        if [ $? -eq 0 ]; then
            echo -e "${GREEN}✓ Orphan packages removed${NC}"
        else
            echo -e "${RED}✗ Failed to remove orphan packages${NC}"
        fi
    fi
else
    echo -e "${GREEN}No orphan packages found${NC}"
fi

# 6. Clear user cache
echo -e "\n${BLUE}--------------------------------------${NC}"
echo -e "${BLUE}Clearing User Cache${NC}"
echo -e "${BLUE}--------------------------------------${NC}"
if [ -d "$HOME/.cache" ]; then
    if prompt_user "Clear ~/.cache directory?"; then
        cache_before=$(get_size_kb "$HOME/.cache")
        echo "Cache size before: $(format_size $cache_before)"
        
        rm -rf "$HOME/.cache/"*
        
        cache_after=$(get_size_kb "$HOME/.cache")
        cache_saved=$((cache_before - cache_after))
        echo -e "${GREEN}✓ Space freed: $(format_size $cache_saved)${NC}"
    fi
else
    echo -e "${GREEN}~/.cache directory does not exist${NC}"
fi

# 7. Clear system logs
echo -e "\n${BLUE}--------------------------------------${NC}"
echo -e "${BLUE}Clearing System Logs${NC}"
echo -e "${BLUE}--------------------------------------${NC}"
if prompt_user "Clear system logs older than 7 days?"; then
    log_before=$(get_size_kb "/var/log/journal/")
    echo "Journal size before: $(format_size $log_before)"
    
    sudo journalctl --vacuum-time=7d
    
    log_after=$(get_size_kb "/var/log/journal/")
    log_saved=$((log_before - log_after))
    echo -e "${GREEN}✓ Space freed: $(format_size $log_saved)${NC}"
fi

# 8. Check for .pacnew files
echo -e "\n${BLUE}--------------------------------------${NC}"
echo -e "${BLUE}Checking for .pacnew Files${NC}"
echo -e "${BLUE}--------------------------------------${NC}"
if prompt_user "Check for .pacnew/.pacsave files?"; then
    pacnew_files=$(sudo find /etc -name "*.pacnew" -o -name "*.pacsave" 2>/dev/null)
    if [ -n "$pacnew_files" ]; then
        echo -e "${YELLOW}⚠ Configuration files need attention:${NC}"
        echo "$pacnew_files"
        echo -e "${YELLOW}Please merge these manually using pacdiff or similar tools${NC}"
    else
        echo -e "${GREEN}✓ No .pacnew or .pacsave files found${NC}"
    fi
fi

# 9. Check failed services
echo -e "\n${BLUE}--------------------------------------${NC}"
echo -e "${BLUE}Checking Failed Services${NC}"
echo -e "${BLUE}--------------------------------------${NC}"
if prompt_user "Check for failed systemd services?"; then
    failed_services=$(systemctl --failed --no-pager)
    if echo "$failed_services" | grep -q "0 loaded units listed"; then
        echo -e "${GREEN}✓ No failed services${NC}"
    else
        echo -e "${YELLOW}⚠ Failed services found:${NC}"
        systemctl --failed
    fi
fi

# 10. Update locate database
if command -v updatedb &> /dev/null; then
    echo -e "\n${BLUE}--------------------------------------${NC}"
    echo -e "${BLUE}Updating Locate Database${NC}"
    echo -e "${BLUE}--------------------------------------${NC}"
    if prompt_user "Update locate database?"; then
        sudo updatedb
        if [ $? -eq 0 ]; then
            echo -e "${GREEN}✓ Locate database updated${NC}"
        else
            echo -e "${RED}✗ Failed to update locate database${NC}"
        fi
    fi
fi

# Summary
echo -e "\n${GREEN}======================================${NC}"
echo -e "${GREEN}   Maintenance Complete!${NC}"
echo -e "${GREEN}======================================${NC}"
echo -e "Log saved to: ${LOG_FILE}"
echo ""

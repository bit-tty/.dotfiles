#!/bin/bash
# System Information Gatherer for nftables Configuration
# Run this script to collect information needed to customize your nftables config

echo "=== Network Interface Information ==="
echo "Available network interfaces:"
ip link show | grep -E '^[0-9]+:' | awk -F': ' '{print $2}' | grep -v lo

echo -e "\n=== Current IP Addresses ==="
ip addr show | grep -E 'inet [0-9]' | grep -v '127.0.0.1'

echo -e "\n=== Default Gateway Information ==="
echo "Default gateway:"
ip route show default

echo -e "\n=== Gateway MAC Address ==="
echo "Gateway ARP entry (if available):"
GATEWAY_IP=$(ip route show default | awk '/default/ {print $3}' | head -1)
if [ ! -z "$GATEWAY_IP" ]; then
    arp -n $GATEWAY_IP 2>/dev/null || echo "Gateway MAC not in ARP table (may need to ping gateway first)"
fi

echo -e "\n=== Network Configuration Summary ==="
echo "Primary interface (excluding loopback):"
PRIMARY_IF=$(ip route show default | awk '/default/ {print $5}' | head -1)
echo "Interface: $PRIMARY_IF"

if [ ! -z "$PRIMARY_IF" ]; then
    echo "IP Address:"
    ip addr show $PRIMARY_IF | grep 'inet ' | awk '{print $2}' | head -1
    
    echo "Network range (assuming /24):"
    ip addr show $PRIMARY_IF | grep 'inet ' | awk '{print $2}' | head -1 | sed 's/\.[0-9]*\/24/.0\/24/'
fi

echo -e "\n=== DNS Servers ==="
echo "Current DNS servers:"
grep nameserver /etc/resolv.conf | awk '{print $2}' | head -4

echo -e "\n=== Suggested Configuration Values ==="
echo "Based on your system, here are the suggested values:"
echo "NIC_DATA_NAME = \"$PRIMARY_IF\""
if [ ! -z "$PRIMARY_IF" ]; then
    LOCAL_IP=$(ip addr show $PRIMARY_IF | grep 'inet ' | awk '{print $2}' | cut -d'/' -f1 | head -1)
    echo "NIC_DATA_IP = \"$LOCAL_IP\""
    
    # Calculate network range
    NETWORK=$(echo $LOCAL_IP | cut -d'.' -f1-3).0/24
    echo "LOCAL_INETW = { $NETWORK }"
fi

echo "DHCP_SERVER = \"$GATEWAY_IP\""

echo -e "\nDNS_SERVERS = {"
grep nameserver /etc/resolv.conf | awk '{print $2}' | head -4 | sed 's/$/,/' | tr '\n' ' '
echo "}"

echo -e "\n=== Gateway MAC Address Collection ==="
echo "To get the gateway MAC address, run:"
echo "ping -c 1 $GATEWAY_IP && arp -n $GATEWAY_IP"

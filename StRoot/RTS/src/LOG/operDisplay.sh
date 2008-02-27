#!/bin/sh

/usr/bin/logger -i -t operDisplay -p user.err "Starting..."

for ((;1;)); do
/RTS/bin/LINUX/i686/operDisplay </dev/null 2>/dev/null >/dev/null

/usr/bin/logger -i -t operDisplay -p user.err "Died -- restarting"
sleep 5
done

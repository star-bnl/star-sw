ls -1 */*/*event.root | awk -F\/ '{printf("%20s%20s/%20s\n",$3,$1,$2);}' | sort

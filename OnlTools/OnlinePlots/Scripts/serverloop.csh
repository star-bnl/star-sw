#!/bin/tcsh

unset DISPLAY   
while 1 
	nohup evpServer -live -start -end -nogui >>& server.out
	/RTS/bin/LINUX/i686/rtsLog -p 8005 -d WARNING -c server.csh Restarting evpServer in 30 sec; Press 'live' in presenter gui
	sleep 30;
end


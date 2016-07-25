#!/bin/sh 
#
# tcptune	tune the ethernet interface(s) for optimal TCP
#
# chkconfig: 345 60 20
# description:  Sets TCP parameters for ethernet interface
#		Initial default is to deal with eth1 for gigabit
#	       
# probe: true
#
# Eric Myers <myers@umich.edu> - 3 October 2001
# @(#) $Revision: 1.1.1.1 $
######################################################################


# Default gig interface parameters:

GIGETH=eth1
MAXMTU=1500


# RedHat function library:
. /etc/rc.d/init.d/functions

# per-host custom configuration:
if [ -f /etc/sysconfig/tcptune ] ; then
	. /etc/sysconfig/tcptune
fi


###
# Kernel version info

MAJREV=`uname -r | awk -F. '{print $1}'`
MIDREV=`uname -r | awk -F. '{print $2}'`




case "$1" in
  start)
	echo -n "Tune TCP stack: "
	echo 1048576 > /proc/sys/net/core/rmem_max
	echo 1048576 > /proc/sys/net/core/wmem_max

	# This is per Dantong for 2.4 kernels

	if [ $MAJREV -eq 2 -a $MIDREV -ge 4 ]; then
###	  echo "This is 2.4 kernel (or better) "
  	  echo "4096 87380 4194304" > /proc/sys/net/ipv4/tcp_rmem 
	  echo "4096 65536 4194304" > /proc/sys/net/ipv4/tcp_wmem 
	fi

        # Jumbo frames MTU - can be very dependent on card

	ifconfig $GIGETH mtu $MAXMTU &&  success || failure

	echo 
	;;

  stop)
	echo -n  "Reset TCP stack to defaults: "
	echo 65536 > /proc/sys/net/core/rmem_max
	echo 65536 > /proc/sys/net/core/wmem_max
	if [ $MAJREV -eq 2 -a $MIDREV -ge 4 ]; then
##	  echo "This is 2.4 kernel (or better) "
  	  echo "4096 87380 4194304" > /proc/sys/net/ipv4/tcp_rmem 
	  echo "4096 65536 4194304" > /proc/sys/net/ipv4/tcp_wmem 
	fi

	ifconfig $GIGETH mtu 1500   && success || failure

	echo 
	;;

  status)
	echo "TCP parameters for $GIGETH: "
	echo -n " TCP read buffer max: 	" 
	cat /proc/sys/net/core/rmem_max
	echo -n " TCP read write max: 	" 
	cat /proc/sys/net/core/wmem_max

	echo -n " IPv4 read buffer: 	"
	cat /proc/sys/net/ipv4/tcp_rmem 
	echo -n " IPv4 write buffer: 	"
	cat /proc/sys/net/ipv4/tcp_wmem 
	echo " "

	ifconfig $GIGETH

	;;

  restart)
	$0 stop
	$0 start
	;;

  probe)
	ifconfig $GIGETH
	;;

  *)
	echo "Usage: nfslock {start|stop|status|restart}"
	exit 1
esac

exit 0

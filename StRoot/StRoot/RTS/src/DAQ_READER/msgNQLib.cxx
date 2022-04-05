#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <poll.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#if defined(__linux__) || defined(__APPLE__)

#endif

#include <rtsLog.h>


#include "msgNQLib.h"



// index is the desciptor - NOT the task
//static struct msgNQStruct desc[MSG_NQ_MAXQUEUES] ;


/*
	Opens the pipe associated with a task and returns a standard file
	desciptor if successfull or -1 if not.

	If the pipe didn't exist - it makes it first via mknod.

	If the second argument ("lock") is true it additionally places
	an advisory lock on the pipe itself. This should only be done if the
	caller will be the listener of the pipe!

	If another task has the same pipe open msgQCreate will return error.

*/
int msgNQCreate(char *host, int port, int msglen)
{
	struct sockaddr_in me ;
	int size, dsc ;
	struct hostent *hostent ;
	int optval ;
	int ret ;

	size = sizeof(struct sockaddr_in) ;
	memset((char *)&me,0,size) ;


	me.sin_family = AF_INET ;
	me.sin_port = htons(port) ;


	hostent = gethostbyname(host) ;
	if(hostent == NULL) {
	  LOG(CRIT,"Unknown host %s (%s)",host,strerror(errno),0,0,0) ;
		return -1 ;
	}


	memcpy(&me.sin_addr.s_addr,*(hostent->h_addr_list),sizeof(me.sin_addr.s_addr)) ;

	errno = 0 ;
	dsc = socket(AF_INET, SOCK_STREAM, 0) ;
	if(dsc < 0) {
		LOG(CRIT,"socket() failed [%s]",strerror(errno),0,0,0,0) ;
		return -1 ;
	}

	optval = 1 ;
	setsockopt(dsc,SOL_SOCKET, SO_KEEPALIVE, (char *)&optval, sizeof(optval)) ;
	setsockopt(dsc,SOL_SOCKET, SO_REUSEADDR, (char *)&optval, sizeof(optval)) ;

	errno = 0 ;
	if(connect(dsc,(struct sockaddr *)&me,size) < 0) {
		LOG(CRIT,"connect() to %s, port %d failed [%s]",host,port,strerror(errno),0,0) ;
		close(dsc) ;
		return -1 ;
	}


#if defined(__linux__) || defined(__APPLE__)
	LOG(DBG,"Before fcntl") ;
	errno = 0 ;
	ret = fcntl(dsc,F_SETFL, O_NONBLOCK) ;
	if(ret < 0) {
		LOG(CRIT,"fcntl() failed [%s]",strerror(errno),0,0,0,0) ;
		close(dsc) ;
		return -1 ;
	}


#else
	LOG(DBG,"Before fcntl...",0,0,0,0,0) ;
	int modes ;
	errno = 0 ;
	ret = fcntl(dsc,F_GETFL,&modes) ;
	if(ret < 0) {
		LOG(CRIT,"fcntl() failed [%s]",strerror(errno),0,0,0,0) ;
		close(dsc) ;
		return -1 ;
	}

	LOG(DBG,"Before fcntl 0x%X",modes,0,0,0,0) ;
	errno = 0 ;
	ret = fcntl(dsc,F_SETFL, modes|O_NONBLOCK) ;
	if(ret < 0) {
		LOG(CRIT,"fcntl() failed [%s]",strerror(errno),0,0,0,0) ;
		close(dsc) ;
		return -1 ;
	}
#endif
		
	return dsc ;
}

/*
	Send a message over the desc with a timeout and a locking
	check.

	Returns:	positive integer (number of bytes written)
			negative integer 
			-1	error 
			-2	timeout (MSG_Q_TIMEOUT)

*/

int msgNQSend(int dsc, char *what, int size, int timeout, int prio)
{
	int ret ;
	struct pollfd pollstruct ;


	if(timeout == WAIT_FOREVER) timeout = 100000000 ;

	pollstruct.fd = dsc ;
	pollstruct.events = POLLOUT ;

	// override size
	size = 120 ;


	for(;;) {
		if(msgNQCheck(dsc)==0) {
			LOG(ERR,"Task %d not there...",dsc,0,0,0,0) ;
			return MSG_Q_NOTASK ;
		}

		errno = 0 ;
		ret = write(dsc,what,size) ;
		if(ret < 0) {
			if(errno == EAGAIN) {
				if(timeout) {

					errno = 0 ;
					ret = poll(&pollstruct,1,1000) ;	// 1 sec

					if((timeout % 10) == 0) {
						LOG(DBG,"Unable to send to task %d in 10 seconds...",dsc,0,0,0,0) ;
					}

					timeout-- ;
					
					if(ret >= 0) {	// timeout or OK - retry
						continue ;
					}

					if(errno == EINTR) {	// a signal was caught
						continue ;	// retry ;
					}
					LOG(ERR,"poll() returned (%s)",strerror(errno),0,0,0,0) ;
					return MSG_Q_ERROR ;

				}
				else break ;
			}
			else {
				LOG(ERR,"Can't write to task (%s)",strerror(errno),0,0,0,0) ;
				return MSG_Q_ERROR ;
			}
		}
		else break ;	// written something - bytes in "ret"
	} ;

	if(ret < 0) return MSG_Q_TIMEOUT ;

	if(ret != size) {
		LOG(ERR,"Bad size (%d != ret %d) in task %d (%s)",120,ret,dsc,strerror(errno),prio) ;
		return MSG_Q_ERROR ;	// oops
	}

	return size ;
}

/*
	Usual receive.
*/
int msgNQReceive(int dsc, char *where, int size, int timeout)
{
	int ret ;
	struct pollfd pollstruct ;
//	int i ;



	if(timeout < 0) timeout = 100000000 ;

	pollstruct.fd = dsc ;
#if defined(__linux__) || defined(__APPLE__)
	pollstruct.events = POLLIN | POLLPRI ;
#else
	pollstruct.events = POLLIN | POLLRDNORM | POLLRDBAND | POLLPRI ;
#endif
	// force size!
	size = 120 ;

	for(;;) {
		if(!msgNQCheck(dsc)) {	// not locked?
			LOG(ERR,"Task %d not there ...",dsc,0,0,0,0) ;
			return MSG_Q_NOTASK ;
		}

		LOG(DBG,"Before read %d",size,0,0,0,0) ;
		errno = 0 ;
		ret = read(dsc,where,size) ;
		LOG(DBG,"After read %d, %d",ret,errno,0,0,0);

		if(ret < 0) {
			if(errno == EAGAIN) {	// non-blocking read
				if(timeout) {	

					errno = 0 ;
					ret = poll(&pollstruct,1,1000) ;

					if((timeout % 10) == 0) {
						LOG(DBG,"Unable to rcv. from task %d in 10 seconds...",dsc,0,0,0,0) ;
					}

					timeout-- ;
					if(ret >= 0) {	// timeout or OK
						continue ;
					}

					if(errno == EINTR) {	// a signal was caught
						LOG(DBG,"Signal caught while in rcv. poll() from task %d...",dsc,0,0,0,0) ;
						continue ;	// retry ;
					}
					LOG(ERR,"poll() returned (%s)",strerror(errno),0,0,0,0) ;
					return MSG_Q_ERROR ;

				}
				else break ;
					
			}
			else {
				LOG(ERR,"Can't read from task %d (%s)",dsc,strerror(errno),0,0,0) ;
				return MSG_Q_ERROR ;
			}
		}
		break ;
		
	} ;

	if(ret < 0) return MSG_Q_TIMEOUT ;	// timeout

	if(ret != size) {
		LOG(ERR,"Read returned %d instead of %d - task %d",ret,size,dsc,0,0) ;
		return MSG_Q_ERROR ;
	}


	return 120 ;
}


/*
	Close the descriptor and mark it closed.
*/
int msgNQDelete(int desc)
{

	close(desc) ;

	return 0 ;
}


/*
	Check for the existance of the task on a previously open
	desciptor.
	Returns TRUE if the other end is still alive or 0 if it isn't
*/
int msgNQCheck(int dsc)
{
	int optval ;
	int ret ;
#if defined(__linux__) || defined(__APPLE__)
	socklen_t size;
#else
	int size ;
#endif

	if(dsc < 0) {
		LOG(WARN,"No such NQueue %d",dsc,0,0,0,0) ;
		return 0 ;	
	}

	size = sizeof(optval) ;

	// do something to the socket, doesn't matter what...
	ret = getsockopt(dsc, SOL_SOCKET, SO_KEEPALIVE, (char *)&optval, &size) ;
	if(ret < 0) {
		LOG(ERR,"getsockopt() returned error for dsc %d [%s]",dsc,strerror(errno),0,0,0) ;
		return 0 ;	// dead
	}


	return 1 ;	// OK

}


	
	

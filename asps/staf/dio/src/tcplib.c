/* Copyright 1992, Lawrence Berkeley Laboratory */

/* tcplib.c - tcp message library */

/*
modification history
--------------------
01b,17sep97,cet  -incorporate Carl Lionberger code from cosmic test
01a,30may92,whg  written.
*/

/*
DESCRIPTION
TBS...
*/
#include <sys/types.h>
#ifdef						aix
typedef unsigned long u_long;
typedef unsigned int u_int;
#endif						/* aix */
#include <sys/types.h>
#include <arpa/inet.h>
#ifndef						VXWORKS
#include <sys/time.h>
#endif						/* VXWORKS */
#include <sys/socket.h>
#include <netinet/in.h>
#ifndef						VXWORKS
#include <netdb.h>
#endif						/* VXWORKS */
#include <sys/ioctl.h>
#ifdef						sparc
#include <sys/filio.h>
#endif						/* sparc */
#include "tcplib.h"

/* #ifdef craig
int connect(int socket, struct sockaddr *name, int namelen);
#endif */

#define INET_ADDR_ERROR -1
/**********************************************************************
*
* hostGetByName - return the internet address of a host
*
*/

#ifdef VXWORKS
#include "vxWorks.h"
#include "hostLib.h"
#else
static int hostGetByName(name)
char *name;
{
	struct hostent *hp, *gethostbyname();

	if ((hp = gethostbyname(name)))
		return *(int *) hp->h_addr;

	return INET_ADDR_ERROR;
}
#endif
/**********************************************************************
*
* tcpConnect - connect to remote host
*
* RETURNS:
*/
int tcpConnect(hostName, serverPort, pSocket)
char *hostName;
int serverPort;
int *pSocket;
{
	int localSocket;
	struct sockaddr_in remoteAddr;

	localSocket = socket(AF_INET, SOCK_STREAM, 0);
	if (localSocket < 0)
		return socketCallFailed;

	memset((char *) &remoteAddr, 0, sizeof(remoteAddr));
	remoteAddr.sin_family = htons(AF_INET);
	remoteAddr.sin_port = htons(serverPort);

	remoteAddr.sin_addr.s_addr = inet_addr(hostName);
	if (remoteAddr.sin_addr.s_addr == INET_ADDR_ERROR) {
		remoteAddr.sin_addr.s_addr = hostGetByName(hostName);
		if (remoteAddr.sin_addr.s_addr == INET_ADDR_ERROR) {
			close(localSocket);
			return gethostbynameFailure;
		}
	}
	if (connect(localSocket, (struct sockaddr *) & remoteAddr,
		    sizeof(remoteAddr)) < 0) {
		close(localSocket);
		return connectFailure;
	}
	*pSocket = localSocket;
	return ok;
}
/**********************************************************************
*
*/
#ifdef							__sgi
int tcpRead(void *fd, void *buf, u_int len)
#else							/*__sgi*/
#ifdef							sun4os5pc
int tcpRead(void *fd, char *buf, int len)
#else							/*sun4os5pc*/
int tcpRead(int *fd, char *buf, int len)
#endif							/*sun4os5pc*/
#endif							/*__sgi*/
{
        int locallen, timeout = 0;

        ioctl(*fd, FIONREAD, (caddr_t)&locallen);

        while (locallen < 1 && timeout < 30)
        {
           if (timeout <20)
              usleep(100000);
           else
              sleep(1);
           timeout++;
           ioctl(*fd, FIONREAD, (caddr_t)&locallen);
        }
        if (timeout >= 30) return -1;

/*
        printf("timeout,len,locallen: %d %d %d\n ", timeout, len, locallen);
*/

        if (locallen > len) locallen = len;

        len = read(*fd, buf, locallen);

        if (len < 1) {
                return -1;
        }
        return len;
}
/**********************************************************************
*
*
*/
#ifdef							__sgi
int tcpWrite(void *fd, void *buf, u_int len)
#else							/*__sgi*/
#ifdef sun4os5pc
int tcpWrite(void *fd, char *buf, int len)
#else							/*sun4os5pc*/
int tcpWrite(int *fd, char *buf, int len)
#endif							/*sun4os5pc*/
#endif							/*__sgi*/
{
        int i, cnt;

        for (cnt = len; cnt > 0; cnt -= i, buf += i) {
                if ((i = write(*fd, buf, cnt)) == -1) {
                        return (-1);
                }
        }
        return len;
}

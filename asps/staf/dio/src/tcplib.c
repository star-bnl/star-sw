#ifndef WIN32
/* Copyright 1992, Lawrence Berkeley Laboratory */

/* tcplib.c - tcp message library */

/*
modification history
--------------------
01d,21nov97,cet  more honest defines
01c,02oct97,hjw  server mode (socket write)
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
#ifdef						__sun
#include <sys/filio.h>    /* contains definition of FIONREAD */
#endif						/* sun */
#include "tcplib.h"


#define INET_ADDR_ERROR -1
#define PP printf(
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
* tcpStartServer - start server (writer) and wait for connection from client
* (reader).
*
* RETURNS: zero for ok
* written by hjw,  04aug97
*
*/
int tcpStartServer(int serverPort, int *pSocket) {

  struct sockaddr_in serv_addr,cli_addr;
  int clilen,tempSocket,realSocket;

  tempSocket = socket(AF_INET, SOCK_STREAM, 0);
  if(tempSocket<0) return 1;

/* bbb void *memset(void *s, int c, size_t n); */
  memset((char *) &serv_addr, 0, sizeof(serv_addr));
  serv_addr.sin_family      = AF_INET;
  serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  serv_addr.sin_port        = htons(serverPort);
  if(bind(tempSocket,(struct sockaddr*)&serv_addr,sizeof(serv_addr))) {
    return 2;
  }

  listen(tempSocket,5);
  clilen=sizeof(cli_addr);
  realSocket=accept(tempSocket,(struct sockaddr*)&cli_addr,&clilen);
  close(tempSocket);
  *pSocket=realSocket;
  return 0;
}
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
 int tcpRead(int *fd, char *buf, int len)
{
        int locallen, timeout = 3;

        ioctl(*fd, FIONREAD, (caddr_t)&locallen);

        while (locallen < 1 && timeout-- )
        {
              sleep(1);
           ioctl(*fd, FIONREAD, (caddr_t)&locallen);
        }
        if (timeout <= 0) return -1;

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
int tcpWrite(int *fd, char *buf, int len)
{
        int i, cnt;

        for (cnt = len; cnt > 0; cnt -= i, buf += i) {
	  if ((i = write(*fd, buf, cnt)) == -1) return (-1);
        }
        return len;
}
#endif /* WIN32 */

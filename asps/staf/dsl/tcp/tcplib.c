/* Copyright 1992, Lawrence Berkeley Laboratory */

/* tcplib.c - tcp message library */

/*
modification history
--------------------
01a,30may92,whg  written.
*/

/*
DESCRIPTION
TBS...
*/
#include <arpa/inet.h>
#include <sys/types.h>
#ifndef VXWORKS
#include <sys/time.h>
#endif
#include <sys/socket.h>
#include <netinet/in.h>
#ifndef VXWORKS
#include <netdb.h>
#endif
#include "tcplib.h"

int connect(int socket, struct sockaddr *name, int namelen);

#define INET_ADDR_ERROR -1
/******************************************************************************
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
/******************************************************************************
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

	remoteAddr.sin_addr.s_addr = htons(inet_addr(hostName));
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
/******************************************************************************
*
*/
int tcpRead(int *fd, char *buf, int len)
{
	len = read(*fd, buf, len);

	if (len < 1) {
		return -1;
	}
	return len;
}
/******************************************************************************
*
*
*/
int tcpWrite(int *fd, char *buf, int len)
{
	int i, cnt;

	for (cnt = len; cnt > 0; cnt -= i, buf += i) {
		if ((i = write(*fd, buf, cnt)) == -1) {
			return (-1);
		}
	}
	return len;
}

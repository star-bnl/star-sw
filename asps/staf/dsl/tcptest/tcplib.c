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
#include <stdio.h>
#include <string.h>
#include "tcplib.h"

#ifdef craig
int connect(int socket, struct sockaddr *name, int namelen);
#endif

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
/*- DEBUG -*/
	char optval[256]={0};
	int i,optlen;

printf("tcpConnect - socket\n");
	localSocket = socket(AF_INET, SOCK_STREAM, 0);
	if (localSocket < 0)
		return socketCallFailed;

printf("tcpConnect - memset\n");
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
/*-----------------------
-----------------------*/
printf("tcpConnect - opts\n");
optlen=256;getsockopt(localSocket, SOL_SOCKET, SO_DEBUG, optval, &optlen);
printf("SO_DEBUG(%d) = ",optlen); for(i=0;i<optlen;i++)printf("%d,",optval[i]);printf("\n");
optlen=256;getsockopt(localSocket, SOL_SOCKET, SO_REUSEADDR, optval, &optlen);
printf("SO_REUSEADDR(%d) = ",optlen); for(i=0;i<optlen;i++)printf("%d,",optval[i]);printf("\n");
optlen=256;getsockopt(localSocket, SOL_SOCKET, SO_KEEPALIVE, optval, &optlen);
printf("SO_KEEPALIVE(%d) = ",optlen); for(i=0;i<optlen;i++)printf("%d,",optval[i]);printf("\n");
optlen=256;getsockopt(localSocket, SOL_SOCKET, SO_DONTROUTE, optval, &optlen);
printf("SO_DONTROUTE(%d) = ",optlen); for(i=0;i<optlen;i++)printf("%d,",optval[i]);printf("\n");
optlen=256;getsockopt(localSocket, SOL_SOCKET, SO_LINGER, optval, &optlen);
printf("SO_LINGER(%d) = ",optlen); for(i=0;i<optlen;i++)printf("%d,",optval[i]);printf("\n");
optlen=256;getsockopt(localSocket, SOL_SOCKET, SO_BROADCAST, optval, &optlen);
printf("SO_BROADCAST(%d) = ",optlen); for(i=0;i<optlen;i++)printf("%d,",optval[i]);printf("\n");
optlen=256;getsockopt(localSocket, SOL_SOCKET, SO_OOBINLINE, optval, &optlen);
printf("SO_OOBINLINE(%d) = ",optlen); for(i=0;i<optlen;i++)printf("%d,",optval[i]);printf("\n");
optlen=256;getsockopt(localSocket, SOL_SOCKET, SO_SNDBUF, optval, &optlen);
printf("SO_SNDBUF(%d) = ",optlen); for(i=0;i<optlen;i++)printf("%d,",optval[i]);printf("\n");
optlen=256;getsockopt(localSocket, SOL_SOCKET, SO_RCVBUF, optval, &optlen);
printf("SO_RCVBUF(%d) = ",optlen); for(i=0;i<optlen;i++)printf("%d,",optval[i]);printf("\n");
optlen=256;getsockopt(localSocket, SOL_SOCKET, SO_TYPE, optval, &optlen);
printf("SO_TYPE(%d) = ",optlen); for(i=0;i<optlen;i++)printf("%d,",optval[i]);printf("\n");
optlen=256;getsockopt(localSocket, SOL_SOCKET, SO_ERROR, optval, &optlen);
printf("SO_ERROR(%d) = ",optlen); for(i=0;i<optlen;i++)printf("%d,",optval[i]);printf("\n");
printf("tcpConnect - connect\n");
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
#ifndef sun
int tcpRead(int *fd, char *buf, int len)
#else   /*sun*/
int tcpRead(void *fd, caddr_t buf, int len)
#endif  /*sun*/
{
int ii;

printf("read =");
	len = read(*(int*)fd, buf, len);
printf(" %d %d (",*(int*)fd,len);
for(ii=0;ii<(len>20?20:len);ii++){
   if(buf[ii] < 32 || 126 < buf[ii]){
      printf("%2x ",buf[ii]);
   } else {
      printf(".%c ",buf[ii]);
   }
}
printf(")\n");

	if (len < 1) {
		return -1;
	}
	return len;
}
/******************************************************************************
*
*
*/
#ifndef sun
int tcpWrite(int *fd, char *buf, int len)
#else   /*sun*/
int tcpWrite(void *fd, caddr_t buf, int len)
#endif  /*sun*/
{
	int i, cnt;
int ii;

printf("write = %d %d (",*(int*)fd,len);
for(ii=0;ii<(len>20?20:len);ii++){
   if(buf[ii] < 32 || 126 < buf[ii]){
      printf("%2x ",buf[ii]);
   } else {
      printf(".%c ",buf[ii]);
   }
}
printf(")\n");
	for (cnt = len; cnt > 0; cnt -= i, buf += i) {
		if ((i = write(*(int*)fd, buf, cnt)) == -1) {
			return (-1);
		}
	}
	return len;
}

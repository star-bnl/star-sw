/* Copyright 1998, Lawrence Berkeley Laboratory */

/* tcptest.c - test xdrrec and winrec for dsl */

/*
modification history
--------------------
04apr98,whg  written
*/
/*
DESCRIPTION
routines to test xdrrec for dsl
*/
#include <stdio.h>
#ifdef WIN32
#include <io.h>
#include <winsock.h>
#else	/* WIN32 */
#include <stdarg.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <sys/time.h>
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#ifdef _AIX
#include <sys/select.h>
#endif /* _AIX */ 
#endif /* WIN32 */
#include "tcplib.h"

static unsigned nRead = 0;
static unsigned nWrite = 0;
static void tcpStart(void);

/*****************************************************************************
*
* tcpCloseSocket - system independent close socket
*
* RETURNS: TCP_SUCCESS if no errors, otherwise TCP_FAIL
*/
int tcpCloseSocket(TCP_SOCKET socket)
{
#ifdef WIN32
	return 0 == closesocket(socket) ? TCP_SUCCESS : TCP_FAIL;
#else
	return 0 == close(socket) ? TCP_SUCCESS : TCP_FAIL;
#endif /* WIN32 */
}
/*****************************************************************************
*
* tcpConnectToClient - server accept connect from client
*
* RETURNS: TCP_SUCCESS if no errors, otherwise TCP_FAIL
*/
int tcpConnectToClient(TCP_SOCKET *pSocket, TCP_SOCKET listenSocket)
{
	TCP_SOCKET acceptSocket;
	int length;
	struct sockaddr_in remoteAddr;

	length = sizeof(remoteAddr);
	memset((char *)&remoteAddr, 0, length);
	acceptSocket = accept(listenSocket,
		(struct sockaddr *)&remoteAddr, &length);
	if (acceptSocket == INVALID_SOCKET) {
		TCP_ERROR("accept failed");
	}
	*pSocket = acceptSocket;
	return TCP_SUCCESS;
}
/*****************************************************************************
*
* tcpConnectToServer - clinet connect to server
*
* RETURNS: TCP_SUCCESS if no errors, otherwise TCP_FAIL
*/
int tcpConnectToServer(TCP_SOCKET *pSocket, char *hostName, int port)
{
	TCP_SOCKET sock;
	struct sockaddr_in remoteAddr;
	struct hostent *hp;
	tcpStart();

	if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		TCP_ERROR("socket failed");
	}
	if ((hp = gethostbyname(hostName)) == NULL) {
		TCP_ERROR("gethostbyname failed");
	}
	memset((char *) &remoteAddr, 0, sizeof(remoteAddr));
	remoteAddr.sin_family = hp->h_addrtype;
	memcpy(&remoteAddr.sin_addr, hp->h_addr_list[0], hp->h_length);
	remoteAddr.sin_port = htons((short)port);
	if (connect(sock, (struct sockaddr *) &remoteAddr,
		    sizeof(remoteAddr)) != 0) {
		tcpCloseSocket(sock);
		TCP_ERROR("connect failed");
	}
	*pSocket = sock;
	return TCP_SUCCESS;
}
/*****************************************************************************
*
* tcpListen - set up server listen socket
*
* RETURNS: TCP_SUCCESS if no errors, otherwise TCP_FAIL
*/
int tcpListen(TCP_SOCKET *pSocket, int port)
{
	TCP_SOCKET sock;
	struct sockaddr_in localAddr;

	tcpStart();
	if ((sock = socket(AF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET) {
		TCP_ERROR("socket failed");
	}
	memset((char *) &localAddr, 0, sizeof(localAddr));
	localAddr.sin_family = AF_INET;
	localAddr.sin_port = htons((short)port);

	if (bind(sock, (struct sockaddr *) & localAddr,
		 sizeof(localAddr)) < 0) {
		tcpCloseSocket(sock);
		TCP_ERROR("bind failed");
	}
	if (listen(sock, 5) < 0) {
		TCP_ERROR("listen failed");
	}
	*pSocket = sock;
	return TCP_SUCCESS;
}
/******************************************************************************
*
* tcpRead - read routine for xdrrec
*/
int tcpRead(char *handle, char *buf, int len)
{
	int sock = *((int *)handle);

	if((len = recv(sock, buf, len, 0)) < 1) {
		return -1;
	}
	nRead += len;
	return len;
}
/******************************************************************************
*
* tcpReadCount - read byte count
*/
unsigned tcpReadCount(void)
{
	return nRead;
}
/*****************************************************************************
*
* tcpStart - initialize return TRUE
*/
static void tcpStart(void)
{
#ifdef WIN32
	int status;
	WSADATA WSAData;
	static int started = 0;
	if (!started) {
		if ((status = WSAStartup(MAKEWORD(1,1), &WSAData)) != 0) {
			fprintf(stderr, "WSAStartup failed: status %d, lastError %d\n",
				status, GetLastError());
			exit(0);
		}
	}
#endif /* WIN32 */ 
}
/******************************************************************************
*
* tcpWrite - write routine for xdrrec
*/
int tcpWrite(char *handle, char *buf, int len)
{
	int sock = *((int *)handle);

	if (send(sock, buf, len, 0) != len) {
		return -1;
	}
	nWrite += len;
	return len;
}
/******************************************************************************
*
* tcpWriteCount - write byte count
*/
unsigned tcpWriteCount(void)
{
	return nWrite;
}

/* Copyright 1998, Lawrence Berkeley Laboratory */

/* tcplib.h - tcplib definitions */

/*
modification history
--------------------
05apr98,whg  written.
*/
/*
DESCRIPTION
definitions for tcplib
*/
#ifndef TCPLIB_H
#define TCPLIB_H


#ifndef INVALID_SOCKET
#define INVALID_SOCKET ((TCP_SOCKET)(~0))
#endif /* INVALID_SOCKET */
typedef int TCP_SOCKET;

/* Misc definitions */
#define TCP_FAIL		TCP_FALSE	/* fail return */
#define TCP_FALSE		(0)			/* C style false */
#define TCP_SERVER_PORT	5000		/* TCP server port number */
#define TCP_SUCCESS		TCP_TRUE	/* success return */
#define TCP_TRUE		(1)			/* C style true */

/* error and debug macros */
#define TCP_ERROR_PRINT(msg) fprintf(stderr, "%s - %s(%d)\n",\
	msg, __FILE__, __LINE__)
#define TCP_ERROR(msg) {TCP_ERROR_PRINT(msg); return TCP_FAIL;}
#define TCP_TRACE fprintf(stderr, "TRACE: %s.%d\n", __FILE__, __LINE__)

int tcpCloseSocket(TCP_SOCKET socket);
int tcpConnectToClient(TCP_SOCKET *pSocket, TCP_SOCKET listenSocket);
int tcpConnectToServer(TCP_SOCKET *pSocket, char *hostName, int port);
int tcpListen(TCP_SOCKET *pSocket, int port);
int tcpRead(char *handle, char *buf, int len);
unsigned tcpReadCount(void);
int tcpWrite(char *handle, char *buf, int len);
unsigned tcpWriteCount(void);
#endif /* TCPLIB_H */

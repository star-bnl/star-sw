/* Copyright 1993, Lawrence Berkeley Laboratory */

/* tcplib.h - definitions for tcp test */

/*
modification history
--------------------
01a,30jul93,whg  written.
*/

/*
DESCRIPTION
TBS ...
*/
#ifndef TCP_LIB_H
#define TCP_LIB_H

#define TCP_TEST_COUNT 200
typedef	enum {
	ok = 0,
	acceptFailure,
	bindFailure,
	connectFailure,
	gethostbynameFailure,
	listenFailure,
	socketCallFailed,
} TCP_CODE_T;
#ifndef OK
#define OK 0
#endif
char *tcpCodeStr(int code);
#define TCP_SERVER_PORT 5000

int tcpConnect(char *hostName, int serverPort, int *pSocket);
int tcpRead(int *pFd, char *buf, int nbytes);
int tcpServer(int serverPort, int (*serverFcn)(), int spawn);
int tcpWrite(int *pFd, char *buf, int nbytes);

int close(int fd);
int fork(void);
int listen(int s, int backlog);
char *memset(char *ptr, int val, int nbyte);
int read(int fd, char *buf, int nbytes);
int socket(int domain, int type, int protocol);
int write(int fd, char *buf, int nbytes);

#endif

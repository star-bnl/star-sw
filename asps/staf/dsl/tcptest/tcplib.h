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
	socketCallFailed
} TCP_CODE_T;
#ifndef OK
#define OK 0
#endif
char *tcpCodeStr(int code);
#define TCP_SERVER_PORT 7253

int tcpConnect(char *hostName, int serverPort, int *pSocket);
int tcpServer(int serverPort, int (*serverFcn)(), int spawn);
#ifndef sun
int tcpRead(int *pFd, char *buf, int nbytes);
int tcpWrite(int *pFd, char *buf, int nbytes);
#else /*sun*/
int tcpRead(void *pFd, caddr_t buf, int nbytes);
int tcpWrite(void *pFd, caddr_t buf, int nbytes);
#endif /*sun*/

int close(int fd);
int listen(int s, int backlog);

#ifndef sun
/*char *memset(char *ptr, int val, int nbyte);*/
int fork(void);
int read(int fd, char *buf, int nbytes);
int write(int fd, char *buf, int nbytes);
#else /*sun*/
long fork(void);
int read(int fd, void *buf, unsigned int nbytes);
int write(int fd, const void *buf, unsigned int nbytes);
#endif /*sun*/

int socket(int domain, int type, int protocol);

#endif

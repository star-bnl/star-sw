/* Copyright 1993, Lawrence Berkeley Laboratory */
#ifdef __cplusplus
extern "C" {
#endif

/* tcplib.h - definitions for tcp test */

/*
modification history
--------------------
01b,19jul96,cet  C++ modified.
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
#define TCP_SERVER_PORT 5000

int tcpConnect(char *hostName, int serverPort, int *pSocket);
int tcpServer(int serverPort, int (*serverFcn)(), int spawn);
#ifdef __sgi
int tcpRead(void *pFd, void *buf, u_int nbytes);
int tcpWrite(void *pFd, void *buf, u_int nbytes);
#else /*IRIX*/
#ifdef sun4os5pc
int tcpRead(void *pFd, char *buf, int nbytes);
int tcpWrite(void *pFd, char *buf, int nbytes);
#else /*sun4os5pc*/
int tcpRead(int *pFd, char *buf, int nbytes);
int tcpWrite(int *pFd, char *buf, int nbytes);
#endif /*sun4os5pc*/
#endif /*IRIX*/

int close(int fd);
int fork(void);
int listen(int s, int backlog);
#ifndef sun
/*-cet01b-char *memset(char *ptr, int val, int nbyte); */
#endif
int read(int fd, char *buf, int nbytes);
int socket(int domain, int type, int protocol);
int write(int fd, char *buf, int nbytes);

#endif

#ifdef __cplusplus
}
#endif

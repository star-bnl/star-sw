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
int tcpStartServer(int serverPort, int *pSocket); /* blocks until connect */


#ifdef                                                  __sun
#define TCPREAD  int (*)(void *, char *, int)
#define TCPWRITE int (*)(void *, char *, int)
#endif

#ifdef                                                  __sgi
#define TCPREAD  int (*)(void *, void *, u_int)
#define TCPWRITE int (*)(void *, void *, u_int)
#endif

#ifdef                                                  Linux
#if defined(i386_linux2) || defined(i386_redhat50)
#define TCPREAD  int (*)(...)             
#define TCPWRITE int (*)(...)             
#endif
#ifdef i386_redhat51
#define TCPREAD  int (*)(char *, char *, int)             /*JCS*/
#define TCPWRITE int (*)(char *, char *, int)             /*JCS*/
#endif
#endif /*Linux/

#ifndef TCPREAD
#define TCPREAD  int (*)(int  *, char *, int)
#define TCPWRITE int (*)(int  *, char *, int)
#endif                                                 /*JCS*/

int tcpRead (int  *fd, char *buf, int len);
int tcpWrite(int  *fd, char *buf, int len);

#ifndef WIN32
#ifndef Linux
  int read(int fd, char *buf, int nbytes);
  int socket(int domain, int type, int protocol);
  int write(int fd, char *buf, int nbytes);
  int close(int fd);
  int fork(void);
  int listen(int s, int backlog);
#else
  int read(int fd, void *buf, unsigned int nbytes);
  int socket(int domain, int type, int protocol);
  int write(int fd, const void *buf, unsigned int nbytes);
#endif
#endif

#endif

#ifdef __cplusplus
}
#endif

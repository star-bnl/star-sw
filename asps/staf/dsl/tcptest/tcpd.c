/* Copyright 1992, Lawrence Berkeley Laboratory */

/* tcpd.c - unix tcp server */

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
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <netinet/in.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include "tcplib.h"

#ifdef craig
int accept(int s, struct sockaddr *addr, int *addrlen);
int bind(int s, struct sockaddr *name, int namelen);
int wait3(int *statusp, int options, struct rusage *rusage);
#endif

static void reaper();

/******************************************************************************
*
* tcpServer - tcp network server
*
* RETURNS:
*/
int tcpServer(serverPort, serverFcn, spawn)
int serverPort;
int (*serverFcn) ();
int spawn;
{
	extern int errno;
	int length, listenSocket, acceptSocket;
	struct sockaddr_in localAddr, remoteAddr;

	if ((listenSocket = socket(AF_INET, SOCK_STREAM, 0)) < 0)
		return socketCallFailed;

	memset((char *) &localAddr, 0, sizeof(localAddr));
	localAddr.sin_family = htons(AF_INET);;
	localAddr.sin_port = htons(serverPort);

	if (bind(listenSocket, (struct sockaddr *) & localAddr,
		 sizeof(localAddr)) < 0) {
		close(listenSocket);
		return bindFailure;
	}
	if (listen(listenSocket, 5) < 0) {
		perror("listen failed\n");
		return listenFailure;
	}

	signal(SIGCHLD, reaper);/* eliminate zombies */

	for (;;) {
printf(".");
		errno = 0;
		length = sizeof(remoteAddr);
		memset((char *) &remoteAddr, 0, length);

		acceptSocket = accept(listenSocket,
				 (struct sockaddr *) & remoteAddr, &length);

		if (acceptSocket < 0) {
			if (errno == EINTR)
				continue;	/* if signal SIGCHLD */
			close(listenSocket);

			return acceptFailure;
		}

		if (spawn) {
			if (fork() == 0) {
printf("child process\n");
				close(listenSocket);
				return serverFcn(acceptSocket);
			}
			else {
printf("parent process\n");
			}
		}
		else {
			serverFcn(acceptSocket);
		}
		close(acceptSocket);
	}
}
/******************************************************************************
*
* reaper - prevent zombies
*
* "reap" child processes that have exited.
* Copy from sun IPC tutorial chapter 9.
*
* RETURNS:
*/
static void reaper()
{
	int status;

	while (wait3(&status, WNOHANG, 0) > 0)
		continue;
}

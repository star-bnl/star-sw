/* Copyright 1992, Lawrence Berkeley Laboratory */

/* sdld.c - simple data logger unix daemon */

/*
modification history
--------------------
01a,30may92,whg  written.
*/

/*
DESCRIPTION
TBS...
*/
#include <rpc/rpc.h>
#include "tcplib.h"
#include "dsxdr.h"

extern int errno;

/******************************************************************************
*
* main - sdld main
*
* RETURNS:
*/
void main(argc, argv)
int argc;
char **argv;
{
	int dsServer();
	int spawn = 1, status;

	errno = 0;
	status = tcpServer(TCP_SERVER_PORT, dsServer, spawn);

	if (status)
		printf("tcpdd exited - %s\n", tcpCodeStr(status));

	if (errno)
		perror("errno");
}
/******************************************************************************
*
* dsServer - daemon for
*
* RETURNS:
*/
int dsServer(socket)
int socket;
{
	XDR xdr;

	memset((char *)&xdr, 0, sizeof(xdr));
#ifndef sun
	xdrrec_create(&xdr, 0, 0, &socket, tcpRead, tcpWrite);
#else   /*!sun*/
	xdrrec_create(&xdr, 0, 0,
		(const caddr_t)&socket,
		(int (*) (void *, caddr_t, int))tcpRead,
		(int (*) (void *, caddr_t, int))tcpWrite);
#endif  /*!sun*/
	xdr.x_op = XDR_DECODE;

	printf("xdrrec_skiprecord: %d\n", xdrrec_skiprecord(&xdr));
	if (!dsReadTest(&xdr, TCP_TEST_COUNT)) {
		dsPerror("dsReadTest failed");
	}
	else {
		printf("read done\n");
	}
	xdr_destroy(&xdr);
	return 0;
}

/* Copyright 1993, Lawrence Berkeley Laboratory */

/* vxtest.c - vxworks test of simple data logger */

/*
modification history
--------------------
29jul93,whg  written.
*/

/*
DESCRIPTION
TBS...
*/
#include <rpc/rpc.h>
#include "tcplib.h"
#include "dsxdr.h"

/******************************************************************************
*
* dsTestTcp - test 
*/
int dsTestTcp(host)
char *host;
{
	int socket, status;
	XDR xdr;

	status = tcpConnect(host, TCP_SERVER_PORT, &socket);
	if (status) {
		printf("tcpConnect failed\n");
		return -1;
	}
#ifndef sun
        xdrrec_create(&xdr, 0, 0, &socket, tcpRead, tcpWrite);
#else   /*!sun*/
        xdrrec_create(&xdr, 0, 0,
                (const caddr_t)&socket,
                (int (*) (void *, caddr_t, int))tcpRead,
                (int (*) (void *, caddr_t, int))tcpWrite);
#endif  /*!sun*/
	xdr.x_op = XDR_ENCODE;
	if (!dsWriteTest(&xdr, TCP_TEST_COUNT)) {
		dsPerror("dsWriteTest failed");
	}
	if (!xdrrec_endofrecord(&xdr, 1)) {
		printf("xdr_endofrecord failed for encode\n");
	}
	xdr_destroy(&xdr);
	close(socket);
	return 0;
}

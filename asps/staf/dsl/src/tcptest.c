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
#include <rpc/rpc.h>
#include "tcplib.h"
#define DS_ADVANCED
#include "dsxdr.h"

static void clientTest(char *host);
static void serverTest(void);
#ifdef _CRAY
#define XDRREC_CAST (unsigned long)
#else
#define XDRREC_CAST 
#endif
#define DS_COUNT 200
/****************************************************************************
*/
void main(int argc, char **argv)
{
	if (argc == 1) {
		printf("server starting\n\nto run client type:\n\ttcptest <host>\n");
		serverTest();
	}
	else {
		clientTest(argv[1]);
	}
}
/****************************************************************************
*/
static void clientTest(char *host)
{
	double t0;
	TCP_SOCKET sock;
	XDR xdr;

	if (!tcpConnectToServer(&sock, host, TCP_SERVER_PORT)) {
		return;
	}
	xdrrec_create(&xdr, 0, 0, XDRREC_CAST&sock, tcpRead, tcpWrite);
	xdr.x_op = XDR_DECODE;
	if (!xdrrec_skiprecord(&xdr)) {
		TCP_ERROR_PRINT("xdrrec_skiprecord failed");
		goto done;
	}
	t0 = msecTime(NULL);
	if (!dsReadTest(&xdr, DS_COUNT)) {
		dsPerror("dsReadTest failed");
		goto done;
	}
	printf("Read success, elapsed time, %.3f sec, %d bytes\n",
		msecTime(NULL) - t0, tcpReadCount());
done:
	xdr_destroy(&xdr);
	tcpCloseSocket(sock);
}
/****************************************************************************
*/
static void serverTest(void)
{
	double t0;
	int bigEndian = TRUE;
	unsigned count = 0;
	TCP_SOCKET listenSocket, sock;
	XDR xdr;

	if (!tcpListen(&listenSocket, TCP_SERVER_PORT)) {
		return;
	}
	for (;;) {
		if(!tcpConnectToClient(&sock, listenSocket)) {
			return;
		}
		xdrrec_create(&xdr, 0, 0, XDRREC_CAST&sock, tcpRead, tcpWrite);
		xdr.x_op = XDR_ENCODE;
		t0 = msecTime(NULL);
		if (!dsWriteTest(&xdr, DS_COUNT, bigEndian)) {
			dsPerror("dsWriteTest failed");
			goto done;
		}
		if (!xdrrec_endofrecord(&xdr, TRUE)) {
			TCP_ERROR_PRINT("xdrrec_endofrecord failed");
			goto done;
		}
		printf("Write success, elapsed time %.3f sec, %d bytes %s Endian\n",
			msecTime(NULL) - t0, tcpWriteCount() - count,
			bigEndian ? "Big" : "Little");
		count = tcpWriteCount();
		bigEndian = !bigEndian;
done:
		xdr_destroy(&xdr);
		tcpCloseSocket(sock);
	}
}

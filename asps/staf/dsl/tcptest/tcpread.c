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
#include <sys/types.h>
#include "tcplib.h"
#include "dsxdr.h"

/**********************************************************************
*
* dsTestTcp - test 
*/
int dsTestTcpR(host)
char *host;
{
	int socket, status;
	XDR xdr;
		/*int len=4,ii;*/
char buf[100];	/*, *str=buf;*/

	status = tcpConnect(host, TCP_SERVER_PORT, &socket);
	if (status) {
		printf("tcpConnect failed\n");
		return -1;
	}
printf("### %p %p ###\n",tcpRead, tcpWrite);
	memset((char*)&xdr,0,sizeof(xdr));
#ifndef sun
        xdrrec_create(&xdr, 1000, 1000, &socket, tcpRead, tcpWrite);
#else   /*!sun*/
        xdrrec_create(&xdr, 1000, 1000,
                (const caddr_t)&socket,
                (int (*) (void *, caddr_t, int))tcpRead,
                (int (*) (void *, caddr_t, int))tcpWrite);
#endif  /*!sun*/
	xdr.x_op = XDR_DECODE;
/* printf("%d %p %d %d %d %d\n",xdr.x_op,xdr.x_ops,xdr.x_public
	,*xdr.x_private,xdr.x_base,xdr.x_handy); */
printf("(((%d)))\n",socket);
/* printf("(((%d)))\n",len = tcpRead(&socket,buf,len)); */
/* printf("(((%d)))\n",XDR_GETBYTES(&xdr,buf,len)); */
/* printf("(((%d)))\n",xdr_string(&xdr,&str,sizeof(buf))); 
printf("(%s)\n",str); */
/*
printf("read = (");
for(ii=0;ii<len;ii++){
   if(buf[ii] < 32 || 126 < buf[ii]){
      printf("%2x ",buf[ii]);
   } else {
      printf(".%c ",buf[ii]);
   }
}
printf(")\n");
*/

	printf("xdrrec_skiprecord: %d\n", xdrrec_skiprecord(&xdr));
	if (!dsReadTest(&xdr, TCP_TEST_COUNT)) {
		dsPerror("dsReadTest failed");
	}
	sleep(10);
	xdr_destroy(&xdr);
/*	close(socket);	*/
	return 0;
}

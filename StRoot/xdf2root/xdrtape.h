/* Copyright 1995, Lawrence Berkeley Laboratory */
 
/* xdrtape.h */

/*
modification history
--------------------
01a,30jan95,whg  written
*/

/*
DESCRIPTION
definitions for xdrtape
*/
#ifndef _XDRTAPE_H
#define _XDRTAPE_H

int xdrtape_create(XDR *xdrs, enum xdr_op op, int fd, unsigned size,
	int (*iofcn)(int fd, char *buf, unsigned count));
void xdrtape_get_error(XDR *xdrs, int *pCode, char **pMsg);
int xdrtape_flush(XDR *xdrs);
void xdrtape_perror(XDR *xdrs, char *msg);

#define XDRTAPE_DEFAULT_BLOCK_SIZE	(2 << 14)	/* 32K bytes */

/* codes returned by xdrtape_errcode */
#define XDRTAPE_OK	0
#define XDRTAPE_INSUFFICIENT_MEMORY	1
#define XDRTAPE_INVALID_XDR_OP	2
#define XDRTAPE_IO_WRITE_ERROR	3
#define XDRTAPE_IO_READ_ERROR	4
#define XDRTAPE_READ_EOF	5
#define XDRTAPE_UNIMPLEMENTED_FCN	6

#endif /* _XDRTAPE_H */

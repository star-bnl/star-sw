/* Copyright 1995, Lawrence Berkeley Laboratory */
 
/* xdrtape.c */

/*
modification history
--------------------
01a,30jan95,whg  written
*/
/*
DESCRIPTION
These routines provide a generic tape to xdr interface.
Routines mtRead and mtWrite must be supplied to support a specific
type of tape.  These routines must have the signature:
int iofcn(int fd, char *buf, unsigned count)
Where fd is an I/O descriptor and buf points to a buffer with count bytes.
mtRead returns the number of bytes if data is read, zero if an EOF is read
and -1 if an error occured.  mtWrite returns count for success else an
error has occured.     
*/
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <rpc/rpc.h>
#if defined(sparc) && !defined(ntohl)
/* big-endian sun */
#define ntohl(x)    (x)
#define htonl(x)    (x)
#endif
#include "asuAlloc.h"
#include "xdrtape.h"
/*****************************************************************************
*
* macros
*
*/
#define XDRTAPE_ERROR(code) {xdrs->x_handy = code; return FALSE;}

#define BUFLEN(pT) ((pT)->limit - (pT)->first)
#define BUFDATA(pT) ((pT)->in - (pT)->out)
#define BUFSPACE(pT) ((pT)->limit - (pT)->in)
/*****************************************************************************
*
* Function prototypes for static functions
*
*/
static void xdrtape_destroy(XDR *xdrs);
static bool_t xdrtape_getbytes(XDR *xdrs, void *addr, unsigned len);
static bool_t xdrtape_getlong(XDR *xdrs, long *lp);
static unsigned xdrtape_getpos(XDR *xdrs);
static long *xdrtape_inline(XDR *xdrs, int len);
static bool_t xdrtape_putbytes(XDR *xdrs, void *addr, unsigned len);
static bool_t xdrtape_putlong(XDR *xdrs, long *lp);
static bool_t xdrtape_setpos(XDR *xdrs, unsigned pos);
/*****************************************************************************
*
* xdrtape_ops - pointers to xdr_ops for xdrtape streams. DO NOT change order.
*
*/
static struct  xdr_ops xdrtape_ops = {
	xdrtape_getlong,
	xdrtape_putlong,
	xdrtape_getbytes,
	xdrtape_putbytes,
	xdrtape_getpos,
	xdrtape_setpos,
	xdrtape_inline,
	xdrtape_destroy
};
/*****************************************************************************
*
* TAPEBUF_T - buffer structure for physical tape records 
*
*/
typedef struct xdr_tapebuf_t {
	int (*iofcn)(int fd, char *buf, unsigned count);
	int fd;			/*I/O descriptor for tape device */
	unsigned pos;	/* number of bytes transfered */
	char *first;	/* address of buffer first byte*/
	char *in;		/* address of next empty byte */
	char *out;		/* address of next data byte */ 
	char *limit;	/* address of buffer last byte + 1 */
}TAPEBUF_T;
/*****************************************************************************
*
* xdrtape_create - create data structures for a xdrtape stream
*
*/
int xdrtape_create(XDR *xdrs, enum xdr_op op, int fd, unsigned size,
	int (*iofcn)(int fd, char *buf, unsigned count))
{
	TAPEBUF_T *pTape;

	memset(xdrs, 0, sizeof(XDR));
	
	if (size <= 0) {
		size = XDRTAPE_DEFAULT_BLOCK_SIZE;
	}
	if (!(pTape = (TAPEBUF_T *)CALLOC(1, sizeof(TAPEBUF_T) + size))) {
		XDRTAPE_ERROR(XDRTAPE_INSUFFICIENT_MEMORY);
	}
	xdrs->x_private = (caddr_t)pTape; 	
	xdrs->x_ops = &xdrtape_ops;
	xdrs->x_op = op;
	pTape->fd = fd;
	pTape->iofcn = iofcn;
	pTape->first = pTape->in = pTape->out = (char *)&pTape[1];
	pTape->limit = pTape->first + size;

	return TRUE;
}
/*****************************************************************************
*
* xdrtape_destroy - free data structures for a xdrtape stream
*
*/
static void xdrtape_destroy(XDR *xdrs)
{
	FREE(xdrs->x_private);
}
/*****************************************************************************
*
* xdrtape_get_error - return reason for failure
*
*/
#define ERROR_CASE(c) case c: {ptr = #c; break;}
void xdrtape_get_error(XDR *xdrs, int *pCode, char **pMsg)
{
	char *ptr;

	switch (xdrs->x_handy) {
		ERROR_CASE(XDRTAPE_OK);
		ERROR_CASE(XDRTAPE_INSUFFICIENT_MEMORY);
		ERROR_CASE(XDRTAPE_INVALID_XDR_OP);
		ERROR_CASE(XDRTAPE_IO_WRITE_ERROR);
		ERROR_CASE(XDRTAPE_IO_READ_ERROR);
		ERROR_CASE(XDRTAPE_READ_EOF);
		ERROR_CASE(XDRTAPE_UNIMPLEMENTED_FCN);
		default:
			ptr = "XDRTAPE_INVALID_ERROR_CODE";
			break;
	}
	if (pMsg != NULL) {
		*pMsg = ptr;
	}
	if (pCode != NULL) {
		*pCode =xdrs->x_handy;
	}
	return;
}
/*****************************************************************************
*
* xdrtape_flush - unconditional write of buffered data to tape
*
*/
int xdrtape_flush(XDR *xdrs)
{
	TAPEBUF_T *pTape = (TAPEBUF_T *)xdrs->x_private;
	int n;

	if (xdrs->x_op != XDR_ENCODE) {
		XDRTAPE_ERROR(XDRTAPE_INVALID_XDR_OP);
	}
	n = BUFDATA(pTape);
	if (n > 0) {
		if (pTape->iofcn(pTape->fd, pTape->out, n) != n) {
			XDRTAPE_ERROR(XDRTAPE_IO_WRITE_ERROR);
		}
		pTape->pos += n;
		pTape->in = pTape->out = pTape->first;
	}
	else {
		assert(n == 0);
	}
	return TRUE; 
}
/*****************************************************************************
*
* xdrtape_getbytes - return bytes from xdr stream
*
*/
static bool_t xdrtape_getbytes(XDR *xdrs, void *addr, unsigned len)
{
 	TAPEBUF_T *pTape = (TAPEBUF_T *)xdrs->x_private;
	int  n;

	if (xdrs->x_op != XDR_DECODE) {
		XDRTAPE_ERROR(XDRTAPE_INVALID_XDR_OP);
	}
	while (len > 0) {
		/* calculate data in buffer */
		n = BUFDATA(pTape);
		if (n <= 0) {
			assert(n == 0);
			n = pTape->iofcn(pTape->fd, pTape->first, BUFLEN(pTape));
			if (n == 0) {
				XDRTAPE_ERROR(XDRTAPE_READ_EOF);
			}
			else if(n < 0) {
				XDRTAPE_ERROR(XDRTAPE_IO_READ_ERROR);
			}
			pTape->pos += n;
			pTape->out = pTape->first;
			pTape->in = pTape->first + n;
		}
		if ((unsigned)n > len) {
			n = len;
		}
		memcpy(addr, pTape->out, n);
		pTape->out += n;
		addr = ((char *)addr) + n;
		len -= n;
	}
	return TRUE;
}
/*****************************************************************************
*
* xdrtape_getlong - return long in internal representation
*
*/
static bool_t xdrtape_getlong(XDR *xdrs, long *lp)
{
	unsigned long l;
	
	if (!xdrtape_getbytes(xdrs, &l, sizeof(long))) {
		return FALSE;
	}
	*lp = (long)ntohl(l);
	return TRUE;
}
/*****************************************************************************
*
* xdrtape_getpos - return number of bytes transfered to or from tape
*
*/
static unsigned xdrtape_getpos(XDR *xdrs)
{
	return ((TAPEBUF_T *)xdrs->x_private)->pos;
}
/*****************************************************************************
*
* xdrtape_inline - not implemented
*
*/
static long *xdrtape_inline(XDR *xdrs, int len)
{
	len = 0; /* to avoid warning from some compilers */
	XDRTAPE_ERROR(XDRTAPE_UNIMPLEMENTED_FCN);
}
/*****************************************************************************
*
* xdrtape_perror - print last error message
*
*/
void xdrtape_perror(XDR *xdrs, char *msg)
{
	char *p =": ", *s;

	if (msg == NULL) {
		p = msg = "";
	}
	xdrtape_get_error(xdrs, NULL, &s);
	fprintf(stderr, "%s%s%s\n", msg, p, s);
}
/*****************************************************************************
*
* xdrtape_putbytes - transfer bytes to xdr stream
*
*/
static bool_t xdrtape_putbytes(XDR *xdrs, void *addr, unsigned len)
{
 	TAPEBUF_T *pTape = (TAPEBUF_T *)xdrs->x_private;
	int n;

	if (xdrs->x_op != XDR_ENCODE) {
		XDRTAPE_ERROR(XDRTAPE_INVALID_XDR_OP);
	}
	while (len > 0) {
		n = BUFSPACE(pTape);
		if (n <= 0) {
			assert(n == 0);
			if (!xdrtape_flush(xdrs)) {
				return FALSE;
			}
			n = BUFSPACE(pTape);
			assert(n > 0);
		}
		if ((unsigned)n > len) {
			n = len;
		}
		memcpy(pTape->in, addr, n);
		addr = ((char *)addr) + n;
		pTape->in += n;
		len -= n;
	}
	return TRUE;
}
/*****************************************************************************
*
* xdrtape_putlong - translate long to external rep and put in xdr stream
*
*/
static bool_t xdrtape_putlong(XDR *xdrs, long *lp)
{
	long l;

	l = (long)htonl((unsigned long)(*lp));
	return xdrtape_putbytes(xdrs, &l, sizeof(long));
}
/*****************************************************************************
*
* xdrtape_setpos - not implemented
*
*/
static bool_t xdrtape_setpos(XDR *xdrs, unsigned pos)
{
	pos = 0; /* to avoid warning from some compilers */
	XDRTAPE_ERROR(XDRTAPE_UNIMPLEMENTED_FCN);
}

/* Copyright 1993, Lawrence Berkeley Laboratory */

/* dosxdr.c - simple xdrstdio routines for dos */

/*
modification history
--------------------
01a,24apr93,whg  written.
*/

/*
DESCRIPTION
TBS ...
*/
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <rpc/rpc.h>
#define DS_PRIVATE
#include "dstype.h"
#include "dsxdr.h"

/******************************************************************************
*
* define static used to create XDR structure
*/
static bool_t xdrstdio_getlong(XDR *xdrs, long *lp);
static bool_t xdrstdio_putlong(XDR *xdrs, long *lp);
static bool_t xdrstdio_getbytes(XDR *xdrs, char *cp, unsigned len);
static bool_t xdrstdio_putbytes(XDR *xdrs, char *cp, unsigned len);
static unsigned xdrstdio_getpos(XDR *xdrs);
static bool_t xdrstdio_setpos(XDR *xdrs, unsigned pos);
static long *xdrstdio_inline(XDR *xdrs, unsigned pos);
static void xdrstdio_destroy(XDR *xdrs);

static struct xdr_ops xdrstdio_ops = {
	xdrstdio_getlong,
	xdrstdio_putlong,
	xdrstdio_getbytes,
	xdrstdio_putbytes,
	xdrstdio_getpos,
	xdrstdio_setpos,
	xdrstdio_inline,
	xdrstdio_destroy,
};
/******************************************************************************
*
* xdr_bytes - counted block opaque bytes
*
*/
bool_t xdr_bytes(XDR *xdrs, char **cpp, unsigned *sizep, unsigned maxsize)
{
	char *sp = *cpp;
	unsigned nodesize;

	if (!xdr_u_int(xdrs, sizep)) {
		return FALSE;
	}
	if ((nodesize = *sizep) > maxsize && xdrs->x_op != XDR_FREE) {
		return FALSE;
	}

	switch (xdrs->x_op) {

	case XDR_DECODE:
		if (nodesize == 0) {
			return TRUE;
		}
		if (sp == NULL) {
			*cpp = sp = (char *)malloc(nodesize);
		}
		if (sp == NULL) {
			fprintf(stderr, "xdr_bytes: out of memory\n");
			return FALSE;
		}
		/* fall into ... */
	case XDR_ENCODE:
		return xdr_opaque(xdrs, sp, nodesize);
	case XDR_FREE:
		if (sp != NULL) {
			free(sp);
			*cpp = NULL;
		}
		return TRUE;
	default:
		return FALSE;
	}
}
	
/******************************************************************************
*
* xdr_double - translate IEEE little Endian double to XDR
*
*/
bool_t xdr_double(xdrs, dp)
XDR *xdrs;
double *dp;
{
	char c[8], *cp = (char *)dp;

	if (xdrs->x_op == XDR_DECODE) {
		if (!XDR_GETBYTES(xdrs, c, 8)) {
			return FALSE;
		}
		cp[0] = c[7];
		cp[1] = c[6];
		cp[2] = c[5];
		cp[3] = c[4];
		cp[4] = c[3];
		cp[5] = c[2];
		cp[6] = c[1];
		cp[7] = c[0];
		return TRUE;
	}
	if (xdrs->x_op == XDR_ENCODE) {
		c[0] = cp[7];
		c[1] = cp[6];
		c[2] = cp[5];
		c[3] = cp[4];
		c[4] = cp[3];
		c[5] = cp[2];
		c[6] = cp[1];
		c[7] = cp[0];
		return XDR_PUTBYTES(xdrs, c, 8);
	}
	if (xdrs->x_op == XDR_FREE) {
		return TRUE;
	}
	return FALSE;
}
/******************************************************************************
*
* xdr_float - little Endian IEEE float to XDR
*
*/
bool_t xdr_float(xdrs, fp)
XDR *xdrs;
float *fp;
{
	return xdr_long(xdrs, (long *)fp);
}
/******************************************************************************
*
* xdr_free - free data structures
*
*/
void xdr_free(xdrproc_t proc, char *objp)
{
	XDR x;

	x.x_op = XDR_FREE;
	((void (*)(XDR *, char *))(*proc))(&x, objp);
}
/******************************************************************************
*
* xdr_int - int to XDR
*
*/
bool_t xdr_int(xdrs, ip)
XDR *xdrs;
int *ip;
{
	long l;

	if (xdrs->x_op == XDR_DECODE) {
		if (!XDR_GETLONG(xdrs, &l)) {
			return FALSE;
		}
		if (l < INT_MIN || l > INT_MAX) {
			return FALSE;
		}
		*ip = (int)l;
		return TRUE;
	}
	if (xdrs->x_op == XDR_ENCODE) {
		l = (long)*ip;
		return XDR_PUTLONG(xdrs, &l);
	}
	if (xdrs->x_op == XDR_FREE)
		return TRUE;

	return FALSE;
}
/******************************************************************************
*
* xdr_u_int - unsigned to XDR
*
*/
bool_t xdr_u_int(xdrs, uip)
XDR *xdrs;
unsigned *uip;
{
	unsigned long ul;

	if (xdrs->x_op == XDR_DECODE) {
		if (!XDR_GETLONG(xdrs, (long *)&ul)) {
			return FALSE;
		}
		if (ul > UINT_MAX) {
			return FALSE;
		}
		*uip = (unsigned)ul;
		return TRUE;
	}
	if (xdrs->x_op == XDR_ENCODE) {
		ul = (unsigned long)*uip;
		return XDR_PUTLONG(xdrs, (long *)&ul);
	}
	if (xdrs->x_op == XDR_FREE) {
		return TRUE;
	}
	return FALSE;
}
/******************************************************************************
*
* xdr_long - long to XDR
*
*/
bool_t xdr_long(xdrs, lp)
XDR *xdrs;
long *lp;
{
	if (xdrs->x_op == XDR_DECODE) {
		return XDR_GETLONG(xdrs, lp);
	}
	if (xdrs->x_op == XDR_ENCODE) {
		return XDR_PUTLONG(xdrs, lp);
	}
	if (xdrs->x_op == XDR_FREE) {
		return TRUE;
	}
	return FALSE;
}
/******************************************************************************
*
* xdr_u_long - same as xdr_long
*
*/
bool_t xdr_u_long(xdrs, ulp)
XDR *xdrs;
unsigned long *ulp;
{
	return xdr_long(xdrs, (long *)ulp);
}
/******************************************************************************
*
* xdr_opaque - bytes to XDR
*
*/
bool_t xdr_opaque(XDR *xdrs, char *cp, unsigned cnt)
{
	int pad;
	long fill;

	if (cnt == 0) {
		return TRUE;
	}
	pad = DS_PAD(cnt, BYTES_PER_XDR_UNIT);

	if (xdrs->x_op == XDR_DECODE) {
		if (!XDR_GETBYTES(xdrs, cp, cnt)) {
			return FALSE;
		}
		if (pad == 0) {
			return TRUE;
		}
		return XDR_GETBYTES(xdrs, (char *)&fill , pad);
	}
	if (xdrs->x_op == XDR_ENCODE) {
		if (!XDR_PUTBYTES(xdrs, cp, cnt)) {
			return FALSE;
		}
		if (pad == 0) {
			return TRUE;
		}
		fill = 0;
		return XDR_PUTBYTES(xdrs, (char *)&fill , pad);
	}
	if (xdrs->x_op == XDR_FREE) {
		return TRUE;
	}
	return FALSE;
}
/******************************************************************************
*
* xdr_string - string to XDR counted bytes
*
*/
bool_t xdr_string(XDR *xdrs, char **cpp, unsigned maxsize)
{
	char *sp = *cpp;
	unsigned long size;

	if (xdrs->x_op == XDR_FREE && sp == NULL) {
		return TRUE;
	}
	if (xdrs->x_op == XDR_ENCODE || xdrs->x_op == XDR_FREE) {
		size = strlen(sp);
	}
	if (!xdr_long(xdrs, (long *)&size)) {
		return FALSE;
	}
	if (size > maxsize) {
		return FALSE;
	}
	switch (xdrs->x_op) {
	case XDR_DECODE:
		if (sp == NULL) {
			*cpp = sp = malloc((unsigned)size + 1);
		}
		if (sp == NULL) {
			fprintf(stderr, "xdr_string out of memory\n");
			return FALSE;
		}
		sp[(unsigned)size] = '\0';
	/* fall into */
	case XDR_ENCODE:
		return xdr_opaque(xdrs, sp, (unsigned)size);
	case XDR_FREE:
		free(sp);
		*cpp = NULL;
		return TRUE;
	}
	return FALSE;
}
/******************************************************************************
*
* xdrmem_create - dummy create xdr memory region
*
*/
void xdrmem_create(XDR *xdrs, char *addr, unsigned size, enum xdr_op op)
{
	xdrs->x_op = op;
	xdrs->x_ops = NULL;
	xdrs->x_private = xdrs->x_base = addr;
	xdrs->x_handy = size;
	printf("no xdrmem\n");
	exit(0);
}
/******************************************************************************
*
* xdrstdio_create - stdio file stream
*
*/
void xdrstdio_create(XDR *xdrs, FILE *stream, enum xdr_op op)
{
	xdrs->x_op = op;
	xdrs->x_ops = &xdrstdio_ops;
	xdrs->x_private = (char *)stream;
}
/******************************************************************************
*
* xdrstdio - just flush any data
*
*/
static void xdrstdio_destroy(XDR *xdrs)
{
	(void)fflush((FILE *)xdrs->x_private);
}
/******************************************************************************
*
* xdrstdio_getbytes - read bytest from stream
*
*/
static bool_t xdrstdio_getbytes(xdrs, cp, len)
XDR *xdrs;
char *cp;
unsigned len;
{
	if ((len != 0) &&
		(fread(cp, len, 1, (FILE *)xdrs->x_private) != 1)) {
		return FALSE;
	}
	return TRUE;
}
/******************************************************************************
*
* xdrstdio_putbytes - write bytes to stream
*
*/
static bool_t xdrstdio_putbytes(xdrs, cp, len)
XDR *xdrs;
char *cp;
unsigned len;
{
	if ((len != 0) &&
		(fwrite(cp, (int)len, 1, (FILE *)xdrs->x_private) != 1)) {
		return FALSE;
	}
	return TRUE;
}
/******************************************************************************
*
* xdrstdio_getlong - read long and translate to intel
*
*/
static bool_t xdrstdio_getlong(xdrs, lp)
XDR *xdrs;
long *lp;
{
	char c[4], *cp = (char *)lp;

	if (!XDR_GETBYTES(xdrs, c, 4)) {
		return FALSE;
	}
	cp[0] = c[3];
	cp[1] = c[2];
	cp[2] = c[1];
	cp[3] = c[0];
	return TRUE;
}
/******************************************************************************
*
* xdrstdio_putlong - translate from intel and write long
*
*/
static bool_t xdrstdio_putlong(xdrs, lp)
XDR *xdrs;
long *lp;
{
	char c[4], *cp = (char *)lp;

	c[0] = cp[3];
	c[1] = cp[2];
	c[2] = cp[1];
	c[3] = cp[0];
	return XDR_PUTBYTES(xdrs, c, 4);
}
/******************************************************************************
*
* xdrstdio_getpos - dummy getpos
*
*/
static unsigned xdrstdio_getpos(XDR *xdrs)
{
	if(xdrs) { /* not useful if unsigned */
		return FALSE;
	}
	return FALSE;
}
/******************************************************************************
*
* xdrstdio_setpos - dummy setpos
*
*/
static bool_t xdrstdio_setpos(XDR *xdrs, unsigned pos)
{
	return ((fseek((FILE *)xdrs->x_private, (long)pos, 0) < 0) ?
		FALSE : TRUE);
}
/******************************************************************************
*
* xdrstdio_inline - dummy to prevent crash
*
*/
static long *xdrstdio_inline(XDR *xdrs, unsigned pos)
{
	if (pos || xdrs->x_op) { /* to prevent unused from compiler */
		return NULL;
	}
	return NULL;
}

/* Copyright 1993, Lawrence Berkeley Laboratory */

/* dosxdr.c - simple xdrstdio routines for dos */

/*
modification history
--------------------
24apr93,whg	written.
11feb95,whg	don't compile non INTEL
*/

/*
DESCRIPTION
xdr routines for intel based OS ...
*/
#ifdef _MSC_VER /* only if Microsoft compiler */
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include "asuAlloc.h"
#define DS_PRIVATE
#include "dsxdr.h"
/******************************************************************************
*
* define static used to create XDR structure for mem
*/
static bool_t xdrmem_getlong(XDR *xdrs, long *lp);
static bool_t xdrmem_putlong(XDR *xdrs, long *lp);
static bool_t xdrmem_getbytes(XDR *xdrs, char *cp, unsigned len);
static bool_t xdrmem_putbytes(XDR *xdrs, char *cp, unsigned len);
static unsigned xdrmem_getpos(XDR *xdrs);
static bool_t xdrmem_setpos(XDR *xdrs, unsigned pos);
static long *xdrmem_inline(XDR *xdrs, unsigned pos);
static void xdrmem_destroy(XDR *xdrs);

static struct xdr_ops xdrmem_ops = {
	xdrmem_getlong,
	xdrmem_putlong,
	xdrmem_getbytes,
	xdrmem_putbytes,
	xdrmem_getpos,
	xdrmem_setpos,
	xdrmem_inline,
	xdrmem_destroy,
};

/******************************************************************************
*
* define static used to create XDR structure for stdio
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
#ifndef WIN32 
/******************************************************************************
*/
long htonl(long l)
{
	char *cp = (char *)&l;
	union {char c[4]; long l;}rtn;

	if (DS_IS_BIG_ENDIAN) {
		return l;
	}
	rtn.c[0] = cp[3];
	rtn.c[1] = cp[2];
	rtn.c[2] = cp[1];
	rtn.c[3] = cp[0];
	return rtn.l;
}
/******************************************************************************
*/
long ntohl(long l)
{
	char *cp = (char *)&l;
	union {char c[4]; long l;}rtn;

	if (DS_IS_BIG_ENDIAN) {
		return l;
	}
	rtn.c[0] = cp[3];
	rtn.c[1] = cp[2];
	rtn.c[2] = cp[1];
	rtn.c[3] = cp[0];
	return rtn.l;
}
#endif
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
			*cpp = sp = (char *)MALLOC(nodesize);
		}
		if (sp == NULL) {
			dsErrorPrint("xdr_bytes: out of memory\n");
			return FALSE;
		}
		return xdr_opaque(xdrs, sp, nodesize);
	case XDR_ENCODE:
		return xdr_opaque(xdrs, sp, nodesize);
	case XDR_FREE:
		if (sp != NULL) {
			FREE(sp);
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
bool_t xdr_double(XDR *xdrs, double *dp)
{
	char c[8], *cp = (char *)dp;

	if (!DS_IS_IEEE_FLOAT) {
		return FALSE;
	}		
	if (xdrs->x_op == XDR_DECODE) {
		if (DS_IS_BIG_ENDIAN) {
			return XDR_GETBYTES(xdrs, (char *)dp, 8);
		}
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
		if (DS_IS_BIG_ENDIAN) {
			return XDR_PUTBYTES(xdrs, (char *)dp, 8);
		}
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
bool_t xdr_float(XDR *xdrs, float *fp)
{
	if (!DS_IS_IEEE_FLOAT) {
		return FALSE;
	}
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
bool_t xdr_int(XDR *xdrs, int *ip)
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
bool_t xdr_u_int(XDR *xdrs, unsigned *uip)
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
bool_t xdr_long(XDR *xdrs, long *lp)
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
bool_t xdr_u_long(XDR *xdrs, unsigned long *ulp)
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
			*cpp = sp = MALLOC((unsigned)size + 1);
		}
		if (sp == NULL) {
			dsErrorPrint("xdr_string out of memory\n");
			return FALSE;
		}
		sp[(unsigned)size] = '\0';
	/* fall into */
	case XDR_ENCODE:
		return xdr_opaque(xdrs, sp, (unsigned)size);
	case XDR_FREE:
		FREE(sp);
		*cpp = NULL;
		return TRUE;
	}
	return FALSE;
}
/******************************************************************************
*
* xdrmem_create - create xdr memory region
*
*/
void xdrmem_create(XDR *xdrs, char *addr, unsigned size, enum xdr_op op)
{
	xdrs->x_op = op;
	xdrs->x_ops = &xdrmem_ops;
	xdrs->x_private = xdrs->x_base = addr;
	xdrs->x_handy = size;
}
/******************************************************************************
*
*/
static void xdrmem_destroy(XDR *xdrs)
{
	xdrs->x_private = xdrs->x_base = NULL;
	xdrs->x_handy = 0;
}
/******************************************************************************
*
*/ 
static bool_t xdrmem_getlong(XDR *xdrs, long *lp)
{
	if (xdrs->x_handy < sizeof(long)) {
		return FALSE;
	}
	*lp = (long)ntohl((u_long)(*((long *)(xdrs->x_private))));
	xdrs->x_handy -= sizeof(long);
	xdrs->x_private += sizeof(long);
	return TRUE;
}
/******************************************************************************
*
*/ 
static bool_t xdrmem_putlong(XDR *xdrs, long *lp)
{
	if (xdrs->x_handy < sizeof(long)) {
		return FALSE;
	}
	*(long *)xdrs->x_private = (long)htonl((u_long)(*lp));
	xdrs->x_handy -= sizeof(long);
	xdrs->x_private += sizeof(long);
	return TRUE;
}
/******************************************************************************
*
*/ 
static bool_t xdrmem_getbytes(XDR *xdrs, caddr_t addr, u_int len)
{
	if (xdrs->x_handy < (int)len) {
		return FALSE;
	}
	memcpy(addr, xdrs->x_private, len);
	xdrs->x_handy -= len; 
	xdrs->x_private += len;
	return TRUE;
}
/******************************************************************************
*
*/ 
static bool_t xdrmem_putbytes(XDR *xdrs, caddr_t addr, u_int len)
{
	if (xdrs->x_handy < (int)len) {
		return FALSE;
	}
	memcpy(xdrs->x_private, addr, len);
	xdrs->x_handy -= len; 
	xdrs->x_private += len;
	return TRUE;
}
/******************************************************************************
*
*/ 
static u_int xdrmem_getpos(XDR *xdrs)
{
	return (u_int)(xdrs->x_private - xdrs->x_base);
}
/******************************************************************************
*
*/ 
static bool_t xdrmem_setpos(XDR *xdrs, u_int pos)
{
	caddr_t newaddr = xdrs->x_base + pos;
	caddr_t lastaddr = xdrs->x_private + xdrs->x_handy;

	if (newaddr > lastaddr) {
		return FALSE;
	}
	xdrs->x_private = newaddr;
	xdrs->x_handy = (int)(lastaddr - newaddr);
	return TRUE;
}
/******************************************************************************
*
*/ 
static long *xdrmem_inline(XDR *xdrs, unsigned len)
{
	long *buf = 0;

	if (xdrs->x_handy >= (int)len) {
		xdrs->x_handy -= len;
		buf = (long *) xdrs->x_private;
		xdrs->x_private += len;
	}
	return buf;
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
bool_t xdrstdio_getbytes(XDR *xdrs, char *cp, unsigned len)
{
 unsigned lenread; 
  if ( !len ) return TRUE;
/**VP		(fread(cp, len, 1, (FILE *)xdrs->x_private) != 1)) {**/
  lenread=fread(cp, len, 1, (FILE *)xdrs->x_private); 
  if (lenread == len)) return FALSE;
  if (!lenread) {
    printf("DSL: ***ERROR Unexpected EOF**\n"); 
  } else {
    printf("DSL: ***ERROR Try read %d but got only %d bytes***\n",len,lenread);}
  return TRUE;
}
/******************************************************************************
*
* xdrstdio_putbytes - write bytes to stream
*
*/
static bool_t xdrstdio_putbytes(XDR *xdrs, char *cp, unsigned len)
{
unsigned lenwrote;
  if (! len) return TRUE;
  lenwrote = fwrite(cp, (int)len, 1, (FILE *)xdrs->x_private);
  if (len == lenwrote) return FALSE;
  printf ("DSL: ***Error , tried to write %d but wrote %d ***\n",len,lenwrote);

  return TRUE;
}
/******************************************************************************
*
* xdrstdio_getlong - read long and translate to intel
*
*/
static bool_t xdrstdio_getlong(XDR *xdrs, long *lp)
{
	long l;

	if (!XDR_GETBYTES(xdrs, (char *)&l, sizeof(long))) {
		return FALSE;
	}
	*lp = ntohl(l); 
	return TRUE;
}
/******************************************************************************
*
* xdrstdio_putlong - translate from intel and write long
*
*/
static bool_t xdrstdio_putlong(XDR *xdrs, long *lp)
{
	long l;

	l = htonl(*lp);
	return XDR_PUTBYTES(xdrs, (char *)&l, sizeof(long));
}
/******************************************************************************
*
* xdrstdio_getpos - dummy getpos
*
*/
static unsigned xdrstdio_getpos(XDR *xdrs)
{
	return (unsigned)ftell((FILE *)xdrs->x_private);
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
#endif /* _MSC_VER only if NT or DOS */

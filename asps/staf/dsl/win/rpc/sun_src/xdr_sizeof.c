/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 * 
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */
/*
 * xdr_sizeof.c
 *
 * Copyright 1990 Sun Microsystems, Inc.
 *
 * General purpose routine to see how much space something will use
 * when serialized using XDR.
 */

#include <rpc/types.h>
#include <rpc/xdr.h>
#include <sys/types.h>
#include <rpc/trace.h>

/* ARGSUSED */
static bool_t
x_putlong(xdrs, longp)
	XDR *xdrs;
	long *longp;
{
	trace1(TR_x_putlong, 0);

	xdrs->x_handy += BYTES_PER_XDR_UNIT;
	trace1(TR_x_putlong, 1);
	return (TRUE);
}

/* ARGSUSED */
static bool_t
x_putbytes(xdrs, bp, len)
	XDR *xdrs;
	char  *bp;
	int len;
{
	trace2(TR_x_putbytes, 0, len);
	xdrs->x_handy += len;
	trace2(TR_x_putbytes, 1, len);

	return (TRUE);
}

static u_int
x_getpostn(xdrs)
	XDR *xdrs;
{
	trace1(TR_x_getpostn, 0);
	trace1(TR_x_getpostn, 1);
	return (xdrs->x_handy);
}

/* ARGSUSED */
static bool_t
x_setpostn(xdrs, pos)
	XDR *xdrs;
	u_int pos;
{
	/* This is not allowed */
	trace2(TR_x_setpostn, 0, pos);
	trace2(TR_x_setpostn, 1, pos);
	return (FALSE);
}

static long *
x_inline(xdrs, len)
	XDR *xdrs;
	int len;
{
	trace2(TR_x_inline, 0, len);
	if (len == 0) {
		trace2(TR_x_inline, 1, len);
		return (NULL);
	}
	if (xdrs->x_op != XDR_ENCODE) {
		trace2(TR_x_inline, 1, len);
		return (NULL);
	}
	if (len < (int) xdrs->x_base) {
		/* x_private was already allocated */
		xdrs->x_handy += len;
		trace2(TR_x_inline, 1, len);
		return ((long *) xdrs->x_private);
	} else {
		/* Free the earlier space and allocate new area */
		if (xdrs->x_private)
			free(xdrs->x_private);
		if ((xdrs->x_private = (caddr_t) malloc(len)) == NULL) {
			xdrs->x_base = 0;
			trace2(TR_x_inline, 1, len);
			return (NULL);
		}
		xdrs->x_base = (caddr_t) len;
		xdrs->x_handy += len;
		trace2(TR_x_inline, 1, len);
		return ((long *) xdrs->x_private);
	}
}

static
harmless()
{
	/* Always return FALSE/NULL, as the case may be */
	trace1(TR_harmless, 0);
	trace1(TR_harmless, 1);
	return (0);
}

static void
x_destroy(xdrs)
	XDR *xdrs;
{
	trace1(TR_x_destroy, 0);
	xdrs->x_handy = 0;
	xdrs->x_base = 0;
	if (xdrs->x_private) {
		free(xdrs->x_private);
		xdrs->x_private = NULL;
	}
	trace1(TR_x_destroy, 1);
	return;
}

unsigned long
xdr_sizeof(func, data)
	xdrproc_t func;
	void *data;
{
	XDR x;
	struct xdr_ops ops;
	bool_t stat;
	/* to stop ANSI-C compiler from complaining */
	typedef  bool_t (* dummyfunc1)(XDR *, long *);
	typedef  bool_t (* dummyfunc2)(XDR *, caddr_t, int);

	trace1(TR_xdr_sizeof, 0);
	ops.x_putlong = x_putlong;
	ops.x_putbytes = x_putbytes;
	ops.x_inline = x_inline;
	ops.x_getpostn = x_getpostn;
	ops.x_setpostn = x_setpostn;
	ops.x_destroy = x_destroy;

	/* the other harmless ones */
	ops.x_getlong =  (dummyfunc1) harmless;
	ops.x_getbytes = (dummyfunc2) harmless;

	x.x_op = XDR_ENCODE;
	x.x_ops = &ops;
	x.x_handy = 0;
	x.x_private = (caddr_t) NULL;
	x.x_base = (caddr_t) 0;

	stat = func(&x, data);
	if (x.x_private)
		free(x.x_private);
	trace1(TR_xdr_sizeof, 1);
	return (stat == TRUE ? (unsigned) x.x_handy: 0);
}

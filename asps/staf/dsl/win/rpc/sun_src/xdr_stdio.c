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
 * Copyright (c) 1984 - 1991 by Sun Microsystems, Inc.
 */

#pragma ident	"@(#)xdr_stdio.c	1.15	94/04/24 SMI"

#if !defined(lint) && defined(SCCSIDS)
static char sccsid[] = "@(#)xdr_stdio.c	1.15	94/04/24 SMI";
#endif

/*
 * xdr_stdio.c, XDR implementation on standard i/o file.
 *
 * This set of routines implements a XDR on a stdio stream.
 * XDR_ENCODE serializes onto the stream, XDR_DECODE de-serializes
 * from the stream.
 */

#include "rpc_mt.h"
#include <rpc/types.h>
#include <stdio.h>
#include <rpc/xdr.h>
#include <sys/types.h>
#include <rpc/trace.h>

static struct xdr_ops *xdrstdio_ops();

/*
 * Initialize a stdio xdr stream.
 * Sets the xdr stream handle xdrs for use on the stream file.
 * Operation flag is set to op.
 */
void
xdrstdio_create(xdrs, file, op)
	register XDR *xdrs;
	FILE *file;
	enum xdr_op op;
{
	trace1(TR_xdrstdio_create, 0);
	xdrs->x_op = op;
	xdrs->x_ops = xdrstdio_ops();
	xdrs->x_private = (caddr_t)file;
	xdrs->x_handy = 0;
	xdrs->x_base = 0;
	trace1(TR_xdrstdio_create, 1);
}

/*
 * Destroy a stdio xdr stream.
 * Cleans up the xdr stream handle xdrs previously set up by xdrstdio_create.
 */
static void
xdrstdio_destroy(xdrs)
	register XDR *xdrs;
{
	trace1(TR_xdrstdio_destroy, 0);
	(void) fflush((FILE *)xdrs->x_private);
	/* xx should we close the file ?? */
	trace1(TR_xdrstdio_destroy, 1);
}

static bool_t
xdrstdio_getlong(xdrs, lp)
	XDR *xdrs;
	register long *lp;
{
	trace1(TR_xdrstdio_getlong, 0);
	if (fread((caddr_t)lp, sizeof (long), 1,
			(FILE *)xdrs->x_private) != 1) {
		trace1(TR_xdrstdio_getlong, 1);
		return (FALSE);
	}
#ifndef mc68000
	*lp = ntohl(*lp);
#endif
	trace1(TR_xdrstdio_getlong, 1);
	return (TRUE);
}

static bool_t
xdrstdio_putlong(xdrs, lp)
	XDR *xdrs;
	long *lp;
{

#ifndef mc68000
	long mycopy = htonl(*lp);
	lp = &mycopy;
#endif
	trace1(TR_xdrstdio_putlong, 0);
	if (fwrite((caddr_t)lp, sizeof (long), 1,
			(FILE *)xdrs->x_private) != 1) {
		trace1(TR_xdrstdio_putlong, 1);
		return (FALSE);
	}
	trace1(TR_xdrstdio_putlong, 1);
	return (TRUE);
}

static bool_t
xdrstdio_getbytes(xdrs, addr, len)
	XDR *xdrs;
	caddr_t addr;
	int len;
{
	trace2(TR_xdrstdio_getbytes, 0, len);
	if ((len != 0) &&
		(fread(addr, (int)len, 1, (FILE *)xdrs->x_private) != 1)) {
		trace1(TR_xdrstdio_getbytes, 1);
		return (FALSE);
	}
	trace1(TR_xdrstdio_getbytes, 1);
	return (TRUE);
}

static bool_t
xdrstdio_putbytes(xdrs, addr, len)
	XDR *xdrs;
	caddr_t addr;
	int len;
{
	trace2(TR_xdrstdio_putbytes, 0, len);
	if ((len != 0) &&
		(fwrite(addr, (int)len, 1, (FILE *)xdrs->x_private) != 1)) {
		trace1(TR_xdrstdio_putbytes, 1);
		return (FALSE);
	}
	trace1(TR_xdrstdio_putbytes, 1);
	return (TRUE);
}

static u_int
xdrstdio_getpos(xdrs)
	XDR *xdrs;
{
	u_int dummy1;

	trace1(TR_xdrstdio_getpos, 0);
	dummy1 = (u_int) ftell((FILE *)xdrs->x_private);
	trace1(TR_xdrstdio_getpos, 1);
	return (dummy1);
}

static bool_t
xdrstdio_setpos(xdrs, pos)
	XDR *xdrs;
	u_int pos;
{
	bool_t dummy2;

	trace2(TR_xdrstdio_setpos, 0, pos);
	dummy2 = (fseek((FILE *)xdrs->x_private,
			(long)pos, 0) < 0) ? FALSE : TRUE;
	trace1(TR_xdrstdio_setpos, 1);
	return (dummy2);
}

static long *
xdrstdio_inline(xdrs, len)
	XDR *xdrs;
	int len;
{

	/*
	 * Must do some work to implement this: must insure
	 * enough data in the underlying stdio buffer,
	 * that the buffer is aligned so that we can indirect through a
	 * long *, and stuff this pointer in xdrs->x_buf.  Doing
	 * a fread or fwrite to a scratch buffer would defeat
	 * most of the gains to be had here and require storage
	 * management on this buffer, so we don't do this.
	 */
	trace2(TR_xdrstdio_inline, 0, len);
	trace2(TR_xdrstdio_inline, 1, len);
	return (NULL);
}

static bool_t
xdrstdio_control(xdrs, request, info)
	XDR *xdrs;
	int request;
	void *info;
{
	switch (request) {

	default:
		return (FALSE);
	}
}

static struct xdr_ops *
xdrstdio_ops()
{
	static struct xdr_ops ops;
	extern mutex_t	ops_lock;

/* VARIABLES PROTECTED BY ops_lock: ops */

	trace1(TR_xdrstdio_ops, 0);
	mutex_lock(&ops_lock);
	if (ops.x_getlong == NULL) {
		ops.x_getlong = xdrstdio_getlong;
		ops.x_putlong = xdrstdio_putlong;
		ops.x_getbytes = xdrstdio_getbytes;
		ops.x_putbytes = xdrstdio_putbytes;
		ops.x_getpostn = xdrstdio_getpos;
		ops.x_setpostn = xdrstdio_setpos;
		ops.x_inline = xdrstdio_inline;
		ops.x_destroy = xdrstdio_destroy;
		ops.x_control = xdrstdio_control;
	}
	mutex_unlock(&ops_lock);
	trace1(TR_xdrstdio_ops, 1);
	return (&ops);
}

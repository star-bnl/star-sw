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

#pragma ident	"@(#)xdr_mem.c	1.15	94/04/24 SMI"

#if !defined(lint) && defined(SCCSIDS)
static char sccsid[] = "@(#)xdr_mem.c	1.15	94/04/24 SMI";
#endif

/*
 * xdr_mem.h, XDR implementation using memory buffers.
 *
 * If you have some data to be interpreted as external data representation
 * or to be converted to external data representation in a memory buffer,
 * then this is the package for you.
 *
 */

#ifdef KERNEL
#include <sys/param.h>
#endif

#include "rpc_mt.h"
#include <sys/types.h>
#include <rpc/trace.h>
#include <rpc/types.h>
#include <rpc/xdr.h>
#include <memory.h>

static struct xdr_ops *xdrmem_ops();

/*
 * Meaning of the private areas of the xdr struct for xdr_mem
 * 	x_base : Base from where the xdr stream starts
 * 	x_private : The current position of the stream.
 * 	x_handy : The size of the stream buffer.
 */

/*
 * The procedure xdrmem_create initializes a stream descriptor for a
 * memory buffer.
 */
void
xdrmem_create(xdrs, addr, size, op)
	register XDR *xdrs;
	caddr_t addr;
	u_int size;
	enum xdr_op op;
{
	trace2(TR_xdrmem_create, 0, size);
	xdrs->x_op = op;
	xdrs->x_ops = xdrmem_ops();
	xdrs->x_private = xdrs->x_base = addr;
	xdrs->x_handy = size;
	trace2(TR_xdrmem_create, 1, size);
}

static void
xdrmem_destroy(xdrs)
	XDR *xdrs;
{
	trace1(TR_xdrmem_destroy, 0);
	trace1(TR_xdrmem_destroy, 1);
}

static bool_t
xdrmem_getlong(xdrs, lp)
	register XDR *xdrs;
	long *lp;
{
	trace1(TR_xdrmem_getlong, 0);
	if ((xdrs->x_handy -= sizeof (long)) < 0) {
		trace1(TR_xdrmem_getlong, 1);
		return (FALSE);
	}
	*lp = (long)ntohl((u_long)(*((long *)(xdrs->x_private))));
	xdrs->x_private += sizeof (long);
	trace1(TR_xdrmem_getlong, 1);
	return (TRUE);
}

static bool_t
xdrmem_putlong(xdrs, lp)
	register XDR *xdrs;
	long *lp;
{
	trace1(TR_xdrmem_putlong, 0);
	if ((xdrs->x_handy -= sizeof (long)) < 0) {
		trace1(TR_xdrmem_putlong, 1);
		return (FALSE);
	}
	*(long *)xdrs->x_private = (long)htonl((u_long)(*lp));
	xdrs->x_private += sizeof (long);
	trace1(TR_xdrmem_putlong, 1);
	return (TRUE);
}

static bool_t
xdrmem_getbytes(xdrs, addr, len)
	register XDR *xdrs;
	caddr_t addr;
	register int len;
{
	trace2(TR_xdrmem_getbytes, 0, len);
	if ((xdrs->x_handy -= len) < 0) {
		trace1(TR_xdrmem_getbytes, 1);
		return (FALSE);
	}
	(void) memcpy(addr, xdrs->x_private, len);
	xdrs->x_private += len;
	trace1(TR_xdrmem_getbytes, 1);
	return (TRUE);
}

static bool_t
xdrmem_putbytes(xdrs, addr, len)
	register XDR *xdrs;
	caddr_t addr;
	register int len;
{
	trace2(TR_xdrmem_putbytes, 0, len);
	if ((xdrs->x_handy -= len) < 0) {
		trace1(TR_xdrmem_putbytes, 1);
		return (FALSE);
	}
	(void) memcpy(xdrs->x_private, addr, len);
	xdrs->x_private += len;
	trace1(TR_xdrmem_putbytes, 1);
	return (TRUE);
}

static u_int
xdrmem_getpos(xdrs)
	register XDR *xdrs;
{
	trace1(TR_xdrmem_getpos, 0);
	trace1(TR_xdrmem_getpos, 1);
	return ((u_int)xdrs->x_private - (u_int)xdrs->x_base);
}

static bool_t
xdrmem_setpos(xdrs, pos)
	register XDR *xdrs;
	u_int pos;
{
	register caddr_t newaddr = xdrs->x_base + pos;
	register caddr_t lastaddr = xdrs->x_private + xdrs->x_handy;

	trace2(TR_xdrmem_setpos, 0, pos);
	if ((long)newaddr > (long)lastaddr) {
		trace1(TR_xdrmem_setpos, 1);
		return (FALSE);
	}
	xdrs->x_private = newaddr;
	xdrs->x_handy = (int)lastaddr - (int)newaddr;
	trace1(TR_xdrmem_setpos, 1);
	return (TRUE);
}

static long *
xdrmem_inline(xdrs, len)
	register XDR *xdrs;
	int len;
{
	long *buf = 0;

	trace2(TR_xdrmem_inline, 0, len);
	if (xdrs->x_handy >= len) {
		xdrs->x_handy -= len;
		buf = (long *) xdrs->x_private;
		xdrs->x_private += len;
	}
	trace2(TR_xdrmem_inline, 1, len);
	return (buf);
}

static bool_t
xdrmem_control(xdrs, request, info)
	XDR *xdrs;
	int request;
	void *info;
{
	xdr_bytesrec *xptr;

	switch (request) {

	case XDR_GET_BYTES_AVAIL:
		xptr = (xdr_bytesrec *) info;
		xptr->xc_is_last_record = TRUE;
		xptr->xc_num_avail = xdrs->x_handy;
		return (TRUE);
	default:
		return (FALSE);

	}

}

static struct xdr_ops *
xdrmem_ops()
{
	static struct xdr_ops ops;
	extern mutex_t	ops_lock;

/* VARIABLES PROTECTED BY ops_lock: ops */


	trace1(TR_xdrmem_ops, 0);
	mutex_lock(&ops_lock);
	if (ops.x_getlong == NULL) {
		ops.x_getlong = xdrmem_getlong;
		ops.x_putlong = xdrmem_putlong;
		ops.x_getbytes = xdrmem_getbytes;
		ops.x_putbytes = xdrmem_putbytes;
		ops.x_getpostn = xdrmem_getpos;
		ops.x_setpostn = xdrmem_setpos;
		ops.x_inline = xdrmem_inline;
		ops.x_destroy = xdrmem_destroy;
		ops.x_control = xdrmem_control;
	}
	mutex_unlock(&ops_lock);
	trace1(TR_xdrmem_ops, 1);
	return (&ops);
}

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
 * Copyright (c) 1986 - 1991 by Sun Microsystems, Inc.
 */

#pragma ident	"@(#)xdr_refer.c	1.10	93/07/05 SMI" 

/*
 * xdr_refer.c, Generic XDR routines impelmentation.
 *
 * These are the "non-trivial" xdr primitives used to serialize and de-serialize
 * "pointers".  See xdr.h for more info on the interface to xdr.
 */
#include <sys/types.h>
#include <rpc/trace.h>
#ifdef KERNEL
#include <sys/param.h>
#else
#include <sys/syslog.h>
#include <stdio.h>
#endif
#include <rpc/types.h>
#include <rpc/xdr.h>
#include <memory.h>

#define	LASTUNSIGNED	((u_int)0-1)
char mem_err_msg_ref[] = "xdr_reference: out of memory";

/*
 * XDR an indirect pointer
 * xdr_reference is for recursively translating a structure that is
 * referenced by a pointer inside the structure that is currently being
 * translated.  pp references a pointer to storage. If *pp is null
 * the  necessary storage is allocated.
 * size is the sizeof the referneced structure.
 * proc is the routine to handle the referenced structure.
 */
bool_t
xdr_reference(xdrs, pp, size, proc)
	register XDR *xdrs;
	caddr_t *pp;		/* the pointer to work on */
	u_int size;		/* size of the object pointed to */
	xdrproc_t proc;		/* xdr routine to handle the object */
{
	register caddr_t loc = *pp;
	register bool_t stat;

	trace2(TR_xdr_reference, 0, size);
	if (loc == NULL)
		switch (xdrs->x_op) {
		case XDR_FREE:
			trace1(TR_xdr_reference, 1);
			return (TRUE);

		case XDR_DECODE:
			*pp = loc = (caddr_t) mem_alloc(size);
#ifndef KERNEL
			if (loc == NULL) {
				(void) syslog(LOG_ERR, mem_err_msg_ref);

				trace1(TR_xdr_reference, 1);
				return (FALSE);
			}
			(void) memset(loc, 0, (int)size);
#else
			(void) memset(loc, 0, size);
#endif
			break;
	}

	stat = (*proc)(xdrs, loc, LASTUNSIGNED);

	if (xdrs->x_op == XDR_FREE) {
		mem_free(loc, size);
		*pp = NULL;
	}
	trace1(TR_xdr_reference, 1);
	return (stat);
}


#ifndef KERNEL
/*
 * xdr_pointer():
 *
 * XDR a pointer to a possibly recursive data structure. This
 * differs with xdr_reference in that it can serialize/deserialiaze
 * trees correctly.
 *
 *  What's sent is actually a union:
 *
 *  union object_pointer switch (boolean b) {
 *  case TRUE: object_data data;
 *  case FALSE: void nothing;
 *  }
 *
 * > objpp: Pointer to the pointer to the object.
 * > obj_size: size of the object.
 * > xdr_obj: routine to XDR an object.
 *
 */
bool_t
xdr_pointer(xdrs, objpp, obj_size, xdr_obj)
	register XDR *xdrs;
	char **objpp;
	u_int obj_size;
	xdrproc_t xdr_obj;
{
	bool_t more_data;
	bool_t dummy;

	trace2(TR_xdr_pointer, 0, obj_size);
	more_data = (*objpp != NULL);
	if (! xdr_bool(xdrs, &more_data)) {
		trace1(TR_xdr_pointer, 1);
		return (FALSE);
	}
	if (! more_data) {
		*objpp = NULL;
		trace1(TR_xdr_pointer, 1);
		return (TRUE);
	}
	dummy = xdr_reference(xdrs, objpp, obj_size, xdr_obj);
	trace1(TR_xdr_pointer, 1);
	return (dummy);
}
#endif /* ! KERNEL */

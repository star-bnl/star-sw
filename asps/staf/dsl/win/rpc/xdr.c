/* WHG 07apr98 Modified for use in win32 DSL */
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
 * Copyright (c) 1986-1991 by Sun Microsystems Inc.
 */

/* WHG #pragma ident	"@(#)xdr.c	1.16	94/04/24 SMI" */

#if !defined(lint) && defined(SCCSIDS)
static char sccsid[] = "@(#)xdr.c 1.44 89/02/28";
#endif

/*
 * xdr.c, Generic XDR routines implementation.
 *
 * These are the "generic" xdr routines used to serialize and de-serialize
 * most common data items.  See xdr.h for more info on the interface to
 * xdr.
 */
#include <sys/types.h>
#include <rpc/trace.h>

#ifdef KERNEL
#include <sys/param.h>
#include <sys/systm.h>
#else
/* WHG #include <sys/syslog.h> */
#include <stdio.h>
#endif

#include <limits.h>
#include <rpc/types.h>
#include <rpc/xdr.h>


/*
 * constants specific to the xdr "protocol"
 */
#define	XDR_FALSE	((long) 0)
#define	XDR_TRUE	((long) 1)
#define	LASTUNSIGNED	((u_int) 0-1)

/*
 * for unit alignment
 */
static const char xdr_zero[BYTES_PER_XDR_UNIT];

/*
 * MACRO definitions for the more commonly used XDR_routines
 */
#define	XDR_LONG(xdrs, lp)	\
	((xdrs->x_op == XDR_ENCODE) ? XDR_PUTLONG(xdrs, lp) : \
	(xdrs->x_op == XDR_DECODE) ? XDR_GETLONG(xdrs, lp) : \
	(xdrs->x_op == XDR_FREE) ? TRUE : FALSE)

#define	XDR_U_LONG(xdrs, ulp)	XDR_LONG(xdrs, (long *) ulp)
#define	XDR_INT(xdrs, ip)	((sizeof (int) == sizeof (long)) ? \
	XDR_LONG(xdrs, (long *)ip) : xdr_short(xdrs, (short *)ip))

#define	XDR_U_INT(xdrs, ip)	((sizeof (int) == sizeof (long)) ? \
	XDR_U_LONG(xdrs, (u_long *)ip) : xdr_u_short(xdrs, (u_short *)ip))

#ifndef KERNEL
/*
 * Free a data structure using XDR
 * Not a filter, but a convenient utility nonetheless
 */
void
xdr_free(proc, objp)
	xdrproc_t proc;
	char *objp;
{
	XDR x;

	trace1(TR_xdr_free, 0);
	x.x_op = XDR_FREE;
	(*proc)(&x, objp);
	trace1(TR_xdr_free, 1);
}
#endif

/*
 * XDR nothing
 */
bool_t
xdr_void(/* xdrs, addr */)
	/* XDR *xdrs; */
	/* caddr_t addr; */
{
	trace1(TR_xdr_void, 0);
	trace1(TR_xdr_void, 1);
	return (TRUE);
}

/*
 * XDR integers
 */
bool_t
xdr_int(xdrs, ip)
	XDR *xdrs;
	int *ip;
{
	bool_t dummy;

	trace1(TR_xdr_int, 0);
#ifdef lint
	(void) (xdr_short(xdrs, (short *)ip));
	dummy = xdr_long(xdrs, (long *)ip);
	trace1(TR_xdr_int, 1);
	return (dummy);
#else
	dummy = XDR_INT(xdrs, ip);
	trace1(TR_xdr_int, 1);
	return (dummy);
#endif
}

/*
 * XDR unsigned integers
 */
bool_t
xdr_u_int(xdrs, up)
	XDR *xdrs;
	u_int *up;
{
	bool_t dummy;

	trace1(TR_xdr_u_int, 0);
#ifdef lint
	(void) (xdr_u_short(xdrs, (u_short *)up));
	dummy = xdr_u_long(xdrs, (u_long *)up);
	trace1(TR_xdr_u_int, 1);
	return (dummy);
#else
	dummy = XDR_U_INT(xdrs, up);
	trace1(TR_xdr_u_int, 1);
	return (dummy);
#endif
}

/*
 * XDR long integers
 * same as xdr_u_long
 */
bool_t
xdr_long(xdrs, lp)
	register XDR *xdrs;
	long *lp;
{
	bool_t dummy;

	trace1(TR_xdr_long, 0);
	dummy = XDR_LONG(xdrs, lp);
	trace1(TR_xdr_long, 1);
	return (dummy);
}

/*
 * XDR unsigned long integers
 * same as xdr_long
 */
bool_t
xdr_u_long(xdrs, ulp)
	register XDR *xdrs;
	u_long *ulp;
{
	bool_t dummy;

	trace1(TR_xdr_u_long, 0);
	dummy = XDR_U_LONG(xdrs, ulp);
	trace1(TR_xdr_u_long, 1);
	return (dummy);
}

/*
 * XDR short integers
 */
bool_t
xdr_short(xdrs, sp)
	register XDR *xdrs;
	short *sp;
{
	long l;
	bool_t dummy;

	trace1(TR_xdr_short, 0);
	switch (xdrs->x_op) {

	case XDR_ENCODE:
		l = (long) *sp;
		dummy = XDR_PUTLONG(xdrs, &l);
		trace1(TR_xdr_short, 1);
		return (dummy);

	case XDR_DECODE:
		if (!XDR_GETLONG(xdrs, &l)) {
			trace1(TR_xdr_short, 1);
			return (FALSE);
		}
		*sp = (short) l;
		trace1(TR_xdr_short, 1);
		return (TRUE);

	case XDR_FREE:
		trace1(TR_xdr_short, 1);
		return (TRUE);
	}
	trace1(TR_xdr_short, 1);
	return (FALSE);
}

/*
 * XDR unsigned short integers
 */
bool_t
xdr_u_short(xdrs, usp)
	register XDR *xdrs;
	u_short *usp;
{
	u_long l;
	bool_t dummy;


	trace1(TR_xdr_u_short, 0);
	switch (xdrs->x_op) {

	case XDR_ENCODE:
		l = (u_long) *usp;
		dummy = XDR_PUTLONG(xdrs, (long *)&l);
		trace1(TR_xdr_u_short, 1);
		return (dummy);

	case XDR_DECODE:
		if (!XDR_GETLONG(xdrs, (long *)&l)) {
#ifdef KERNEL
			printf("xdr_u_short: decode FAILED\n");
#endif
			trace1(TR_xdr_u_short, 1);
			return (FALSE);
		}
		*usp = (u_short) l;
		trace1(TR_xdr_u_short, 1);
		return (TRUE);

	case XDR_FREE:
		trace1(TR_xdr_u_short, 1);
		return (TRUE);
	}
#ifdef KERNEL
	printf("xdr_u_short: bad op FAILED\n");
#endif
	trace1(TR_xdr_u_short, 1);
	return (FALSE);
}


/*
 * XDR a char
 */
bool_t
xdr_char(xdrs, cp)
	XDR *xdrs;
	char *cp;
{
	int i;

	trace1(TR_xdr_char, 0);

	if (xdrs->x_op == XDR_ENCODE)
		i = (*cp);

	if (! XDR_INT(xdrs, &i)) {
		trace1(TR_xdr_char, 1);
		return (FALSE);
	}
	if (xdrs->x_op == XDR_DECODE)
		*cp = i;
	trace1(TR_xdr_char, 1);
	return (TRUE);
}

#ifndef KERNEL
/*
 * XDR an unsigned char
 */
bool_t
xdr_u_char(xdrs, cp)
	XDR *xdrs;
	u_char *cp;
{
	u_int u;

	trace1(TR_xdr_u_char, 0);
	if (xdrs->x_op == XDR_ENCODE)
		u = (*cp);
	if (! XDR_U_INT(xdrs, &u)) {
		trace1(TR_xdr_u_char, 1);
		return (FALSE);
	}
	if (xdrs->x_op == XDR_DECODE)
		*cp = u;
	trace1(TR_xdr_u_char, 1);
	return (TRUE);
}
#endif /* !KERNEL */

/*
 * XDR booleans
 */
bool_t
xdr_bool(xdrs, bp)
	register XDR *xdrs;
	bool_t *bp;
{
	long lb;
	bool_t dummy;

	trace1(TR_xdr_bool, 0);
	switch (xdrs->x_op) {

	case XDR_ENCODE:
		lb = *bp ? XDR_TRUE : XDR_FALSE;
		dummy = XDR_PUTLONG(xdrs, &lb);
		trace1(TR_xdr_bool, 1);
		return (dummy);

	case XDR_DECODE:
		if (!XDR_GETLONG(xdrs, &lb)) {
#ifdef KERNEL
			printf("xdr_bool: decode FAILED\n");
#endif
			trace1(TR_xdr_bool, 1);
			return (FALSE);
		}
		*bp = (lb == XDR_FALSE) ? FALSE : TRUE;
		trace1(TR_xdr_bool, 1);
		return (TRUE);

	case XDR_FREE:
		trace1(TR_xdr_bool, 1);
		return (TRUE);
	}
#ifdef KERNEL
	printf("xdr_bool: bad op FAILED\n");
#endif
	trace1(TR_xdr_bool, 1);
	return (FALSE);
}

/*
 * XDR enumerations
 */
bool_t
xdr_enum(xdrs, ep)
	XDR *xdrs;
	enum_t *ep;
{
	bool_t dummy;

#ifndef lint
	enum sizecheck { SIZEVAL };	/* used to find the size of an enum */

	/*
	 * enums are treated as ints
	 */
	trace1(TR_xdr_enum, 0);
	if (sizeof (enum sizecheck) == sizeof (long)) {
		dummy = XDR_LONG(xdrs, (long *)ep);
		trace1(TR_xdr_enum, 1);
		return (dummy);
	} else if (sizeof (enum sizecheck) == sizeof (short)) {
		dummy = xdr_short(xdrs, (short *)ep);
		trace1(TR_xdr_enum, 1);
		return (dummy);
	} else if (sizeof (enum sizecheck) == sizeof (char)) {
		dummy = xdr_char(xdrs, (char *)ep);
		trace1(TR_xdr_enum, 1);
		return (dummy);
	} else {
		trace1(TR_xdr_enum, 1);
		return (FALSE);
	}
#else
	trace1(TR_xdr_enum, 0);
	(void) (xdr_char(xdrs, (char *)ep));
	(void) (xdr_short(xdrs, (short *)ep));
	dummy = xdr_long(xdrs, (long *)ep);
	trace1(TR_xdr_enum, 1);
	return (dummy);
#endif
}

/*
 * XDR opaque data
 * Allows the specification of a fixed size sequence of opaque bytes.
 * cp points to the opaque object and cnt gives the byte length.
 */
bool_t
xdr_opaque(xdrs, cp, cnt)
	register XDR *xdrs;
	caddr_t cp;
	register u_int cnt;
{
	bool_t dummy;
	register u_int rndup;
	char crud[BYTES_PER_XDR_UNIT];

	/*
	 * if no data we are done
	 */
	trace2(TR_xdr_opaque, 0, cnt);
	if (cnt == 0) {
		trace1(TR_xdr_opaque, 1);
		return (TRUE);
	}

	/*
	 * round byte count to full xdr units
	 */
	rndup = cnt % BYTES_PER_XDR_UNIT;
	if ((int) rndup > 0)
		rndup = BYTES_PER_XDR_UNIT - rndup;

	if (xdrs->x_op == XDR_DECODE) {
		if (!XDR_GETBYTES(xdrs, cp, cnt)) {
#ifdef KERNEL
			printf("xdr_opaque: decode FAILED\n");
#endif
			trace1(TR_xdr_opaque, 1);
			return (FALSE);
		}
		if (rndup == 0) {
			trace1(TR_xdr_opaque, 1);
			return (TRUE);
		}
		dummy = XDR_GETBYTES(xdrs, crud, rndup);
		trace1(TR_xdr_opaque, 1);
		return (dummy);
	}

	if (xdrs->x_op == XDR_ENCODE) {

		if (!XDR_PUTBYTES(xdrs, cp, cnt)) {
#ifdef KERNEL
			printf("xdr_opaque: encode FAILED\n");
#endif
			trace1(TR_xdr_opaque, 1);
			return (FALSE);
		}
		if (rndup == 0) {
			trace1(TR_xdr_opaque, 1);
			return (TRUE);
		}
		dummy = XDR_PUTBYTES(xdrs, (caddr_t) &xdr_zero[0], rndup);
		trace1(TR_xdr_opaque, 1);
		return (dummy);
	}

	if (xdrs->x_op == XDR_FREE) {
		trace1(TR_xdr_opaque, 1);
		return (TRUE);
	}

#ifdef KERNEL
	printf("xdr_opaque: bad op FAILED\n");
#endif
	trace1(TR_xdr_opaque, 1);
	return (FALSE);
}

/*
 * XDR counted bytes
 * *cpp is a pointer to the bytes, *sizep is the count.
 * If *cpp is NULL maxsize bytes are allocated
 */

#ifndef KERNEL
static const char xdr_err[] = "xdr_%s: out of memory";
#endif

bool_t
xdr_bytes(xdrs, cpp, sizep, maxsize)
	register XDR *xdrs;
	char **cpp;
	register u_int *sizep;
	u_int maxsize;
{
	bool_t dummy;
	register char *sp = *cpp;  /* sp is the actual string pointer */
	register u_int nodesize;

	/*
	 * first deal with the length since xdr bytes are counted
	 * We decided not to use MACRO XDR_U_INT here, because the
	 * advantages here will be miniscule compared to xdr_bytes.
	 * This saved us 100 bytes in the library size.
	 */
	trace2(TR_xdr_bytes, 0, maxsize);
	if (! xdr_u_int(xdrs, sizep)) {
#ifdef KERNEL
		printf("xdr_bytes: size FAILED\n");
#endif
		trace1(TR_xdr_bytes, 1);
		return (FALSE);
	}
	nodesize = *sizep;
	if ((nodesize > maxsize) && (xdrs->x_op != XDR_FREE)) {
#ifdef KERNEL
		printf("xdr_bytes: bad size FAILED\n");
#endif
		trace1(TR_xdr_bytes, 1);
		return (FALSE);
	}

	/*
	 * now deal with the actual bytes
	 */
	switch (xdrs->x_op) {

	case XDR_DECODE:
		if (nodesize == 0) {
			trace1(TR_xdr_bytes, 1);
			return (TRUE);
		}
		if (sp == NULL) {
			*cpp = sp = (char *)mem_alloc(nodesize);
		}
#ifndef KERNEL
		if (sp == NULL) {
/* WHG		(void) syslog(LOG_ERR, xdr_err, (const char *) "bytes"); */
			WHG_MALLOC_ERR("xdr_bytes");
			trace1(TR_xdr_bytes, 1);
			return (FALSE);
		}
#endif
		/* fall into ... */

	case XDR_ENCODE:
		dummy = xdr_opaque(xdrs, sp, nodesize);
		trace1(TR_xdr_bytes, 1);
		return (dummy);

	case XDR_FREE:
		if (sp != NULL) {
			mem_free(sp, nodesize);
			*cpp = NULL;
		}
		trace1(TR_xdr_bytes, 1);
		return (TRUE);
	}
#ifdef KERNEL
	printf("xdr_bytes: bad op FAILED\n");
#endif
	trace1(TR_xdr_bytes, 1);
	return (FALSE);
}

/*
 * Implemented here due to commonality of the object.
 */
bool_t
xdr_netobj(xdrs, np)
	XDR *xdrs;
	struct netobj *np;
{
	bool_t dummy;

	trace1(TR_xdr_netobj, 0);
	dummy = xdr_bytes(xdrs, &np->n_bytes, &np->n_len, MAX_NETOBJ_SZ);
	trace1(TR_xdr_netobj, 1);
	return (dummy);
}

/*
 * XDR a descriminated union
 * Support routine for discriminated unions.
 * You create an array of xdrdiscrim structures, terminated with
 * an entry with a null procedure pointer.  The routine gets
 * the discriminant value and then searches the array of xdrdiscrims
 * looking for that value.  It calls the procedure given in the xdrdiscrim
 * to handle the discriminant.  If there is no specific routine a default
 * routine may be called.
 * If there is no specific or default routine an error is returned.
 */
bool_t
xdr_union(xdrs, dscmp, unp, choices, dfault)
	register XDR *xdrs;
	enum_t *dscmp;		/* enum to decide which arm to work on */
	char *unp;		/* the union itself */
	const struct xdr_discrim *choices;	/* [value, xdr proc] for each arm */
	xdrproc_t dfault;	/* default xdr routine */
{
	register enum_t dscm;
	bool_t dummy;

	/*
	 * we deal with the discriminator;  it's an enum
	 */
	trace1(TR_xdr_union, 0);
	if (! xdr_enum(xdrs, dscmp)) {
#ifdef KERNEL
		printf("xdr_enum: dscmp FAILED\n");
#endif
		trace1(TR_xdr_union, 1);
		return (FALSE);
	}
	dscm = *dscmp;

	/*
	 * search choices for a value that matches the discriminator.
	 * if we find one, execute the xdr routine for that value.
	 */
	for (; choices->proc != NULL_xdrproc_t; choices++) {
		if (choices->value == dscm) {
			dummy = (*(choices->proc))(xdrs, unp, LASTUNSIGNED);
			trace1(TR_xdr_union, 1);
			return (dummy);
		}
	}

	/*
	 * no match - execute the default xdr routine if there is one
	 */
	dummy = (dfault == NULL_xdrproc_t) ? FALSE :
	    (*dfault)(xdrs, unp, LASTUNSIGNED);
	trace1(TR_xdr_union, 1);
	return (dummy);
}


/*
 * Non-portable xdr primitives.
 * Care should be taken when moving these routines to new architectures.
 */


/*
 * XDR null terminated ASCII strings
 * xdr_string deals with "C strings" - arrays of bytes that are
 * terminated by a NULL character.  The parameter cpp references a
 * pointer to storage; If the pointer is null, then the necessary
 * storage is allocated.  The last parameter is the max allowed length
 * of the string as specified by a protocol.
 */
bool_t
xdr_string(xdrs, cpp, maxsize)
	register XDR *xdrs;
	char **cpp;
	u_int maxsize;
{
	bool_t dummy;
	register char *sp = *cpp;  /* sp is the actual string pointer */
	u_int size;
	u_int nodesize;

	/*
	 * first deal with the length since xdr strings are counted-strings
	 */
	trace2(TR_xdr_string, 0, maxsize);
	switch (xdrs->x_op) {
	case XDR_FREE:
		if (sp == NULL) {
			trace1(TR_xdr_string, 1);
			return (TRUE);	/* already free */
		}
		/* fall through... */
	case XDR_ENCODE:
		size = (sp != NULL) ?  strlen(sp) : 0;
		break;
	}
	/*
	 * We decided not to use MACRO XDR_U_INT here, because the
	 * advantages here will be miniscule compared to xdr_string.
	 * This saved us 100 bytes in the library size.
	 */
	if (! xdr_u_int(xdrs, &size)) {
#ifdef KERNEL
		printf("xdr_string: size FAILED\n");
#endif
		trace1(TR_xdr_string, 1);
		return (FALSE);
	}
	if (size > maxsize) {
#ifdef KERNEL
		printf("xdr_string: bad size FAILED\n");
#endif
		trace1(TR_xdr_string, 1);
		return (FALSE);
	}
	nodesize = size + 1;

	/*
	 * now deal with the actual bytes
	 */
	switch (xdrs->x_op) {

	case XDR_DECODE:
		if (nodesize == 0) {
			trace1(TR_xdr_string, 1);
			return (TRUE);
		}
		if (sp == NULL)
			*cpp = sp = (char *)mem_alloc(nodesize);
#ifndef KERNEL
		if (sp == NULL) {
/* WHG		(void) syslog(LOG_ERR, xdr_err,
				(const char *) "string"); WHG */
			WHG_MALLOC_ERR("xdr_string");
			trace1(TR_xdr_string, 1);
			return (FALSE);
		}
#endif
		sp[size] = 0;
		/* fall into ... */

	case XDR_ENCODE:
		dummy = xdr_opaque(xdrs, sp, size);
		trace1(TR_xdr_string, 1);
		return (dummy);

	case XDR_FREE:
		mem_free(sp, nodesize);
		*cpp = NULL;
		trace1(TR_xdr_string, 1);
		return (TRUE);
	}
#ifdef KERNEL
	printf("xdr_string: bad op FAILED\n");
#endif
	trace1(TR_xdr_string, 1);
	return (FALSE);
}

bool_t
xdr_hyper(xdrs, hp)
register XDR *xdrs;
longlong_t *hp;
{
	bool_t	dummy;

	trace1(TR_xdr_hyper, 0);
	if (xdrs->x_op == XDR_ENCODE){
#if !defined(vax) && !defined(i386)
		if (XDR_PUTLONG(xdrs, (long *) hp) == TRUE) {
			dummy = XDR_PUTLONG(xdrs, (long *)((char *) hp +
				BYTES_PER_XDR_UNIT));
			trace1(TR_xdr_hyper, 1);
			return (dummy);
		}

#else
		if (XDR_PUTLONG(xdrs, (long *)((char *) hp +
			BYTES_PER_XDR_UNIT)) == TRUE) {
			dummy = XDR_PUTLONG(xdrs, (long *) hp);
			trace1(TR_xdr_hyper, 1);
			return (dummy);
		}

#endif
		trace1(TR_xdr_hyper, 1);
		return (FALSE);

	} else if (xdrs->x_op == XDR_DECODE) {
#if !defined(vax) && !defined(i386)
		if (XDR_GETLONG(xdrs, (long *)hp) == FALSE ||
		    (XDR_GETLONG(xdrs, (long *)((char *) hp +
				BYTES_PER_XDR_UNIT)) == FALSE)) {
			trace1(TR_xdr_hyper, 1);
			return (FALSE);
		}
#else
  		if ((XDR_GETLONG(xdrs, (long *) ((char *) hp +
 				BYTES_PER_XDR_UNIT)) == FALSE) ||
  				(XDR_GETLONG(xdrs, (long *) hp) == FALSE)) {
 			trace1(TR_xdr_hyper, 1);
  			return (FALSE);
  		}
#endif
		trace1(TR_xdr_hyper, 1);
		return (TRUE);
	}
	trace1(TR_xdr_hyper, 1);
	return (TRUE);
}

bool_t
xdr_u_hyper(xdrs, hp)
register XDR *xdrs;
u_longlong_t *hp;
{
	bool_t dummy;

	trace1(TR_xdr_u_hyper, 0);
	dummy = xdr_hyper(xdrs, (longlong_t *)hp);
	trace1(TR_xdr_u_hyper, 1);
	return (dummy);
}

bool_t
xdr_longlong_t(xdrs, hp)
register XDR *xdrs;
longlong_t *hp;
{
	bool_t dummy;

	trace1(TR_xdr_longlong_t, 0);
	dummy = xdr_hyper(xdrs, hp);
	trace1(TR_xdr_longlong_t, 1);
	return (dummy);
}

bool_t
xdr_u_longlong_t(xdrs, hp)
register XDR *xdrs;
u_longlong_t *hp;
{
	bool_t dummy;

	trace1(TR_xdr_u_longlong_t, 0);
	dummy = xdr_hyper(xdrs, (longlong_t *)hp);
	trace1(TR_xdr_u_longlong_t, 1);
	return (dummy);
}

#ifndef KERNEL
/*
 * Wrapper for xdr_string that can be called directly from
 * routines like clnt_call
 */
bool_t
xdr_wrapstring(xdrs, cpp)
	XDR *xdrs;
	char **cpp;
{
	trace1(TR_xdr_wrapstring, 0);
	if (xdr_string(xdrs, cpp, LASTUNSIGNED)) {
		trace1(TR_xdr_wrapstring, 1);
		return (TRUE);
	}
	trace1(TR_xdr_wrapstring, 1);
	return (FALSE);
}
#endif /* !KERNEL */

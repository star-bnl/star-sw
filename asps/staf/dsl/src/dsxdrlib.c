/* Copyright 1993, Lawrence Berkeley Laboratory */

/* dsxdrlib.c - xdr routines for data structures */

/*
modification history
--------------------
01a,24apr93,whg  written.
*/

/*
DESCRIPTION
TBS ...
*/
#include <stdio.h>
#include <string.h>
#include <rpc/rpc.h>
#define DS_PRIVATE
#include "dscodes.h"
#include "dstype.h"
#include "dsxdr.h"

static int xdr_ctype(XDR *xdrs, char *base, size_t count, DS_TYPE_T *type);
static int xdr_swap(XDR *xdrs, char *ptr, int elsize, int elcount);
static bool_t xdr_table(XDR *xdrs, void *ptr, size_t typeID, size_t nrow);
static bool_t xdr_types(XDR *xdrs, DS_DATASET_T *pDataset, size_t *typeList);

#define XDR_CAST int (*)(XDR *, void *)

#define XDR_PAD(xdrs, n) ((n) == 0 ? TRUE : ((size_t)(n) > MAX_PAD ? FALSE :\
	((xdrs)->x_op == XDR_DECODE ? XDR_GETBYTES(xdrs, padBuf, n) :\
	((xdrs)->x_op == XDR_ENCODE ? XDR_PUTBYTES(xdrs, padZero, n) : FALSE))))

#define XDR_RAW(xdrs, ptr, n)\
	((xdrs)->x_op == XDR_DECODE ? XDR_GETBYTES(xdrs, ptr, n) :\
	((xdrs)->x_op == XDR_ENCODE ? XDR_PUTBYTES(xdrs, ptr, n) : FALSE))

#define MAX_PAD 7
static char padBuf[MAX_PAD], padZero[MAX_PAD] = {0, 0, 0, 0, 0, 0, 0};
/******************************************************************************
*
* xdr_ctype - translate an array of c structs
*
*/
static int xdr_ctype(XDR *xdrs, char *base, size_t count, DS_TYPE_T *type)
{
	char *ptr;
	int (*xdr_fcn)(XDR *xdrs, void *ptr);
	size_t i, nbytes, size;
	DS_FIELD_T *field, *limit;
	DS_TYPE_T *ftype;

#ifndef FORCE_FULL_XDR
	if (DS_REP_IS_STD(type)) {
		 if (!XDR_RAW(xdrs, base, count*type->stdsize)) {
			DS_ERROR(DS_E_XDR_IO_ERROR);
		}
		return TRUE;
	}
#endif
	if (DS_BASIC_TYPE(type)) {
		if ((size = type->size) == type->stdsize && DS_STD_REAL(type)) {
			if (DS_IS_BIG_ENDIAN || size == 1) {
				if (!XDR_RAW(xdrs, base, count*size)) {
					DS_ERROR(DS_E_XDR_IO_ERROR);
				}
			}
			else if (!xdr_swap(xdrs, base, size, count)) {
					DS_ERROR(DS_E_XDR_BYTE_SWAP_ERROR);
			}
			return TRUE;
		}
		switch (type->code) {

		 case DS_TYPE_INT:
			xdr_fcn = (XDR_CAST)xdr_int;
			break;

		 case DS_TYPE_U_INT:
			xdr_fcn = (XDR_CAST)xdr_u_int;
			break;

		 case DS_TYPE_LONG:
			xdr_fcn = (XDR_CAST)xdr_long;
			break;

		 case DS_TYPE_U_LONG:
			xdr_fcn = (XDR_CAST)xdr_u_long;
			break;

		case DS_TYPE_FLOAT:
			xdr_fcn = (XDR_CAST)xdr_float;
			break;

		case DS_TYPE_DOUBLE:
			xdr_fcn = (XDR_CAST)xdr_double;
			break;

		default:
			DS_ERROR(DS_E_SYSTEM_ERROR);
		}
		while (count-- >0) {
			if (!xdr_fcn(xdrs, base)) {
				DS_ERROR(DS_E_XDR_IO_ERROR);
			}
			base += type->size;
		}
		return TRUE;
	}
	if (type->code != DS_TYPE_STRUCT) {
		DS_ERROR(DS_E_SYSTEM_ERROR);
	}
	limit = DS_FIELD_PTR(type) + type->nField;
	for (i = 0; i < count; i++) {
		nbytes = 0;
		for (field = DS_FIELD_PTR(type); field != limit; field++) {
			ftype = field->type;
			ptr = base + field->offset;
			if (!XDR_PAD(xdrs, field->stdoffset - nbytes)) {
				DS_ERROR(DS_E_XDR_PAD_ERROR);
			}
			if (!xdr_ctype(xdrs, ptr, field->count, ftype)) {
				return FALSE;
			}
			nbytes = field->stdoffset + field->count*ftype->stdsize;
		}
		if (!XDR_PAD(xdrs, type->stdsize - nbytes)) {
			DS_ERROR(DS_E_XDR_PAD_ERROR);
		}
		base += type->size;
	}
	return TRUE;
}
/******************************************************************************
*
* xdr_dataset - encode or decode a dataset
*
*/
bool_t xdr_dataset(XDR *xdrs, DS_DATASET_T **ppDataset)
{
	DS_DATASET_T *pDataset = *ppDataset;

	if (xdrs->x_op == XDR_FREE) {
		dsFreeDataset(pDataset);
		*ppDataset = NULL;
		return TRUE;
	}
	if (!xdr_dataset_type(xdrs, &pDataset, 0)) {
		return FALSE;
	}
	if (xdrs->x_op == XDR_DECODE &&  !dsAllocTables(pDataset)) {
		goto fail;
	}
	if (!xdr_dataset_data(xdrs, pDataset)) {
		goto fail;
	}
	*ppDataset = pDataset;
	return TRUE;

fail:
	if (xdrs->x_op == XDR_DECODE) {
		dsFreeDataset(pDataset);
	}
	return FALSE;
}
/******************************************************************************
*
* xdr_dataset_data - encode or decode dataset tables
*
*/
bool_t xdr_dataset_data(XDR *xdrs, DS_DATASET_T *pDataset)
{
	size_t i, n = pDataset->elcount, tid = pDataset->tid;

	if (tid != 0) {
		return xdr_table(xdrs, pDataset->p.data, tid, n);
	}
	for (i = 0; i < n; i++) {
		if (!xdr_dataset_data(xdrs, &pDataset->p.child[i])) {
			return FALSE;
		}
	}
	return TRUE;
}
/******************************************************************************
*
* xdr_dataset_type - encode or decode abstract types for dataset
*
*/
bool_t xdr_dataset_type(XDR *xdrs, DS_DATASET_T **ppDataset, size_t dim)
{
	char buf[DS_MAX_DECL_LEN], *str;
	size_t *tList = NULL;
	DS_DATASET_T *pDataset = *ppDataset;

	if (!dsTypeListCreate(&tList, DS_XDR_HASH_LEN + 1)) {
		return FALSE;
	}
	if (xdrs->x_op == XDR_ENCODE) {
		if (!xdr_types(xdrs, pDataset, tList)) {
			goto fail;
		}
		str = buf;
		if (!dsFmtDatasetDef(str, sizeof(buf), pDataset)) {
			goto fail;
		}
		if (!xdr_string(xdrs, &str, sizeof(buf))) {
			DS_LOG_ERROR(DS_E_XDR_IO_ERROR);
			goto fail;
		}
		goto success;
	}
	if (xdrs->x_op != XDR_DECODE) {
		DS_LOG_ERROR(DS_E_INVALID_XDR_OP);
		goto fail;
	}
	for(;;) {
		str = buf;
		if (!xdr_string(xdrs, &str, sizeof(buf) - 1)) {
			DS_LOG_ERROR(DS_E_XDR_IO_ERROR);
			goto fail;
		}
		if (strncmp(buf, "type", 4) == 0) {
			if (!dsTypeListEnter(tList, buf + 4, &str)) {
				goto fail;
			}
		}
		else if (strncmp(buf, "data", 4) == 0) {
			if (!dsCreateDataset(&pDataset,
				dim, tList, buf+4, &str)) {
				goto fail;
			}
			*ppDataset = pDataset;
			goto success;
		}
		else {
			DS_LOG_ERROR(DS_E_INVALID_DATASET);
			goto fail;
		}
	}
	fail:
		dsTypeListFree(tList);
		return FALSE;
	success:
		dsTypeListFree(tList);
		return TRUE;
}
/******************************************************************************
*
* xdr_swap - translate little endian integer and ieee floating point types
*
*/
static int xdr_swap(XDR *xdrs, char *ptr, int elsize, int elcount)
{
	char buf[DS_SWAP_BUF_SIZE];
	int i, n, nbytes = elcount*elsize;

	if (xdrs->x_op == XDR_DECODE) {
		while (nbytes) {
			n = nbytes > sizeof(buf) ? sizeof(buf) : nbytes;
			if (!XDR_GETBYTES(xdrs, buf, n)) {
				return FALSE;
			}
			switch (elsize) {
			case 2:
				for (i = 0; i < n; i += 2) {
					*ptr++ = buf[i + 1];
					*ptr++ = buf[i];
				}
				break;
			case 4:
				for (i = 0; i < n; i += 4) {
					*ptr++ = buf[i + 3];
					*ptr++ = buf[i + 2];
					*ptr++ = buf[i + 1];
					*ptr++ = buf[i];
				}
				break;
			case 8:
				for (i = 0; i < n; i += 8) {
					*ptr++ = buf[i + 7];
					*ptr++ = buf[i + 6];
					*ptr++ = buf[i + 5];
					*ptr++ = buf[i + 4];
					*ptr++ = buf[i + 3];
					*ptr++ = buf[i + 2];
					*ptr++ = buf[i + 1];
					*ptr++ = buf[i];
				}
				break;
			default:
				return FALSE;
			}
			nbytes -= n;
		}
		return TRUE;
	}
	else if (xdrs->x_op == XDR_ENCODE) {
		while (nbytes) {
			n = nbytes > sizeof(buf) ? sizeof(buf) : nbytes;
			switch (elsize) {
			case 2:
				for (i = 0; i < n; i += 2) {
					buf[i + 1] = *ptr++;
					buf[i] = *ptr++;
				}
				break;
			case 4:
				for (i = 0; i < n; i += 4) {
					buf[i + 3] = *ptr++;
					buf[i + 2] = *ptr++;
					buf[i + 1] = *ptr++;
					buf[i] = *ptr++;
				}
				break;
			case 8:
				for (i = 0; i < n; i += 8) {
					buf[i + 7] = *ptr++;
					buf[i + 6] = *ptr++;
					buf[i + 5] = *ptr++;
					buf[i + 4] = *ptr++;
					buf[i + 3] = *ptr++;
					buf[i + 2] = *ptr++;
					buf[i + 1] = *ptr++;
					buf[i] = *ptr++;
				}
				break;
			default:
				return FALSE;
			}
			nbytes -= n;
			if (!XDR_PUTBYTES(xdrs, buf, n)) {
				return FALSE;
			}
		}
	}
	else {
		return FALSE;
	}
	return TRUE;
}
/******************************************************************************
*
* xdr_table - decode or encode table data
*
*/
static bool_t xdr_table(XDR *xdrs, void *ptr, size_t tid, size_t nrow)
{
	size_t npad, size;
	DS_TYPE_T *type;
    
    if (nrow == 0) {
    	return TRUE;
    }
	if (ptr == NULL) {
		DS_ERROR(DS_E_NULL_POINTER_ERROR);
	}
	if (!dsTypePtr(&type, tid)) {
		return FALSE;
	}
	size = nrow*type->stdsize;
	npad = DS_PAD(size, BYTES_PER_XDR_UNIT);
	if (!xdr_ctype(xdrs, ptr, nrow, type)) {
		return FALSE;
	}
	if (!XDR_PAD(xdrs, npad)) {
		DS_ERROR(DS_E_XDR_PAD_ERROR);
	}
	return TRUE;
}
/******************************************************************************
*
* xdr_types - encode type declarations for dataset tables
*
*/
static bool_t xdr_types(XDR *xdrs, DS_DATASET_T *pDataset, size_t *tList)
{
	char *str;
	size_t h, i, tid = pDataset->tid;
	DS_TYPE_T *pType;

	if (tid != 0) {
		if (!dsTypePtr(&pType, tid)) {
			return FALSE;
		}
		if (!dsTypeListFind(&h, tList, pType->name, NULL)) {
			return FALSE;
		}
		if (tList[h] == tid) {
			return TRUE;
		}
		if (tList[h]) {
			DS_ERROR(DS_E_DUPLICATE_TYPE_NAME);
		}
		if (!dsTypeDef(&str, &i, tid)) {
			return FALSE;
		}
		if (!xdr_bytes(xdrs, &str, (int *)&i, i)) {
			DS_ERROR(DS_E_XDR_IO_ERROR);
		}
		tList[h] = tid;
		return TRUE;
	}
	for (i = 0; i < pDataset->elcount; i++) {
		if (!xdr_types(xdrs, &pDataset->p.child[i], tList)) {
			return FALSE;
		}
	}
	return TRUE;
}

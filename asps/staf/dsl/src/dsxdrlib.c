/* Copyright 1993, Lawrence Berkeley Laboratory */

/* dsxdrlib.c - xdr routines for data structures */

/*
modification history
--------------------
24apr93,whg  written.
11jun96,whg  added indirection to dataset structure
*/

/*
DESCRIPTION
xdr interface for tables and datasets
*/
#include <string.h>
#define DS_PRIVATE
#include "asuAlloc.h"
#include "dsxdr.h"

/******************************************************************************
*
* macros and variables for padding and raw transfers
*
*/
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
* prototypes for static functions
*
*/
static int xdr_ctype(XDR *xdrs, char *base, size_t count, DS_TYPE_T *type);
static int xdr_swap(XDR *xdrs, char *ptr, int elsize, int elcount);
static bool_t xdr_table(XDR *xdrs, void *ptr, size_t typeID, size_t nrow);
static bool_t xdr_types(XDR *xdrs, DS_DATASET_T *pDataset, size_t *typeList);
/******************************************************************************
*
* xdr_ctype - translate an array of c structs
*
* RETURNS: TRUE if success else FALSE
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
		if ((size = type->size) == type->stdsize &&
			(DS_IS_IEEE_FLOAT || !DS_IS_REAL(type))) {
			if (DS_IS_BIG_ENDIAN || size == 1) {
				if (!XDR_RAW(xdrs, base, count*size)) {
					DS_ERROR(DS_E_XDR_IO_ERROR);
				}
			}
			else if (!xdr_swap(xdrs, base, size, count)) {
				return FALSE;
			}
			return TRUE;
		}
		switch (type->code) {

		 case DS_TYPE_LONG:
			xdr_fcn = (int (*)(XDR *, void *))xdr_long;
			break;

		 case DS_TYPE_U_LONG:
			xdr_fcn = (int (*)(XDR *, void *))xdr_u_long;
			break;

		case DS_TYPE_FLOAT:
			xdr_fcn = (int (*)(XDR *, void *))xdr_float;
			break;

		case DS_TYPE_DOUBLE:
			xdr_fcn = (int (*)(XDR *, void *))xdr_double;
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
* RETURNS: TRUE if success else FALSE
*/
bool_t xdr_dataset(XDR *xdrs, DS_DATASET_T **ppDataset)
{
	DS_DATASET_T *pDataset = *ppDataset;

	if (xdrs->x_op == XDR_FREE) {
		dsFreeDataset(pDataset);
		*ppDataset = NULL;
		return TRUE;
	}
	if (!xdr_dataset_type(xdrs, &pDataset)) {
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
* RETURNS: TRUE if success else FALSE
*/
bool_t xdr_dataset_data(XDR *xdrs, DS_DATASET_T *pDataset)
{
	size_t i;
	DS_DATASET_T *item;
	DS_LIST_T list;

	if (xdrs->x_op != XDR_DECODE && xdrs->x_op != XDR_ENCODE) {
		DS_ERROR(DS_E_INVALID_XDR_OP);
	}
	if (!dsListInit(&list)) {
		return FALSE;
	}
	if (!dsVisitList(&list, pDataset)) {
		goto fail;
	}
	for (i = 0; i < list.count; i++) {
		item = list.pItem[i];
		if (DS_IS_TABLE(item)) {
			if (xdrs->x_op == XDR_DECODE) {
				item->elcount = item->maxcount;
			}
			if (!xdr_table(xdrs, item->p.data, item->tid, item->elcount)) {
				goto fail;
			}
		}
	}
	return dsListFree(&list);
fail:
	dsListFree(&list);
	return FALSE;
}
/******************************************************************************
*
* xdr_dataset_type - encode or decode abstract types for dataset
*
* RETURNS: TRUE if success else FALSE
*/
bool_t xdr_dataset_type(XDR *xdrs, DS_DATASET_T **ppDataset)
{
	char buf[DS_MAX_SPEC_LEN+1], *str;
	size_t *tList = NULL;
	DS_BUF_T bp;
	DS_DATASET_T *pDataset = *ppDataset;

	if (!dsTypeListCreate(&tList, DS_XDR_HASH_LEN + 1)) {
		return FALSE;
	}
	if (xdrs->x_op == XDR_ENCODE) {
		if (!xdr_types(xdrs, pDataset, tList)) {
			goto fail;
		}
		str = buf;
		DS_PUT_INIT(&bp, buf, sizeof(buf));
		if (!dsDatasetSpecifier(&bp, pDataset)) {
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
			if (!dsTypeListEnter(tList, buf + 4, (const char**)&str)) {
				goto fail;
			}
		}
		else if (strncmp(buf, "data", 4) == 0) {
			if (!dsCreateDataset(&pDataset,
				tList, buf+4, (const char**)&str)) {
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
* RETURNS: TRUE if success else FALSE
*/
static int xdr_swap(XDR *xdrs, char *ptr, int elsize, int elcount)
{
	char buf[DS_SWAP_BUF_SIZE];	/* size must be a multiple of eight */
	int i, n, nbytes = elcount*elsize;
	
	if (elsize == 0 || sizeof(buf)%elsize != 0) {
		DS_ERROR(DS_E_SYSTEM_ERROR);
	}

	if (xdrs->x_op == XDR_DECODE) {
		while (nbytes) {
			n = nbytes > sizeof(buf) ? sizeof(buf) : nbytes;
			if (!XDR_GETBYTES(xdrs, buf, n)) {
				DS_ERROR(DS_E_XDR_IO_ERROR);
			}
			switch (elsize) {
			case 2:
				for (i = 0; i < n; i += 2) {
					*ptr++ = buf[i + 1];
					*ptr++ = buf[i + 0];
				}
				break;
			case 4:
				for (i = 0; i < n; i += 4) {
					*ptr++ = buf[i + 3];
					*ptr++ = buf[i + 2];
					*ptr++ = buf[i + 1];
					*ptr++ = buf[i + 0];
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
					*ptr++ = buf[i + 0];
				}
				break;
			default:
				DS_ERROR(DS_E_XDR_BYTE_SWAP_ERROR);
			}
			nbytes -= n;
		}
	}
	else if (xdrs->x_op == XDR_ENCODE) {
		while (nbytes) {
			n = nbytes > sizeof(buf) ? sizeof(buf) : nbytes;
			switch (elsize) {
			case 2:
				for (i = 0; i < n; i += 2) {
					buf[i + 1] = *ptr++;
					buf[i + 0] = *ptr++;
				}
				break;
			case 4:
				for (i = 0; i < n; i += 4) {
					buf[i + 3] = *ptr++;
					buf[i + 2] = *ptr++;
					buf[i + 1] = *ptr++;
					buf[i + 0] = *ptr++;
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
					buf[i + 0] = *ptr++;
				}
				break;
			default:
				DS_ERROR(DS_E_XDR_BYTE_SWAP_ERROR);
			}
			nbytes -= n;
			if (!XDR_PUTBYTES(xdrs, buf, n)) {
				DS_ERROR(DS_E_XDR_IO_ERROR);
			}
		}
	}
	else {
		DS_ERROR(DS_E_XDR_IO_ERROR);
	}
	return TRUE;
}
/******************************************************************************
*
* xdr_table - decode or encode table data
*
* RETURNS: TRUE if success else FALSE
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
* RETURNS: TRUE if success else FALSE
*/
static bool_t xdr_types(XDR *xdrs, DS_DATASET_T *pDataset, size_t *tList)
{
	char *str, *typeStr = "type ";
	size_t h, i, len, npad;
	long size;
	DS_DATASET_T *item;
	DS_LIST_T list;
	DS_TYPE_T *pType;

	if (!dsListInit(&list)) {
		return FALSE;
	}
	if (!dsVisitList(&list, pDataset)) {
		goto fail;
	}
	for (i = 0; i < list.count; i++) {
		item = list.pItem[i];
		if (!DS_IS_TABLE(item)) {
			continue;
		}
		if (!dsTypePtr(&pType, item->tid) ||
			!dsTypeListFind(&h, tList, pType->name)) {
			goto fail;
		}
		if (tList[h] == item->tid) {
			continue;
		}
		if (tList[h]) {
			DS_LOG_ERROR(DS_E_DUPLICATE_TYPE_NAME);
			goto fail;
		}
		if (!dsTypeSpecifier((const char**)&str, &len, item->tid)) {
			goto fail;
		}
		size = len + strlen(typeStr);
		npad = DS_PAD((size_t)size, BYTES_PER_XDR_UNIT);
		if (!xdr_long(xdrs, &size) ||
			!XDR_PUTBYTES(xdrs, typeStr, strlen(typeStr)) ||
			!XDR_PUTBYTES(xdrs, str, len) ||
			!XDR_PAD(xdrs, npad)) {
			DS_LOG_ERROR(DS_E_XDR_IO_ERROR);
			goto fail;
		}
		tList[h] = item->tid;
	}
	return dsListFree(&list);
fail:
	dsListFree(&list);
	return FALSE;
}

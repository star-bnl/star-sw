/* Copyright 1993, Lawrence Berkeley Laboratory */

/* dsxdrlib.c - xdr routines for data structures */

/*
modification history
--------------------
24apr93,whg  written.
11jun96,whg  added indirection to dataset structure
07apr98,whg  allow little endian representation of data
*/

/*
DESCRIPTION
xdr interface for tables and datasets
*/
#include <string.h>
#define DS_PRIVATE
#include "dsxdr.h"
/******************************************************************************
*
* macros and variables for padding and raw transfers
*
*/
#define OFFSET(name)\
	(DS_IS_BIG_ENDIAN ? sizeof(DS_ ## name) - (DS_LEN_ ## name) : 0)

#define XDR_PAD(xdrs, n) ((n) == 0 ? TRUE : ((size_t)(n) > MAX_PAD ? FALSE :\
	(xdrs)->x_op == XDR_DECODE ? XDR_GETBYTES(xdrs, padBuf, n) :\
	XDR_PUTBYTES(xdrs, padZero, n)))

#define XDR_RAW(xdrs, ptr, n) ((xdrs)->x_op == XDR_DECODE ?\
	XDR_GETBYTES(xdrs, ptr, n) : XDR_PUTBYTES(xdrs, ptr, n))

#define MAX_PAD 7
static int checkTruncation = TRUE;
static char padBuf[MAX_PAD], padZero[MAX_PAD] = {0, 0, 0, 0, 0, 0, 0};
/******************************************************************************
*
* prototypes for static functions
*
*/
static int xdr_ctype(XDR *xdrs, char *base, size_t count,
					 DS_TYPE_T *type, int swap);
static int xdr_swap(XDR *xdrs, char *ptr, int elsize, int elcount);
static bool_t dsDecodeType(XDR *xdrs, DS_DATASET_T **ppDataset);
static bool_t dsEncodeType(XDR *xdrs, DS_DATASET_T *pDataset);
/*****************************************************************************
*
* dsCheckTruncation - enable truncation check of integer types during write
*
* RETURNS: TRUE
*/
int dsCheckTruncation()
{
	checkTruncation = TRUE;
	return TRUE;
}
/*****************************************************************************
*
* dsConvertLong - convert a long with size > DS_LEN_LONG
*
* RETURNS: TRUE if success else FALSE
*/
static int dsConvertLong(XDR *xdrs, char *base, int swap)
{
	DS_LONG l;

	if (xdrs->x_op == XDR_DECODE) {
		l = 0;
		if (swap) {
			if (!xdr_swap(xdrs, (char *)&l + OFFSET(LONG), DS_LEN_LONG, 1)) {
				return FALSE;
			}
		}
		else if (!XDR_GETBYTES(xdrs, (char *)&l + OFFSET(LONG), DS_LEN_LONG)) {
			DS_ERROR(DS_E_XDR_IO_ERROR);
		}
		if ((l & DS_LONG_SIGN) != 0) {
			l |= ~DS_LONG_MASK;
		}

		*((DS_LONG *)base) = l;
	}
	else {
		l = *((DS_LONG *)base);
		if ((l  < DS_MIN_LONG || l > DS_MAX_LONG) && checkTruncation) {
			DS_ERROR(DS_E_TRUNCATION_ERROR);
		}
		if (swap) {
			return xdr_swap(xdrs, base + OFFSET(LONG), DS_LEN_LONG, 1);
		}
		else if (!XDR_PUTBYTES(xdrs, base + OFFSET(LONG), DS_LEN_LONG)) {
			DS_ERROR(DS_E_XDR_IO_ERROR);
		}
	}
	return TRUE;
}
/*****************************************************************************
*
* dsConvertShort - convert a short with size > DS_LEN_SHORT
*
* RETURNS: TRUE if success else FALSE
*/
static int dsConvertShort(XDR *xdrs, char *base, int swap)
{
	DS_SHORT s;

	if (xdrs->x_op == XDR_DECODE) {
		s = 0;
		if (swap) {
			if (!xdr_swap(xdrs, (char *)&s + OFFSET(SHORT), DS_LEN_SHORT, 1)) {
				return FALSE;
			}
		}
		else if (!XDR_GETBYTES(xdrs, (char *)&s + OFFSET(SHORT), DS_LEN_SHORT)) {
			DS_ERROR(DS_E_XDR_IO_ERROR);
		}
		if ((s & DS_SHORT_SIGN) != 0) {
			s |= ~DS_SHORT_MASK;
		}
		*((DS_SHORT *)base) = s;
	}
	else {
		s = *((DS_SHORT *)base);
		if ((s  < DS_MIN_SHORT || s > DS_MAX_SHORT) && checkTruncation) {
			DS_ERROR(DS_E_TRUNCATION_ERROR);
		}
		if (swap) {
			return xdr_swap(xdrs, base + OFFSET(SHORT), DS_LEN_SHORT, 1);
		}
		else if (!XDR_PUTBYTES(xdrs, base + OFFSET(SHORT), DS_LEN_SHORT)) {
			DS_ERROR(DS_E_XDR_IO_ERROR);
		}
	}
	return TRUE;
}
/*****************************************************************************
*
* dsConvertUnsignedLong - convert a unsigned long with size > DS_LEN_U_LONG
*
* RETURNS: TRUE if success else FALSE
*/
static int dsConvertUnsignedLong(XDR *xdrs, char *base, int swap)
{
	if (xdrs->x_op == XDR_DECODE) {
		*((DS_U_LONG *)base) = 0;
		if (swap) {
			return xdr_swap(xdrs, base + OFFSET(U_LONG), DS_LEN_U_LONG, 1);
		}
		else if (!XDR_GETBYTES(xdrs, base + OFFSET(U_LONG), DS_LEN_U_LONG)) {
			DS_ERROR(DS_E_XDR_IO_ERROR);
		}
	}
	else {
		if (*((DS_LONG *)base) > DS_MAX_U_LONG && checkTruncation) {
			DS_ERROR(DS_E_TRUNCATION_ERROR);
		}
		if (swap) {
			return xdr_swap(xdrs, base + OFFSET(U_LONG), DS_LEN_U_LONG, 1);
		}
		else if (!XDR_PUTBYTES(xdrs, base + OFFSET(U_LONG), DS_LEN_U_LONG)) {
			DS_ERROR(DS_E_XDR_IO_ERROR);
		}
	}
	return TRUE;
}
/*****************************************************************************
*
* dsConvertUnsignedShort - convert a unsigned short with size > DS_LEN_U_SHORT
*
* RETURNS: TRUE if success else FALSE
*/
static int dsConvertUnsignedShort(XDR *xdrs, char *base, int swap)
{
	if (xdrs->x_op == XDR_DECODE) {
		*((DS_U_SHORT *)base) = 0;
		if (swap) {
			return xdr_swap(xdrs, base + OFFSET(U_SHORT), DS_LEN_U_SHORT, 1);
		}
		else if (!XDR_GETBYTES(xdrs, base + OFFSET(U_SHORT), DS_LEN_U_SHORT)) {
				DS_ERROR(DS_E_XDR_IO_ERROR);
		}
	}
	else {
		if (*((DS_SHORT *)base) > DS_MAX_U_SHORT && checkTruncation) {
			DS_ERROR(DS_E_TRUNCATION_ERROR);
		}
		if (swap) {
			return xdr_swap(xdrs, base + OFFSET(U_SHORT), DS_LEN_U_SHORT, 1);
		}
		else if (!XDR_PUTBYTES(xdrs, base + OFFSET(U_SHORT), DS_LEN_U_SHORT)) {
			DS_ERROR(DS_E_XDR_IO_ERROR);
		}
	}
	return TRUE;
}
/******************************************************************************
*
* dsDecodeType - decode abstract types for dataset
*
* RETURNS: TRUE if success else FALSE
*/
static bool_t dsDecodeType(XDR *xdrs, DS_DATASET_T **ppDataset)
{
	char buf[DS_MAX_SPEC_LEN+1], *str;
	int firstLine = TRUE, optsLittleEndian = FALSE;
	size_t *tList = NULL;
	DS_DATASET_T *pDataset = *ppDataset;

	if (!dsTypeListCreate(&tList, DS_XDR_HASH_LEN + 1)) {
		return FALSE;
	}
	for(;;) {
		str = buf;
		if (!xdr_string(xdrs, &str, sizeof(buf) - 1)) {
			DS_LOG_ERROR(DS_E_XDR_IO_ERROR);
			goto fail;
		}
		if (strncmp(buf, "type ", 5) == 0) {
			if (!dsTypeListEnter(tList, buf + 5, &str)) {
				goto fail;
			}
		}
		else if (strncmp(buf, "data ", 5) == 0) {
			if (!dsCreateDataset(&pDataset,
				tList, buf + 5, &str)) {
				goto fail;
			}
			pDataset->flags = optsLittleEndian ? 
				pDataset->flags | DS_F_XDR_L_END : 
				pDataset->flags & ~DS_F_XDR_L_END;
			*ppDataset = pDataset;
			return dsTypeListFree(tList);
		}
		else if (strncmp(buf, "opts ", 5) == 0) {
			if (strstr(buf + 5, "lend") != NULL && firstLine) {
				optsLittleEndian = TRUE;
			}
			else {
				DS_LOG_ERROR(DS_E_DATASET_OPTS_ERROR);
				goto fail;
			}
		}
		else {
			DS_LOG_ERROR(DS_E_INVALID_DATASET);
			goto fail;
		}
	}
	fail:
		dsTypeListFree(tList);
		return FALSE;
}
/*****************************************************************************
*
* dsEncodeBigEndian - write a dataset with big endian byte order
*
* RETURNS: TRUE if success else FALSE
*/
bool_t dsEncodeBigEndian(XDR *xdrs, DS_DATASET_T *pDataset)
{
	if (xdrs->x_op != XDR_ENCODE) {
		DS_ERROR(DS_E_INVALID_XDR_OP);
	}
	pDataset->flags &= ~DS_F_XDR_L_END;
	return dsEncodeType(xdrs, pDataset) && xdr_dataset_data(xdrs, pDataset);
}
/*****************************************************************************
*
* dsEncodeLittleEndian - write a dataset with little endian byte order
*
* RETURNS: TRUE if success else FALSE
*/
bool_t dsEncodeLittleEndian(XDR *xdrs, DS_DATASET_T *pDataset)
{
	if (xdrs->x_op != XDR_ENCODE) {
		DS_ERROR(DS_E_INVALID_XDR_OP);
	}
	pDataset->flags |= DS_F_XDR_L_END;
	return dsEncodeType(xdrs, pDataset) && xdr_dataset_data(xdrs, pDataset);
}
/******************************************************************************
*
* dsEncodeType - encode type declarations for dataset tables
*
* RETURNS: TRUE if success else FALSE
*/
static bool_t dsEncodeType(XDR *xdrs, DS_DATASET_T *pDataset)
{
	char buf[DS_MAX_SPEC_LEN+1];
	size_t *tList = NULL;
	char *str, *typeStr = "type ";
	size_t h, i;
	unsigned size;
	DS_DATASET_T *item;
	DS_LIST_T list;
	DS_TYPE_T *pType;

	if (!dsTypeListCreate(&tList, DS_XDR_HASH_LEN + 1)) {
		return FALSE;
	}
	dsListInit(&list);
	if (!dsVisitList(&list, pDataset)) {
		goto fail;
	}
	if (DS_LITTLE_ENDIAN_XDR(pDataset)) {
		str = "opts lend";
		if (!xdr_string(xdrs, &str, strlen(str))) {
			DS_LOG_ERROR(DS_E_XDR_IO_ERROR);
			goto fail;
		}
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
		if (tList[h] != 0) {
			DS_LOG_ERROR(DS_E_DUPLICATE_TYPE_NAME);
			goto fail;
		}
		tList[h] = item->tid;
		if (!dsTypeSpecifier(&str, item->tid)) {
			goto fail;
		}
		size = strlen(str) + strlen(typeStr);
		if (size >= sizeof(buf)) {
			DS_LOG_ERROR(DS_E_TYPE_STRING_TOO_LONG);
			goto fail;
		}
		strcat(strcpy(buf, typeStr), str);
		str = buf;
		if (!xdr_string(xdrs, &str, size)) {
			DS_LOG_ERROR(DS_E_XDR_IO_ERROR);
			goto fail;
		}
	}
	if (!dsDatasetSpecifier(pDataset, buf, sizeof(buf))) {
		goto fail;
	}
	str = buf;
	if (!xdr_string(xdrs, &str, sizeof(buf) -1)) {
		DS_LOG_ERROR(DS_E_XDR_IO_ERROR);
		goto fail;
	}
	dsTypeListFree(tList);
	return dsListFree(&list);
fail:
	dsTypeListFree(tList);
	dsListFree(&list);
	return FALSE;
}
/*****************************************************************************
*
* dsIgnoreTruncation - disable truncation check of integer types during write
*
* RETURNS: TRUE
*/
int dsIgnoreTruncation()
{
	checkTruncation = FALSE;
	return TRUE;
}
/******************************************************************************
*
* xdr_ctype - translate an array of c structs
*
* RETURNS: TRUE if success else FALSE
*/
static int xdr_ctype(XDR *xdrs, char *base,
					 size_t count, DS_TYPE_T *type, int swap)
{
	char *ptr;
	typedef int (*XDR_FCN)(XDR *, char *, int);
	XDR_FCN xdr_fcn;
	size_t i, nbytes;
	DS_FIELD_T *field, *limit;
	DS_TYPE_T *ftype;

	if (DS_REP_IS_STD(type) && (!swap || !DS_IS_MULTI_BYTE(type))) {
		 if (!XDR_RAW(xdrs, base, count*type->stdsize)) {
			DS_ERROR(DS_E_XDR_IO_ERROR);
		}
		return TRUE;
	}
	if (DS_BASIC_TYPE(type)) {
		if (DS_REP_IS_STD(type)) {
			if (!swap) {
				if (!XDR_RAW(xdrs, base, type->size*count)) {
					DS_ERROR(DS_E_XDR_IO_ERROR);
				}
			}
			else if (!xdr_swap(xdrs, base, type->size, count)) {
				return FALSE;
			}
			return TRUE;
		}
		switch (type->code) {
		case DS_TYPE_SHORT:
			xdr_fcn = dsConvertShort; 
			break;

		case DS_TYPE_U_SHORT:
			xdr_fcn = dsConvertUnsignedShort;
			break;

		case DS_TYPE_LONG:
			xdr_fcn = dsConvertLong;
			break;

		case DS_TYPE_U_LONG:
			xdr_fcn = dsConvertUnsignedLong;
			break;

		case DS_TYPE_FLOAT:
		case DS_TYPE_DOUBLE:
			DS_ERROR(DS_E_NON_IEEE_FLOATING_POINT);

		default:
			DS_ERROR(DS_E_SYSTEM_ERROR);
		}
		while (count-- >0) {
			if (!xdr_fcn(xdrs, base, swap)) {
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
			if (!xdr_ctype(xdrs, ptr, field->count, ftype, swap)) {
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
	int swap;
	size_t elcount, i, npad, skip = 0;
	unsigned pos;
	DS_DATASET_T *item;
	DS_LIST_T list;
	DS_TYPE_T *type;

	if (xdrs->x_op != XDR_DECODE && xdrs->x_op != XDR_ENCODE) {
		DS_ERROR(DS_E_INVALID_XDR_OP);
	}
	dsListInit(&list);
	if (!dsVisitList(&list, pDataset)) {
		goto fail;
	}
	swap = (DS_LITTLE_ENDIAN_XDR(pDataset) ?
				DS_IS_BIG_ENDIAN : DS_IS_LITTLE_ENDIAN);
	for (i = 0; i < list.count; i++) {
		item = list.pItem[i];
		if (DS_IS_TABLE(item)) {
			elcount = xdrs->x_op == XDR_DECODE ? item->maxcount : item->elcount;
			if (elcount == 0) {
				continue;
			}
			if (!dsTypePtr(&type, item->tid)) {
				goto fail;
			}
			npad = DS_PAD(elcount*type->stdsize, BYTES_PER_XDR_UNIT);
			if (item->p.data == NULL) {
				if (xdrs->x_op != XDR_DECODE) {
					DS_LOG_ERROR(DS_E_NULL_POINTER_ERROR);
					goto fail;
				}
				skip += elcount*type->stdsize + npad;
				continue;
			}
			if (skip !=0) {
				pos = xdr_getpos(xdrs) + skip;
				skip = 0;
				if (!xdr_setpos(xdrs, pos)) {
					DS_LOG_ERROR(DS_E_XDR_SETPOS_ERROR);
					goto fail;
				}
			}
			item->elcount = elcount;
			if (!xdr_ctype(xdrs, item->p.data, item->elcount, type, swap)) {
				goto fail;
			}
			if (!XDR_PAD(xdrs, npad)) {
				DS_LOG_ERROR(DS_E_XDR_PAD_ERROR);
				goto fail;
			}
		}
	}
	if (skip !=0) {
		pos = xdr_getpos(xdrs) + skip;
		skip = 0;
		if (!xdr_setpos(xdrs, pos)) {
			DS_LOG_ERROR(DS_E_XDR_SETPOS_ERROR);
			goto fail;
		}
	}
	return dsListFree(&list);
fail:
	dsListFree(&list);
	return FALSE;
}
/******************************************************************************
*
* xdr_dataset_skip - skip a dataset in an decode stream
*
* RETURNS: TRUE if success else FALSE
*/
bool_t xdr_dataset_skip(XDR *xdrs)
{
	DS_DATASET_T *pDataset = NULL;

	if (xdrs->x_op != XDR_DECODE) {
		DS_ERROR(DS_E_INVALID_XDR_OP);
	}
	if (!xdr_dataset_type(xdrs, &pDataset)) {
		return FALSE;
	}
	if (!xdr_dataset_data(xdrs, pDataset)) {
		dsFreeDataset(pDataset);
		return FALSE;
	}
	return dsFreeDataset(pDataset);
}
/******************************************************************************
*
* xdr_dataset_type - encode or decode abstract types for dataset
*
* RETURNS: TRUE if success else FALSE
*/
bool_t xdr_dataset_type(XDR *xdrs, DS_DATASET_T **ppDataset)
{
	DS_DATASET_T *pDataset;

	if (xdrs->x_op == XDR_ENCODE) {
		pDataset = *ppDataset;
		pDataset->flags &= ~DS_F_XDR_L_END;
		return dsEncodeType(xdrs, pDataset);
	}
	if (xdrs->x_op == XDR_DECODE) {
		return dsDecodeType(xdrs, ppDataset);
	}
	DS_ERROR(DS_E_INVALID_XDR_OP);
}
/******************************************************************************
*
* xdr_swap - xdr with swaped byte order for integer and ieee floating point
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
		DS_ERROR(DS_E_INVALID_XDR_OP);
	}
	return TRUE;
}

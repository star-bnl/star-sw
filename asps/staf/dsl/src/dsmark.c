#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#define DS_PRIVATE
#include "dsxdr.h"

#ifndef SEEK_SET /* for sunos */
#define SEEK_SET 0
#define SEEK_END 2
#endif /* ifndef SEEK_SET */

void dsMarkRead(int argc, char *argv[]);
int dsMarkSize(DS_DATASET_T *pDataset, size_t *pSize);
void dsMarkTypes(int argc, char *argv[]);
void dsMarkWrite(int argc, char *argv[]);
void usage(FILE *stream);
/*****************************************************************************
*/
void main(int argc, char *argv[])
{
	char *ptr;
	double t, t0;
	int optChar;

	if (argc < 3 || strlen(argv[1]) != 1 ||
		NULL == strchr("drtw", optChar = *argv[1])) {
		usage(stdout);
		exit(0);
	}
	t0 = msecTime(&ptr);
	switch(optChar) {
	case 'd':
	case 't':
		dsMarkTypes(argc, argv);
		break;

	case 'r':
		dsMarkRead(argc, argv);
		break;

	case 'w':
		dsMarkWrite(argc, argv);
		break;

	default:
		fprintf(stderr, "unimplemented option %c\n", optChar);
		break;
	}
	t = msecTime(&ptr);
	printf("%.12s elapsed time %.3f sec\n", ptr + 11, t - t0);
}
/*****************************************************************************
*/
void dsMarkRead(int argc, char *argv[])
{
	char *fileName;
	int i, optChar;
	long curPos, eofPos;
	size_t dataBytes, dsCount, size, typeBytes;
	DS_DATASET_T *pDataset, *pEntry;
	FILE *stream;
	XDR xdr;

	optChar = *argv[1];
	fileName = argv[2];
	if (argc < 3) {
		fprintf(stderr, "Too few arguments for option %c\n", optChar);
		exit(0);
	}
	if ((stream = fopen(fileName, "rb")) == NULL) {
		fprintf(stderr, "fopen failed for %s\n", fileName);
		exit(0);
	}
	fseek(stream, 0, SEEK_END);
	eofPos = ftell(stream);
	curPos = 0;
	fseek(stream, curPos, SEEK_SET);
	xdrstdio_create(&xdr, stream, XDR_DECODE);
	for (dataBytes = dsCount = typeBytes = 0;curPos < eofPos; dsCount++) {
		pDataset = NULL;
		if (!xdr_dataset_type(&xdr, &pDataset)) {
			dsPerror("dsMarkRead: xdr_dataset_type failed\n");
			exit(0);
		}
		typeBytes += xdr_getpos(&xdr) - curPos;
		for (i = 3; i < argc; i++) {
			if (strcmp(argv[i], "..") == 0) {
				pEntry = pDataset;
			}
			else if (!dsFindEntry(&pEntry, pDataset, argv[i])) {
				continue;
			}
			if (!dsAllocTables(pEntry)) {
				dsPerror("dsMarkRead: dsAllocTables failed");
				exit(0);
			}
		}
		if (!xdr_dataset_data(&xdr, pDataset)) {
			dsPerror("dsMarkRead: xdr_dataset_data failed\n");
			exit(0);
		}
		if (!dsMarkSize(pDataset, &size)) {
			dsPerror("dsMarkRead: dsMarkSize failed\n");
			exit(0);
		}
		dataBytes += size;
		dsFreeDataset(pDataset);
		curPos = xdr_getpos(&xdr);
	}
	printf("fileSize %d, dsetCount %d, dataBytes %d, typeBytes %d\n",
		eofPos, dsCount, dataBytes, typeBytes);
}
/*****************************************************************************
*/
int dsMarkSize(DS_DATASET_T *pDataset, size_t *pSize)
{
	size_t i, size;
	DS_DATASET_T *item;
	DS_LIST_T list;
	DS_TYPE_T *type;

	dsListInit(&list);
	if (!dsVisitList(&list, pDataset)) {
		goto fail;
	}
	for (i = size = 0; i < list.count; i++) {
		item = list.pItem[i];
		if (DS_IS_TABLE(item)) {
			if (!dsTypePtr(&type, item->tid)) {
				goto fail;
			}
			size += item->elcount*type->stdsize;
		}
	}
	size = 4*((size + 3)/4);
	*pSize = size;
	return dsListFree(&list);
fail:
	dsListFree(&list);
	return FALSE;
}
/*****************************************************************************
*/
void dsMarkTypes(int argc, char *argv[])
{
	char buf[DS_MAX_SPEC_LEN+1], *fileName, *ptr, *str;
	int first = 0, i, last = INT_MAX, optChar;
	long curPos, eofPos;
	FILE *stream;
	XDR xdr;

	if (argc > 5) {
		fprintf(stderr, "Too many arguments for option %c\n", optChar);
		exit(0);
	}
	optChar = *argv[1];
	fileName = argv[2];
	if (argc > 3) {
		first = strtol(argv[3], &ptr, 10);
		if (ptr == argv[3]) {
			fprintf(stderr, "Invalid firstIndex: %s\n", argv[3]);
			exit(0);
		}
	}
	if (argc > 4) {
		last = strtol(argv[4], &ptr, 10);
		if (ptr == argv[4]) {
			fprintf(stderr, "Invalid firstIndex: %s\n", argv[4]);
			exit(0);
		}
	}
	if ((stream = fopen(fileName, "rb")) == NULL) {
		fprintf(stderr, "fopen failed for %s\n", fileName);
		exit(0);
	}
	fseek(stream, 0, SEEK_END);
	eofPos = ftell(stream);
	curPos = 0;
	fseek(stream, curPos, SEEK_SET);
	xdrstdio_create(&xdr, stream, XDR_DECODE);
	for (i = 0; i <= last && curPos < eofPos; i++) {
		if (i >= first) {
			for (;;) {
				str = buf;
				if (!xdr_string(&xdr, &str, sizeof(buf) - 1)) {
					fprintf(stderr, "dsMarkTypes: xdr_string failed\n");
					exit(0);
				}
				if (strncmp(buf, "type", 4) == 0 && optChar != 't') {
					continue;
				}
				printf("%s\n", buf);
				if (strncmp(buf, "data", 4) == 0) {
					break;
				}
			}
			if (!xdr_setpos(&xdr, curPos)) {
				fprintf(stderr, "dsMarkTypes: xdr_setpos failed\n");
				exit(0);
			}
		}
		if (!xdr_dataset_skip(&xdr)) {
			fprintf(stderr, "dsMarkTypes: xdr_dataset_skip failed\n");
			exit(0);
		}
		curPos = xdr_getpos(&xdr);
	}
	fclose(stream);
}

/*****************************************************************************
*/
void dsMarkWrite(int argc, char *argv[])
{
	char *fileName, *ptr, *str, *rowType;
	int dsetCount, endian, i, optChar, rowCount, typeChar;
	DS_DATASET_T *pDataset;
	FILE *stream;
	XDR xdr;

	if (argc < 6) {
		fprintf(stderr, "Too few arguments for option %c\n", optChar);
		exit(0);
	}
	i = 1;
	optChar = *argv[i++];
	fileName = argv[i++];
	str = argv[i++];
	if (strlen(str) != 1 || NULL == strchr("BL", endian = toupper(*str))) {
		fprintf(stderr, "dsMarkWrite: invalid endian character %s\n", str);
		exit(0);
	}
	str = argv[i++];
	dsetCount = strtol(str, &ptr, 10);
	if (ptr == str || dsetCount < 1) {
		fprintf(stderr, "dsMarkWrite: invalid dsetCount %s\n", str);
		exit(0);
	}
	if (!dsNewDataset(&pDataset, "dsmark")) {
		dsPerror("dsMarkWrite: dsNewDataset failed");
		exit(0);
	}
	while (i < argc) {
		str = argv[i++];
		rowCount = strtol(str + 1, &ptr, 10);
		typeChar = ptr != (str + 1) && ptr[1] == '\0' ? *ptr : '\0';
		switch (typeChar) {
		case 'c':
			rowType = "struct charType {char c[1024];}";
			break;

		case 'd':
			rowType = "struct doubleType {double d[128];}";
			break;

		case 'f':
			rowType = "struct floatType {float f[256];}";
			break;

		case 'l':
			rowType = "struct longType {long l[256];}";
			break;

		case 's':
			rowType = "struct shortType {short s[512];}";
			break;

		default:
			fprintf(stderr, "dsMarkWrite: invalid tSpec %s\n", str);
			exit(0);
		}
		ptr = NULL;
		if (!dsAddTable(pDataset, str, rowType, rowCount, &ptr)) {
			fprintf(stderr, "tspec: %s\n", str);
			dsPerror("dsMarkWrite: dsAddTable failed");
			exit(0);
		}
	}
	if (!dsSetDataset(pDataset)) {
		dsPerror("dsSetDataset failed");
		exit(0);
	}
	if ((stream = fopen(fileName, "wb")) == NULL) {
		fprintf(stderr, "dsMarkWrite: fopen failed for %s\n", fileName);
		exit(0);
	}
	xdrstdio_create(&xdr, stream, XDR_ENCODE);
	for (i = 0; i < dsetCount; i++) {
		if (endian == 'B') {
			if (!xdr_dataset(&xdr, &pDataset)) {
				dsPerror("dsMarkWrite: xdr_dataset failed");
				exit(0);
			}
		}
		else if (!dsEncodeLittleEndian(&xdr, pDataset)) {
			dsPerror("dsMarkWrite: dsEncodeLittleEndian failed");
			exit(0);
		}
	}
	fclose(stream);
	dsFreeDataset(pDataset);
}
/*****************************************************************************
*/
void usage(FILE *stream)
{
	fprintf(stream, "usage:\tdsmark d fileName [firstIndex [lastIndex]]\n");
	fprintf(stream, "\t\tdump dataset specifier for range of datasets\n");
	fprintf(stream, "\n\tdsmark r fileName [name1 ... nameN]\n");
	fprintf(stream, "\t\tread named datasets and tables (name .. for all)\n");
	fprintf(stream, "\n\tdsmark t fileName [firstIndex [lastIndex]]\n");
	fprintf(stream, "\t\tdump all type info for range of datasets\n");
	fprintf(stream, "\n\tdsmark w fileName endian count tSpec1 ... tSpecN\n");
	fprintf(stream, "\t\twrite count datasets to fileName, endian = b | l\n");
	fprintf(stream, "\t\ttSpec specifies table with 1 KB rows\n");
	fprintf(stream, "\t\ttSpec: <alphaChar><rowsCount><typeChar>\n");
	fprintf(stream, "\t\ttypeChar: c-char  d-double f-float l-long s-short\n");
	fprintf(stream, "\t\texample: dsmark w test.xdf b 20 f2000s m1c t3000f\n");
}

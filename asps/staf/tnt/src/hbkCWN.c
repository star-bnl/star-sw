
#include "hbkCWN.h"

#define NYI printf("Not Yet Implemented\n");return 0

static char * _buffer=NULL;

typedef struct cwn_row_t { 
	long id;
	float x,y,z;
	char name[16];
	float p[3];
}CWN_ROW_T;

CWN_ROW_T cwn_data[]={101,12.34,0.0,43.21,"particle",-1.11,2.22,3.33};

/*--------------------------------------------------------------------*/
STAFCV_T hbkCWNsetDataBuffer(long hid, size_t iblock, char *buffer)
{
	_buffer = buffer;
}

/*--------------------------------------------------------------------*/
STAFCV_T hbkCWNputRowBlock(long hid, size_t irow, size_t iblock)
{
	memcpy(cwn_data,_buffer,44);
	return STAFCV_OK;
}

/*--------------------------------------------------------------------*/
STAFCV_T hbkCWNgetRowBlock(long hid, size_t irow, size_t iblock)
{
	memcpy(_buffer,cwn_data,44);
	return STAFCV_OK;
}

/*--------------------------------------------------------------------*/
size_t hbkCWNblockCount(long hid)
{
	return 3;
}

/*--------------------------------------------------------------------*/
char * hbkCWNblockName(long hid, size_t iblock)
{
	char *names[]={"Block0","Block1","Block2"};
	return names[iblock];
}

/*--------------------------------------------------------------------*/
char * hbkCWNblockChform(long hid, size_t iblock)
{
	char *chforms[]={"id:I*4,x:R*4,y:R*4,z:R*4"
		,"name:C*16"
		,"p(3):R*4"};
	return chforms[iblock];
}

/*--------------------------------------------------------------------*/
size_t hbkCWNblockOffset(long hid, size_t iblock)
{
	size_t offsets[]={0,16,32};
	return offsets[iblock];
}

/*--------------------------------------------------------------------*/
size_t hbkCWNblockSize(long hid, size_t iblock)
{
	size_t sizes[]={16,16,12};
	return sizes[iblock];
}

/*--------------------------------------------------------------------*/
unsigned char hbkCWNblockIsChar(long hid, size_t iblock)
{
	size_t ischars[]={0,1,0};
	return ischars[iblock];
}

/*--------------------------------------------------------------------*/
STAFCV_T hbkCWNclear(long hid)
{
NYI;
}

/*--------------------------------------------------------------------*/
size_t hbkCWNcolumnCount(long hid)
{
	return 6;
}

/*--------------------------------------------------------------------*/
size_t hbkCWNcolumnSize(long hid, size_t icolumn)
{
	size_t sizes[]={4,4,4,4,16,12};
	return sizes[icolumn];
}

/*--------------------------------------------------------------------*/
size_t hbkCWNcolumnOffset(long hid, size_t icolumn)
{
	size_t offsets[]={0,4,8,12,16,32};
	return offsets[icolumn];
}

/*--------------------------------------------------------------------*/
char * hbkCWNcolumnTag(long hid, size_t icolumn)
{
	char * tags[]={"id","x","y","z","name","p"};
	return tags[icolumn];
}

/*--------------------------------------------------------------------*/
NT_TYPE_CODE_T hbkCWNcolumnType(long hid, size_t icolumn)
{
	NT_TYPE_CODE_T types[]={NT_TYPE_LONG,NT_TYPE_FLOAT,NT_TYPE_FLOAT,NT_TYPE_FLOAT,NT_TYPE_CHAR,NT_TYPE_FLOAT};
	return types[icolumn];
}

/*--------------------------------------------------------------------*/
size_t hbkCWNcolumnDimension(long hid, size_t icolumn)
{
	size_t dims[]={1,1,1,1,16,3};
	return dims[icolumn];
}

/*--------------------------------------------------------------------*/
char * hbkCWNcolumnChform(long hid, size_t icolumn)
{
	char *chforms[]={"id:I*4","x:R*4","y:R*4","z:R*4"
		,"name:C*16"
		,"p(3):R*4"};
	return chforms[icolumn];
}

/*--------------------------------------------------------------------*/
char * hbkCWNtitle(long hid)
{
	return "dummy CWN";
}

/*--------------------------------------------------------------------*/
size_t hbkCWNentryCount(long hid)
{
	return 1;
}


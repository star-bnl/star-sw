#ifndef HBNCWN_H
#define HBNCWN_H

#include <sys/types.h>
#include "emlLib.h"

#ifdef __cplusplus
extern "C" 
{
#endif
  
#ifndef NT_TYPE_CODE_T
typedef enum nt_type_code_t 
{
  NT_TYPE_CHAR,
  NT_TYPE_LOGICAL,
  NT_TYPE_LONG,
  NT_TYPE_U_LONG,
  NT_TYPE_LONGLONG,
  NT_TYPE_U_LONGLONG,
  NT_TYPE_FLOAT,
  NT_TYPE_DOUBLE,
  NT_TYPE_UNKNOWN
} NT_TYPE_CODE_T;
#endif /* NT_TYPE_CODE_T */
  
STAFCV_T hbkCWNbook(long, char *);
STAFCV_T hbkCWNhprnt(long);
STAFCV_T hbkCWNcharBlock(long, char *, char *, char *);
STAFCV_T hbkCWNscalarBlock(long, char *, int *, char *);
size_t hbkCWNblockCount(long hid);
char * hbkCWNblockName(long hid, size_t iblock);
char * hbkCWNblockChform(long hid, size_t iblock);
size_t hbkCWNblockOffset(long hid, size_t iblock);
size_t hbkCWNblockSize(long hid, size_t iblock);
unsigned char hbkCWNblockIsChar(long hid, size_t iblock);
size_t hbkCWNcolumnCount(long hid);
size_t hbkCWNcolumnSize(long hid, size_t icolumn);
size_t hbkCWNcolumnOffset(long hid, size_t icolumn);
char * hbkCWNcolumnTag(long hid, size_t icolumn);
size_t hbkCWNcolumnDimension(long hid, size_t icolumn);
char * hbkCWNcolumnChform(long hid, size_t icolumn);
char * hbkCWNtitle(long hid);
size_t hbkCWNentryCount(long hid);
STAFCV_T hbkCWNsetDataBuffer(long hid, size_t iblock, char *buffer);
STAFCV_T hbkCWNputRowBlock(long hid, size_t irow, size_t iblock);
STAFCV_T hbkCWNputRow(long hid);
STAFCV_T hbkCWNgetRowBlock(long hid, size_t irow, size_t iblock);
NT_TYPE_CODE_T hbkCWNcolumnType(long hid, size_t icolumn);
STAFCV_T hbkCWNclear(long hid);


#ifdef __cplusplus
}
#endif

#endif /* HBNCWN_H */

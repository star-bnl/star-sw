#include <stdio.h>
#include "dstype.h"

#include "asuLib.h"
#include "emlLib.h"

void dsuPrintData(FILE *stream, DS_TYPE_CODE_T code
		, unsigned int count, void *data)
{
   size_t i;
   DS_PTR_UNION_T p;
   
   p.v = data;
   
   switch(code) {

      case DS_TYPE_CHAR:
	 fprintf( stream,  "\t");
         for (i = 0; i < count; i++) {
            fprintf( stream,  "%c", p.c[i]);
	 }
         break;
         
      case DS_TYPE_OCTET:
         for (i = 0; i < count; i++) {
            fprintf( stream,  "\t%u", p.o[i]);
         }
         break;
         
      case DS_TYPE_SHORT:
         for (i = 0; i < count; i++) {
            fprintf( stream,  "\t%hd", p.s[i]);
         }
         break;
         
      case DS_TYPE_U_SHORT:
         for (i = 0; i < count; i++) {
            fprintf( stream,  "\t%hu", p.us[i]);
         }
         break;
         
      case DS_TYPE_LONG:
         for (i = 0; i < count; i++) {
            fprintf( stream,  "\t%ld", p.l[i]);
         }
         break;
         
      case DS_TYPE_U_LONG:
         for (i = 0; i < count; i++) {
            fprintf( stream,  "\t%lu", p.ul[i]);
         }
         break;
         
      case DS_TYPE_FLOAT:
         for (i = 0; i < count; i++) {
            fprintf( stream,  "\t%g", p.f[i]);
         }
         break;
         
      case DS_TYPE_DOUBLE:
         for (i = 0; i < count; i++) {
            fprintf( stream,  "\t%g", p.d[i]);
         }
         break;
         
      case DS_TYPE_STRUCT:
         fprintf( stream,  "\tNOT_YET_IMPLEMENTED");
         break;
         
      default:
         fprintf( stream,  "\tINVALID_TYPE");
         break;
   }
}

/*--------------------------------------------------------------------*/
void dsuPrintHeader(FILE *stream, DS_DATASET_T *pTable)
{
   fprintf(stream,"NOT_YET_IMPLEMENTED\n");
}


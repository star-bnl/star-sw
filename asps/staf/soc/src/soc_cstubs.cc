/*:
*:>---------------------------------------------------------------------
*:FILE:         soc_cstubs.cc
*:DESCRIPTION:  
*:AUTHOR:       Dave Morrison
*:BUGS:         
*:HISTORY:      
*:<---------------------------------------------------------------------
*/
#undef CORBA

/*-------------------------------------------- INCLUDES             --*/

#include <stdlib.h>
#include <stdio.h>

#define KUIP
#include "asuAlloc.h"
#include "asuLib.h"
#include "emlLib.h"
#include "socLib.h"

#include "kuip.h"

/*-------------------------------------------- TYPEDEFS             --*/
/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/

STAFCV_T 
soc_bind(char* pkgName, char* solibName)
{
  // Loading of specific shared library not yet implemented ...
  static void *ps = &solibName;

  if (0 != soc->bind(pkgName)) {
    EML_SUCCESS(STAFCV_OK);
  }
  else {
    EML_FAILURE(NO_DYNAMIC_LINKING);
  }
}

STAFCV_T
soc_release(char * aspName)
{
   if( 0 != soc->release(aspName) ){
      EML_SUCCESS(STAFCV_OK);
   }
   else {
      EML_FAILURE(NO_DYNAMIC_LINKING);
   }
}

STAFCV_T 
socobject_name(long idref)
{
   if( !VALID_IDREF(idref) ){
      EML_FAILURE(INVALID_IDREF);
   }

   socObject* p;
   if( !(NULL != (p=soc->getObject(idref))) ){
      EML_FAILURE(OBJECT_NOT_FOUND);
   }

   printf("SOC:\tObject name = %s \n",p->Name());
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
socobject_type(long idref)
{
   if( !VALID_IDREF(idref) ){
      EML_FAILURE(INVALID_IDREF);
   }

   socObject* p;
   if( !(NULL != (p=soc->getObject(idref))) ){
      EML_FAILURE(OBJECT_NOT_FOUND);
   }

   char *t;
   printf("SOC:\tObject type = %s \n",t=p->type());
   FREE(t);
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
socobject_version(long idref)
{
   if( !VALID_IDREF(idref) ){
      EML_FAILURE(INVALID_IDREF);
   }

   socObject* p;
   if( !(NULL != (p=soc->getObject(idref))) ){
      EML_FAILURE(OBJECT_NOT_FOUND);
   }

   char *t;
   printf("SOC:\tObject version = %s \n",t=p->version());
   /* delete[] t; */
   FREE (t);  /* alloc_conflict -akio*/
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
soc_count()
{
   long i = soc->count();
   printf("SOC:\tObject count = %ld \n",i);
   float f=i; set_staf_result(f);
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
soc_deleteid(long id)
{
   soc->deleteID(id);
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
soc_deleteoid(long id)
{
   soc->deleteID(id);
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
soc_deleteobject(char* name, char* type)
{
   soc->deleteObject(name,type);
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
soc_idobject(char* name, char* type)
{
   IDREF_T id;
   if( !soc->idObject(name,type,id) ){
      EML_FAILURE(INVALID_IDREF);
   }
   printf("SOC:\tObject idRef =  %ld \n",id);
   float f = id; set_staf_result(f);
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
soc_list()
{
   char *c;
   c = soc->list();
   fputs(c,stdout);
   FREE(c);                        
   EML_SUCCESS(STAFCV_OK);        
}

STAFCV_T 
soc_newobject(char* name)
{
   soc->newObject(name);

   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
socobject_lock(long idref, char l)
{
   if( !VALID_IDREF(idref) ){
      EML_FAILURE(INVALID_IDREF);
   }

   socObject* p;
   if( !(NULL != (p=soc->getObject(idref))) ){
      EML_FAILURE(OBJECT_NOT_FOUND);
   }

   unsigned char lorig = p->lock();
   switch (l) {
   case 'T': case 't':
      p->lock(TRUE);
      break;
   case 'F': case 'f':
      p->lock(FALSE);
      break;
   case '-':
   default:
      if( lorig ){ printf("SOC:\tObject lock = TRUE \n"); }
      else       { printf("SOC:\tObject lock = FALSE \n"); }
      break;
   }

   float f = lorig; set_staf_result(f);
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
socobject_implements(long idref, char* iface)
{
   if( !VALID_IDREF(idref) ){
      EML_FAILURE(INVALID_IDREF);
   }

   socObject* p;
   if( !(NULL != (p=soc->getObject(idref))) ){
      EML_FAILURE(OBJECT_NOT_FOUND);
   }
   if( p->implementsInterface(iface) ){
      printf("SOC:\tObject (%s) DOES implement (%s) \n",p->Name()
		,iface);
      set_staf_result(1.0);
   }
   else {
      printf("SOC:\tObject (%s) DOES NOT implement (%s) \n",p->Name()
		,iface);
      set_staf_result(0.0);
   }

   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
socobject_delete(char* name, char* type)
{
   soc->deleteObject(name,type);
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
socobject_oid(char* name, char* type)
{
   IDREF_T id;
   if( !soc->idObject(name,type,id) ){
      EML_FAILURE(INVALID_IDREF);
   }
   printf("SOC:\tObject idRef =  %ld \n",id);
   float f = id; set_staf_result(f);
   EML_SUCCESS(STAFCV_OK);
}


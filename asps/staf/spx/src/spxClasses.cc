//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:        spxClasses.cc
//:DESCRIPTION: Implementation classes for SPX.
//:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:        -- STILL IN DEVELOPMENT --
//:HISTORY:     25oct95-v000a-cet- creation
//:<--------------------------------------------------------------------

//:----------------------------------------------- INCLUDES           --
#include <stream.h>
#include <string.h>
#include <time.h>

#include "asuAlloc.h"
#include "asuLib.h"
#include "emlLib.h"
#include "socLib.h"
#include "spxClasses.hh"

//:<---------------------------------------------- MACROS             --
#include "spx_macros.h"

//extern "C" void spx_static();		// DEBUG HACK !!!!!!!!!!!!!!!!!

//:#####################################################################
//:=============================================== CLASS              ==
//: spxDummy

//:----------------------------------------------- CTORS & DTOR       --
spxDummy:: spxDummy(const char* name) 
	: socObject(name, "spxDummy") {
   myPtr = (SOC_PTR_T)this;
   myNCalls = 0;
}

//----------------------------------
spxDummy:: ~spxDummy() {
}

//:----------------------------------------------- ATTRIBUTES         --
long spxDummy::  nCalls () {
   myNCalls++;
   return myNCalls;
}

//----------------------------------
char * spxDummy::  listing () {
   myNCalls++;
   char* c = socObject::listing();
   char* cc = NULL;
   cc = (char*)MALLOC(79+100);
   sprintf(cc,"%s %d calls",c,nCalls());
   FREE(c);
   return cc;
}

//:----------------------------------------------- PUB FUNCTIONS      --
//- override socObject::implementsInterface
unsigned char spxDummy :: implementsInterface (const char * iface) {
   if( 0 == strcmp("spxDummy",iface)
   ||  socObject::implementsInterface(iface)
   ){ return TRUE; }
   return FALSE;
}

//----------------------------------
STAFCV_T spxDummy:: null () {
   myNCalls++;
//spx_static();			// DEBUG HACK !!!!!!!!!!!!!!!!!!!!!!!!!
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T spxDummy:: getTime (char *& tim) {
   myNCalls++;
   time_t it = time(0);
   char* c=NULL;
   c = (char*)MALLOC(strlen(ctime(&it)) +1);
   strcpy(c,ctime(&it));
   tim = (char *)c;
   EML_SUCCESS(STAFCV_OK);
}

//:----------------------------------------------- PRIV FUNCTIONS     --
//:**NONE**

//:#####################################################################
//:=============================================== CLASS              ==
//: spxGrid

//:----------------------------------------------- CTORS & DTOR       --
spxGrid:: spxGrid(const char* name, const short h, const short w) 
	: socObject(name, "spxGrid") {
   myPtr = (SOC_PTR_T)this;
   myHeight = h;
   myWidth = w;
   myGrid = new long* [h];
   for( int i=0;i<h;i++ ){
      myGrid[i] = new long[w];
   }
}

//----------------------------------
spxGrid:: ~spxGrid() {
   for( int i=0;i<myHeight;i++ ){
      delete[] myGrid[i];
   }
   delete[] myGrid;
}

//:----------------------------------------------- ATTRIBUTES         --
short spxGrid::  height () {
   return myHeight;
}

//----------------------------------
short spxGrid::  width () {
   return myWidth;
}

//----------------------------------
char * spxGrid::  listing () {
   char* c = socObject::listing();
   char* cc = NULL;
   cc = (char*)MALLOC(79+100);
   memset(cc,0,79);
   sprintf(cc,"%s Size = (%d, %d)",c,height(),width());
   FREE(c);
   return cc;
}

//:----------------------------------------------- PUB FUNCTIONS      --
//- override socObject::implementsInterface
unsigned char spxGrid :: implementsInterface (const char * iface) {
   if( 0 == strcmp("spxGrid",iface)
   ||  socObject::implementsInterface(iface)
   ){ return TRUE; }
   return FALSE;
}

STAFCV_T spxGrid:: get (short n, short m, long& value) {
   if(n<0||n>myHeight||m<0||m>myHeight)EML_ERROR(INVALID_GRIDCELL);
   value = myGrid[n][m];
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T spxGrid:: set (short n, short m, long value) {
   if(n<0||n>myHeight||m<0||m>myHeight)EML_ERROR(INVALID_GRIDCELL);
   myGrid[n][m] = value;
   EML_SUCCESS(STAFCV_OK);
}

//:----------------------------------------------- PRIV FUNCTIONS     --
//:**NONE**

//:#####################################################################
//:=============================================== CLASS              ==
//: spxFactory

//:----------------------------------------------- CTORS & DTOR       --
spxFactory:: spxFactory(const char * name) 
		: socFactory()
		, socObject(name,"spxFactory") {
   myPtr = (SOC_PTR_T)this;
   lock(TRUE);
}

//----------------------------------
spxFactory:: ~spxFactory() {
}

//:----------------------------------------------- ATTRIBUTES         --
//**NONE**

//:----------------------------------------------- PUB FUNCTIONS      --
//- override socObject::implementsInterface
unsigned char spxFactory :: implementsInterface (const char * iface) {
   if( 0 == strcmp("spxFactory",iface)
   ||  socFactory::implementsInterface(iface)
   ){ return TRUE; }
   return FALSE;
}

STAFCV_T spxFactory:: deleteDummy (const char * name) {
   IDREF_T id;

   if( !soc->idObject(name,"spxDummy",id)
   ||  !deleteEntry(id)
   ||  !soc->deleteID(id)
   ){
      EML_ERROR(CANT_DELETE_OBJECT);
   }
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T spxFactory:: deleteGrid (const char * name) {
   IDREF_T id;

   if( !soc->idObject(name,"spxGrid",id)
   ||  !deleteEntry(id)
   ||  !soc->deleteID(id)
   ){
      EML_ERROR(CANT_DELETE_OBJECT);
   }
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T spxFactory:: findDummy (const char * name
		, spxDummy*& dummy) {
   socObject* obj;
   if( NULL == (obj = soc->findObject(name,"spxDummy")) ){
      dummy = NULL;
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   dummy = (spxDummy*)(obj->ptr());
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T spxFactory:: findGrid (const char * name
		, spxGrid*& grid) {
   socObject* obj;
   if( NULL == (obj = soc->findObject(name,"spxGrid")) ){
      grid = NULL;
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   grid = (spxGrid*)(obj->ptr());
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T spxFactory:: getDummy (IDREF_T id, spxDummy*& dummy) {
   socObject* obj;
   if( NULL == (obj = soc->getObject(id)) ){
      dummy = NULL;
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   if( 0 != strcmp(obj->type(),"spxDummy") ){
      dummy = NULL;
      EML_ERROR(WRONG_OBJECT_TYPE);
   }
   dummy = (spxDummy*)(obj->ptr());
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T spxFactory:: getGrid (IDREF_T id, spxGrid*& grid) {
   socObject* obj;
   if( NULL == (obj = soc->getObject(id)) ){
      grid = NULL;
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   if( 0 != strcmp(obj->type(),"spxGrid") ){
      grid = NULL;
      EML_ERROR(WRONG_OBJECT_TYPE);
   }
   grid = (spxGrid*)(obj->ptr());
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
//- OVER-RIDE socFactory:: list()
char * spxFactory:: list () {
   char tit[] =
		"\n"
                "+-------------------------------------------"
                "-----------------------------------\n"
                "|******************* "
                "SPX - Service Package eXample listing"
                " ********************\n"
                "%s\n";
   
   char *c = socFactory::list();

   char *cc = (char*)MALLOC(strlen(c) +1 +strlen(tit));

   sprintf(cc,tit,c);
   FREE(c);
   return cc;
}

//----------------------------------
STAFCV_T spxFactory:: newDummy (const char * name) {
   IDREF_T id;
   if( soc->idObject(name,"spxDummy",id) ){
      EML_ERROR(DUPLICATE_OBJECT_NAME);
   }
   spxDummy* p;
   p = new spxDummy(name);
   if( !soc->idObject(name,"spxDummy",id) ){
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   addEntry(id);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T spxFactory:: newGrid (const char * name
		, short height, short width) {
   IDREF_T id;
   if( soc->idObject(name,"spxGrid",id) ){
      EML_ERROR(DUPLICATE_OBJECT_NAME);
   }
   spxGrid* p;
   p = new spxGrid(name,height,width);
   if( !soc->idObject(name,"spxGrid",id) ){
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   addEntry(id);
   EML_SUCCESS(STAFCV_OK);
}

//:----------------------------------------------- PRIV FUNCTIONS     --
//:**NONE**


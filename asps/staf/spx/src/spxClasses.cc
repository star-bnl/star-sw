//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:        spxClasses.C
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

//:----------------------------------------------- PUB FUNCTIONS      --
STAFCV_T spxDummy:: hello (const char * message) {
   myNCalls++;
   printf(" %s \n",message);
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

//----------------------------------
STAFCV_T spxDummy:: null () {
   myNCalls++;
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

//----------------------------------
STAFCV_T spxDummy:: getTime (char *& tim) {
   myNCalls++;
   time_t it = time(0);
   char* c=NULL;
   c = (char*)ASUALLOC(strlen(ctime(&it)) +1);
   strcpy(c,ctime(&it));
   tim = (char *)c;
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

//:----------------------------------------------- PRIV FUNCTIONS     --
//:**NONE**

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

//:----------------------------------------------- PUB FUNCTIONS      --
STAFCV_T spxGrid:: get (short n, short m, long& value) {
   if(n<0||n>myHeight||m<0||m>myHeight)EML_ERROR(INVALID_GRIDCELL);
   value = myGrid[n][m];
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

//----------------------------------
STAFCV_T spxGrid:: set (short n, short m, long value) {
   if(n<0||n>myHeight||m<0||m>myHeight)EML_ERROR(INVALID_GRIDCELL);
   myGrid[n][m] = value;
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

//:----------------------------------------------- PRIV FUNCTIONS     --
//:**NONE**

//:=============================================== CLASS              ==
//: spxManager

//:----------------------------------------------- CTORS & DTOR       --
spxManager:: spxManager(const char * name) 
		: socFactory()
		, socObject(name,"spxManager") {
   myPtr = (SOC_PTR_T)this;
   lock(TRUE);
}

//----------------------------------
spxManager:: ~spxManager() {
}

//:----------------------------------------------- ATTRIBUTES         --
//**NONE**

//:----------------------------------------------- PUB FUNCTIONS      --
STAFCV_T spxManager:: deleteDummy (const char * name) {
   IDREF_T id;

   if( !soc->idObject(name,"spxDummy",id)
   ||  !deleteEntry(id)
   ||  !soc->deleteID(id)
   ){
      EML_ERROR(CANT_DELETE_OBJECT);
   }
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

//----------------------------------
STAFCV_T spxManager:: deleteGrid (const char * name) {
   IDREF_T id;

   if( !soc->idObject(name,"spxGrid",id)
   ||  !deleteEntry(id)
   ||  !soc->deleteID(id)
   ){
      EML_ERROR(CANT_DELETE_OBJECT);
   }
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

//----------------------------------
STAFCV_T spxManager:: findDummy (const char * name
		, spxDummy*& dummy) {
   socObject* obj;
   if( !soc->findObject(name,"spxDummy",obj) ){
      dummy = NULL;
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   dummy = (spxDummy*)(obj->ptr());
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

//----------------------------------
STAFCV_T spxManager:: findGrid (const char * name
		, spxGrid*& grid) {
   socObject* obj;
   if( !soc->findObject(name,"spxGrid",obj) ){
      grid = NULL;
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   grid = (spxGrid*)(obj->ptr());
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

//----------------------------------
STAFCV_T spxManager:: getDummy (IDREF_T id, spxDummy*& dummy) {
   socObject* obj;
   if( !soc->getObject(id,obj) ){
      dummy = NULL;
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   if( 0 != strcmp(obj->type(),"spxDummy") ){
      dummy = NULL;
      EML_ERROR(WRONG_OBJECT_TYPE);
   }
   dummy = (spxDummy*)(obj->ptr());
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

//----------------------------------
STAFCV_T spxManager:: getGrid (IDREF_T id, spxGrid*& grid) {
   socObject* obj;
   if( !soc->getObject(id,obj) ){
      grid = NULL;
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   if( 0 != strcmp(obj->type(),"spxGrid") ){
      grid = NULL;
      EML_ERROR(WRONG_OBJECT_TYPE);
   }
   grid = (spxGrid*)(obj->ptr());
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

//----------------------------------
STAFCV_T spxManager:: list () {

   socObject* obj;

   printf("\n"
"**********************************************************************"
   "\n"
"**************** SPX - Service Package eXample listing ***************"
   "\n"
"**********************************************************************"
   "\n"
"* IDREF * NAME            * TYPE            *                         "
    "\n"
"**********************************************************************"
    "\n");
   for( int i=0;i<myCount;i++ ){
      if( soc->getObject(entry(i),obj) ){
	 if( 0 == strcmp("spxGrid",obj->type()) ){
	    printf("* %5d * %-15s * %-15s * Size = (%d,%d) \n"
	    		,obj->idRef(),obj->name(),obj->type()
            		,SPXGRID(obj)->height()
            		,SPXGRID(obj)->width());
	 } else {
	    printf("* %5d * %-15s * %-15s * Calls = %d \n"
	    		,obj->idRef(),obj->name(),obj->type()
            		,SPXDUMMY(obj)->nCalls());
	 }
      } else {
         printf("* %5d * %-15s * %-15s * \n"
                        ,entry(i),"**DELETED**","**DELETED**");
      }
   }
   printf(
"**********************************************************************"
   "\n\n");

   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

//----------------------------------
STAFCV_T spxManager:: newDummy (const char * name) {
   IDREF_T id;
   if( soc->idObject(name,"spxDummy",id) ){
      EML_ERROR(DUPLICATE_OBJECT_NAME);
   }
   static spxDummy* p;
   p = new spxDummy(name);
   if( !soc->idObject(name,"spxDummy",id) ){
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   addEntry(id);
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

//----------------------------------
STAFCV_T spxManager:: newGrid (const char * name
		, short height, short width) {
   IDREF_T id;
   if( soc->idObject(name,"spxGrid",id) ){
      EML_ERROR(DUPLICATE_OBJECT_NAME);
   }
   static spxGrid* p;
   p = new spxGrid(name,height,width);
   if( !soc->idObject(name,"spxGrid",id) ){
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   addEntry(id);
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

//:----------------------------------------------- PRIV FUNCTIONS     --
//:**NONE**


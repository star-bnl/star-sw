//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:        dioClasses.C
//:DESCRIPTION: DIO Classes
//:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:        -- STILL IN DEVELOPMENT --
//:HISTORY:     12dec95-v000a-cet- creation
//:<--------------------------------------------------------------------

//:----------------------------------------------- INCLUDES           --
#include <string.h>
#include "asuAlloc.h"
#include "dioClasses.hh"

//:----------------------------------------------- MACROS             --
#include "dio_macros.h"

//:=============================================== CLASS              ==
// dioStream
//:----------------------------------------------- CTORS & DTOR       --
dioStream:: dioStream()
		: socObject() {
   myMode = DIO_UNKNOWN_MODE;
   myState = DIO_UNKNOWN_STATE;
}

//----------------------------------
dioStream:: ~dioStream() {
}

//:----------------------------------------------- ATTRIBUTES         --
DIO_MODE_T dioStream::  mode () {
   return myMode;
}

//----------------------------------
DIO_STATE_T dioStream::  state () {
   return myState;
}

//:----------------------------------------------- PUB FUNCTIONS      --
STAFCV_T dioStream:: close () {
   EML_ERROR(PURE_VIRTUAL);
}

STAFCV_T dioStream:: getEvent (tdmDataset* destination) {
   EML_ERROR(PURE_VIRTUAL);
}

STAFCV_T dioStream:: open (DIO_MODE_T mode) {
   EML_ERROR(PURE_VIRTUAL);
}

STAFCV_T dioStream:: putEvent (tdmDataset* source) {
   EML_ERROR(PURE_VIRTUAL);
}

//:----------------------------------------------- PROT FUNCTIONS     --
//:----------------------------------------------- PRIV FUNCTIONS     --

//:=============================================== CLASS              ==
// dioFileStream

//:----------------------------------------------- CTORS & DTOR       --
dioFileStream:: dioFileStream(const char * name, const char * fileName)
		: dioStream()
		, socObject(name, "dioFileStream") {
   myPtr = (SOC_PTR_T)this;
   myFileName = (char*)ASUALLOC(strlen(fileName) +1);
   strcpy(myFileName,fileName);
}

//----------------------------------
dioFileStream:: ~dioFileStream() {
   ASUFREE(myFileName);
}

//:----------------------------------------------- ATTRIBUTES         --
char * dioFileStream::  fileName () {
   char * c = (char*)ASUALLOC(strlen(myFileName) +1);
   strcpy(c,myFileName);
   return c;
}

//:----------------------------------------------- PUB FUNCTIONS      --
STAFCV_T dioFileStream:: close () {

   DIO_STATE_T saveState=myState;

/*- Close file and destroy xdr. -*/
   if( &myXDR != NULL ) XDR_DESTROY(&myXDR);
   fclose(myFile);

   myState = DIO_CLOSE_STATE;
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T dioFileStream:: getEvent (tdmDataset* destination) {

   DIO_STATE_T saveState=myState;

   if( !(DIO_OPEN_STATE == myState)
   ||  !(DIO_READ_MODE == myMode || DIO_UPDATE_MODE == myMode)
   ){
      EML_ERROR(BAD_MODE_OR_STATE);
   }
   myState = DIO_READ_STATE;

EML_PRINTF(" Get pointer to destination dataset. \n");
   DSL_PTR_T ddd;
   if( !destination->cvtDslPointer(ddd) ){
      EML_ERROR(BAD_DATASET);
   }
EML_PRINTF(" Convert pointer to DS_DATASET_T. \n");
   DS_DATASET_T *pDest=(DS_DATASET_T*)ddd;

EML_PRINTF(" Read data here. \n");
DS_DATASET_T *pDS=NULL;
bool_t result;
   if( !xdr_dataset_type(&myXDR, &pDS,0)	/* get descriptor */
   ||  !dsIsDataset(&result,pDS) ||  !result	/* sanity check */
   ||  !dio_mapHierarchy(pDest,pDS)		/* map onto memory */
   ||  !dsAllocTables(pDS)			/* no-opt */
   ||  !xdr_dataset_data(&myXDR,pDS)		/* get data */
   ||  !dsFreeDataset(pDS)			/* discard input DS */
   ){
      myState = saveState;
      EML_ERROR(CANT_READ_FROM_STREAM);
   }

   myState = saveState;
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T dioFileStream:: open (DIO_MODE_T mode) {

   xdr_op xdr_mode;
   char* fo_mode="rb";

//- Set XDR & fopen modes -**
   switch (mode){
   case DIO_READ_MODE:
      xdr_mode = XDR_DECODE;
      strcpy(fo_mode,"rb");
      break;
   case DIO_WRITE_MODE:
      xdr_mode = XDR_ENCODE;
      strcpy(fo_mode,"wb");
      break;
   case DIO_UPDATE_MODE:	// Update not yet implimented
   case DIO_UNKNOWN_MODE:
   default:
      EML_ERROR(INVALID_MODE);
      break;
   }

//- Open file. -**
   if( (myFile = fopen(myFileName, fo_mode)) == NULL ){
      EML_ERROR(CANT_OPEN_FILE);
   }

//- Create XDR pointer. -**
   xdrstdio_create(&myXDR, myFile, xdr_mode);
//-? if( myXDR == NULL ) EML_ERROR(BAD_XDR); -??

   myMode = mode;
   myState = DIO_OPEN_STATE;
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T dioFileStream:: putEvent (tdmDataset* source) {
   if( !(DIO_OPEN_STATE == myState)
   ||  !(DIO_WRITE_MODE == myMode || DIO_UPDATE_MODE == myMode)
   ){
      EML_ERROR(BAD_MODE_OR_STATE);
   }
   myState = DIO_WRITE_STATE;

//- Get pointer to source dataset -**
   DSL_PTR_T ddd;
   if( !source->cvtDslPointer(ddd) ){
      EML_ERROR(BAD_DATASET);
   }
   DS_DATASET_T *pSour=(DS_DATASET_T*)ddd;

//- Write data here -**
   if( !xdr_dataset(&myXDR, &pSour) ){
      myState = DIO_OPEN_STATE;
      EML_ERROR(ERROR_WRITING_DATASET);
   }

   myState = DIO_OPEN_STATE;
   EML_SUCCESS(STAFCV_OK);
}

//:----------------------------------------------- PRIV FUNCTIONS     --
// **NONE**

// ---------------------------------------------------------------------

//:=============================================== CLASS              ==
// dioTapeStream
//:----------------------------------------------- CTORS & DTOR       --
dioTapeStream:: dioTapeStream(const char * name, const char * tape)
		: dioStream()
		, socObject(name,"dioTapeStream") {
   myPtr = (SOC_PTR_T)this;
   myTapeName = (char*)ASUALLOC(strlen(tape) +1);
   strcpy(myTapeName,tape);
}
dioTapeStream:: ~dioTapeStream() {
   ASUFREE(myTapeName);
}

//:----------------------------------------------- ATTRIBUTES         --
char * dioTapeStream::  tapeName () {
   char * c = (char*)ASUALLOC(strlen(myTapeName) +1);
   strcpy(c,myTapeName);
   return c;
}

//----------------------------------
void dioTapeStream:: bufferSize (long bufferSize) {
   myBufferSize = bufferSize;
}

//----------------------------------
long dioTapeStream::  bufferSize () {
   return myBufferSize;
}

//:----------------------------------------------- PUB FUNCTIONS      --
//:----------------------------------------------- PROT FUNCTIONS     --
//:----------------------------------------------- PRIV FUNCTIONS     --

//:=============================================== CLASS              ==
// dioSockStream
//:----------------------------------------------- CTORS & DTOR       --
dioSockStream:: dioSockStream(const char * name, long sock)
		: dioStream()
		, socObject(name,"dioSockStream") {
   myPtr = (SOC_PTR_T)this;
   mySocket = sock;
}
dioSockStream:: ~dioSockStream() {
}

//:----------------------------------------------- ATTRIBUTES         --
long dioSockStream::  socket () {
   return mySocket;
}

//----------------------------------
void dioSockStream:: bufferSize (long bufferSize) {
   myBufferSize = bufferSize;
}

//----------------------------------
long dioSockStream::  bufferSize () {
   return myBufferSize;
}

//:----------------------------------------------- PUB FUNCTIONS      --
//:----------------------------------------------- PROT FUNCTIONS     --
//:----------------------------------------------- PRIV FUNCTIONS     --

//:=============================================== CLASS              ==
// dioFactory

//:----------------------------------------------- CTORS & DTOR       --
dioFactory:: dioFactory(const char * name)
		: socFactory()
		, socObject(name, "dioFactory") {
   myPtr = (SOC_PTR_T)this;
   lock(TRUE);
}

//----------------------------------
dioFactory:: ~dioFactory() {
}

//:----------------------------------------------- ATTRIBUTES         --
//:**NONE**

//:----------------------------------------------- PUB FUNCTIONS      --
STAFCV_T dioFactory:: deleteFileStream (const char * name ){
   if( !soc->deleteObject(name,"dioFileStream") ){
      EML_ERROR(CANT_DELETE_OBJECT);
   }
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T dioFactory:: findFileStream (const char * name
		, dioFileStream*& fileStream ){
   socObject* obj;
   if( !soc->findObject(name,"dioFileStream",obj) ){
      fileStream = NULL;
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   fileStream = DIOFILESTREAM(obj);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T dioFactory:: getFileStream (IDREF_T id
		, dioFileStream*& fileStream ){
   socObject* obj;
   if( !soc->getObject(id,obj) ){
      fileStream = NULL;
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   if( 0 != strcmp(obj->type(),"dioFileStream") ){
      fileStream = NULL;
      EML_ERROR(WRONG_OBJECT_TYPE);
   }
   fileStream = DIOFILESTREAM(obj);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
char * dioFactory:: list (){
   socObject* obj;

   printf("\n"
"+---------------------------------------------------------------------"
   "\n"
"|**************** DIO - Dataset Input/Output listing *****************"
   "\n"
"+-------+-----------------+-----------------+-------------------------"
   "\n"
"| IDREF | NAME            | TYPE            | MODE & STATE            "
   "\n"
"+-------+-----------------+-----------------+-------------------------"
    "\n");
   for( int i=0;i<count();i++ ){
      if( soc->getObject(entry(i),obj) ){
         if( 0 == strcmp("dioFileStream",obj->type()) ){
            printf("| %5d | %-15s | %-15s | %s %s \n"
                        ,obj->idRef(),obj->name(),obj->type()
			, dio_mode2text(DIOFILESTREAM(obj)->mode())
			, dio_state2text(DIOFILESTREAM(obj)->state())
                        );
         } else if( 0 == strcmp("dioTestStream",obj->type()) ){
            printf("| %5d | %-15s | %-15s | \n"
                        ,obj->idRef(),obj->name(),obj->type()
                        );
         }
      } else {
         printf("| %5d | %-15s | %-15s | \n"
                        ,entry(i),"**DELETED**","**DELETED**");
      }
   }
   printf(
"+-------+-----------------+-----------------+-------------------------"
   "\n\n");

   return ""; // TEMPORARY HACK
}

//----------------------------------
STAFCV_T dioFactory:: newFileStream (const char * name
		, const char * fileName) {
   IDREF_T id;
   if( soc->idObject(name,"dioFileStream",id) ){
      EML_ERROR(DUPLICATE_OBJECT_NAME);
   }
   static dioFileStream* p;
   p = new dioFileStream(name,fileName);
   if( !soc->idObject(name,"dioFileStream",id) ){
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   addEntry(id);
/***
   if( !p->open(mode) ){
      p->close();
      soc->deleteObject(name,"dioFileStream");
      EML_ERROR(CANT_OPEN_FILE);
   }
***/
   EML_SUCCESS(STAFCV_OK);

}

//:----------------------------------------------- PRIV FUNCTIONS     --

// ---------------------------------------------------------------------


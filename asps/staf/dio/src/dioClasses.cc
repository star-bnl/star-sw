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
// dioFileStream

//:----------------------------------------------- CTORS & DTOR       --
dioFileStream:: dioFileStream(const char * name)
		: socObject(name, "dioFileStream") {
   myPtr = (SOC_PTR_T)this;
   myMode = DIO_UNKNOWN_MODE;
   myState = DIO_UNKNOWN_STATE;
}

//----------------------------------
dioFileStream:: ~dioFileStream() { }

//:----------------------------------------------- ATTRIBUTES         --
DIO_MODE_T dioFileStream::  mode () {
   return myMode;
}

//----------------------------------
DIO_STATE_T dioFileStream::  state () {
   return myState;
}

//:----------------------------------------------- PUB FUNCTIONS      --
STAFCV_T dioFileStream:: close () {

   DIO_STATE_T saveState=myState;

/*- Close file and destroy xdr. -*/
   if( &myXDR != NULL ) XDR_DESTROY(&myXDR);
   fclose(myFile);

   myState = DIO_CLOSE_STATE;
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

//----------------------------------
STAFCV_T dioFileStream:: getEvent (const char * path ) {

   DIO_STATE_T saveState=myState;

   if( !(DIO_OPEN_STATE == myState)
   ||  !(DIO_READ_MODE == myMode || DIO_UPDATE_MODE == myMode)
   ){
      EML_ERROR(BAD_MODE_OR_STATE);
   }
   myState = DIO_READ_STATE;
/*##########
   char * here;
   if( !dui->pwd(here)
   ||  !dui->cd(path)
   ){
      myState = saveState;
      EML_ERROR(HACK_BROKEN);
   }
##########*/

//- Read data here -**
DS_DATASET_T *pDS=NULL;
bool_t result;
   if( !xdr_dataset(&myXDR, &pDS)
   ||  !dsIsDataset(&result,pDS)
   ||  !result
   ||  !dio_addHierarchy(dui_pDScwd,pDS)
   ){
//    dui->cd(here);
      myState = saveState;
      EML_ERROR(CANT_READ_FROM_STREAM);
   }

/*&&&&&&&&&&
   dui->cd(here);
&&&&&&&&&&*/
   myState = saveState;
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

//----------------------------------
STAFCV_T dioFileStream:: open (const char * fileName, DIO_MODE_T mode) {

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
   if( (myFile = fopen(fileName, fo_mode)) == NULL ){
      EML_ERROR(CANT_OPEN_FILE);
   }

//- Create XDR pointer. -**
   xdrstdio_create(&myXDR, myFile, xdr_mode);
//-? if( myXDR == NULL ) EML_ERROR(BAD_XDR); -??

   myMode = mode;
   myState = DIO_OPEN_STATE;
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

//----------------------------------
STAFCV_T dioFileStream:: putEvent (const char * path ){
   if( !(DIO_OPEN_STATE == myState)
   ||  !(DIO_WRITE_MODE == myMode || DIO_UPDATE_MODE == myMode)
   ){
      EML_ERROR(BAD_MODE_OR_STATE);
   }
   myState = DIO_WRITE_STATE;
/*%%%%%%%%%%
   tdmDataset *dataset;
   if( !tdm->findDataset(path,dataset) ){
      EML_ERROR(TDMDATASET_NOT_FOUND);
   }

...WORK WORK WORK WORK WORK WORK WORK WORK

   char * here;
   if( !dui->pwd(here)
   ||  !dui->cd(path)
   ){
      myState = DIO_OPEN_STATE;
      EML_ERROR(HACK_BROKEN);
   }
%%%%%%%%%%*/

//- Write data here -**
   if( !xdr_dataset(&myXDR, &dui_pDScwd) ){
      myState = DIO_OPEN_STATE;
      EML_ERROR(ERROR_WRITING_DATASET);
   }

// dui->cd(here);
   myState = DIO_OPEN_STATE;
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

//:----------------------------------------------- PRIV FUNCTIONS     --
// **NONE**

// ---------------------------------------------------------------------

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
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
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
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
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
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

//----------------------------------
STAFCV_T dioFactory:: list (){
   socObject* obj;

   printf("\n"
"**********************************************************************"
   "\n"
"***************** DIO - Dataset Input/Output listing *****************"
   "\n"
"**********************************************************************"
   "\n"
"* IDREF * NAME            * TYPE            * MODE & STATE            "
   "\n"
"**********************************************************************"
    "\n");
   for( int i=0;i<count();i++ ){
      if( soc->getObject(entry(i),obj) ){
         if( 0 == strcmp("dioFileStream",obj->type()) ){
            printf("* %5d * %-15s * %-15s * %s %s \n"
                        ,obj->idRef(),obj->name(),obj->type()
			, dio_mode2text(DIOFILESTREAM(obj)->mode())
			, dio_state2text(DIOFILESTREAM(obj)->state())
                        );
         } else if( 0 == strcmp("dioTestStream",obj->type()) ){
            printf("* %5d * %-15s * %-15s * \n"
                        ,obj->idRef(),obj->name(),obj->type()
                        );
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
STAFCV_T dioFactory:: newFileStream (const char * name
		, const char * fileName, DIO_MODE_T mode ){
   IDREF_T id;
   if( soc->idObject(name,"dioFileStream",id) ){
      EML_ERROR(DUPLICATE_OBJECT_NAME);
   }
   static dioFileStream* p;
   p = new dioFileStream(name);
   if( !soc->idObject(name,"dioFileStream",id) ){
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   addEntry(id);
   if( !p->open(fileName,mode) ){
      p->close();
      soc->deleteObject(name,"dioFileStream");
      EML_ERROR(CANT_OPEN_FILE);
   }
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);

}

//:----------------------------------------------- PRIV FUNCTIONS     --

// ---------------------------------------------------------------------


//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:        amiClasses.C
//:DESCRIPTION: AMI Classes
//:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:        06feb96-Broker does not care whether table is RO/WO/RW
//:BUGS:        -- STILL IN DEVELOPMENT --
//:HISTORY:     12dec95-v000a-cet- creation
//:<--------------------------------------------------------------------

//:----------------------------------------------- INCLUDES           --
#include "tdmLib.h"
#include "ami_macros.h"
#include "amiClasses.hh"

//:----------------------------------------------- MACROS             --

//:=============================================== CLASS              ==
// amiInvoker

//:----------------------------------------------- CTORS & DTOR       --
amiInvoker:: amiInvoker(const char * name, long rank
		, FNC_PTR_T pam
		, const STRING_SEQ_T& specs)
		: socObject(name, "amiInvoker") {
   myPtr = (SOC_PTR_T)this;
   if( rank != specs._length){
      EML_LOG_ERROR(BAD_MODULE_RANK);
      myRank=-1; /*-AMI_E_NO_MODULE_RANK-*/
      return;
   }
   myRank = rank;
   myTblSpecs = new char* [myRank];
   DS_DATASET_T *pTABLE=NULL; char *pDATA=NULL;
   for( int i=0;i<myRank;i++ ){
      myTblSpecs[i] = new char[strlen(specs._buffer[i]) +1];
      strcpy(myTblSpecs[i], specs._buffer[i]);
//- HACK - should delete each table after creation
      dsNewTable(&pTABLE,"TABLE",myTblSpecs[i],0,pDATA);
      pTABLE=NULL; pDATA=NULL;  //- HACK
   }
   myPamFtn = pam;
}

//----------------------------------
amiInvoker:: ~amiInvoker() { }

//:----------------------------------------------- ATTRIBUTES         --
long amiInvoker::  rank () {
   return myRank;
}

//----------------------------------
FNC_PTR_T amiInvoker::  pFunction () {
   return myPamFtn;
}

//:----------------------------------------------- PUB FUNCTIONS      --
STAFCV_T amiInvoker:: call (TABLE_SEQ_T& tbl) {

   STAFCV_T status;

//- Check number of tables in sequence.
   if( tbl._length != rank() ){
      EML_PRINTF("PAM = (%s) \n",name());
      EML_ERROR(WRONG_PAM_RANK);
   }
//- Create arrays of TAS-structs for tables.
   TABLE_HEAD_ST **h;
   char **d;
   if( tbl._length > 0 ){
      h = new TABLE_HEAD_ST* [tbl._length];
      d = new char* [tbl._length];
   }
   for( int i=0;i<tbl._length;i++ ){
//- Check types of tables in sequence.
      if( !((tbl._buffer[i])->isType(myTblSpecs[i])) ){
	 EML_PRINTF("table #%d (%s) is wrong type\n",i
			,(tbl._buffer[i])->name());
	 delete[] h;
	 delete[] d;
	 EML_ERROR(WRONG_TABLE_TYPE);
      }
//- Convert table objs to 2-struct (TAS-like) tables.
      if( !((tbl._buffer[i])->cvtTasStructs(h[i],d[i])) ){
	 printf("table #%d (%s) can't be converted\n",i
			,(tbl._buffer[i])->name());
	 delete[] h;
	 delete[] d;
	 EML_ERROR(TABLE_CONVERSION_FAILURE);
      }
   }
//- Call user-written Physics Analysis Module.
   status = ami_pamSwitch(myRank, myPamFtn, h, d);

//- Setting NOK for tables.
   for( i=0;i<tbl._length;i++ ){
      (tbl._buffer[i])->rowCount(h[i]->nok);
   }
//- Deleteing TAS-structures.
   if( tbl._length > 0 ){
      delete[] h;
      delete[] d;
   }
//- WARNING!!! - Pass pure return value out to status vector.
     set_staf_status(status);
     return status;
}

//----------------------------------
STAFCV_T amiInvoker:: init () {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//----------------------------------
STAFCV_T amiInvoker:: start () {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//----------------------------------
STAFCV_T amiInvoker:: stop () {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//----------------------------------
char * amiInvoker:: tableSpec (long ntbl) {

   char * c = new char[strlen(myTblSpecs[ntbl]) + 1];
   strcpy(c,myTblSpecs[ntbl]);
   return c;
}

//----------------------------------
AMI_IO_MODE_T amiInvoker:: tableMode (long ntbl) {
   return AMI_UPDATE_MODE; //HACK - This is temporary
}

//:----------------------------------------------- PRIV FUNCTIONS     --
// **NONE**

// ---------------------------------------------------------------------

//:=============================================== CLASS              ==
// amiBroker

//:----------------------------------------------- CTORS & DTOR       --
amiBroker:: amiBroker(const char * name)
		: socObject(name, "amiBroker") {
   myPtr = (SOC_PTR_T)this;
   lock(TRUE);
}

//----------------------------------
amiBroker:: ~amiBroker() {
}

//:----------------------------------------------- ATTRIBUTES         --
//:**NONE**

//:----------------------------------------------- PUB FUNCTIONS      --
STAFCV_T amiBroker:: callInvoker (const char * name
		, const STRING_SEQ_T& tnames) {

//- Find the correct invoker.
   amiInvoker* invoker=NULL;
   if( !findInvoker(name, invoker) ){
      EML_ERROR(OBJECT_NOT_FOUND);
   }
//- Check number of table names passed.
   if( tnames._length > AMI_MAX_TABLES ){
      EML_ERROR(TOO_MANY_TABLES);
   }
//- Find tables and marshal into sequence.
//- (or create tables when they do not exist)
   TABLE_SEQ_T tables;
   tables._length = tnames._length;
   tables._maximum = tnames._maximum;
   tables._buffer = new tdmTable* [tnames._maximum];

   for( int i=0;i<tnames._length;i++ ){
      if( !tdm->findTable(tnames._buffer[i] ,tables._buffer[i]) ){
	 if( AMI_INPUT_MODE == invoker->tableMode(i) ){
	    EML_ERROR(OBJECT_NOT_FOUND);
	 }
	 else {
	    if( !tdm->newTable(tnames._buffer[i], invoker->tableSpec(i)
			, 1) // TEMPORARY HACK !!!
	    ||  !tdm->findTable(tnames._buffer[i], tables._buffer[i])
	    ){
	    if( !tdm->findTable(tnames._buffer[i], tables._buffer[i])
	    ){ // HACK - should not be called twice... but it works
	       EML_ERROR(CANT_CREATE_OBJECT);
	    }
	    else {
	       EML_MESSAGE(WARNING: Table created with 1 row.);
	    }
	    }
	 }
      }
   }
//- Call the actual invoker object.
   invoker->call(tables);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T amiBroker:: deleteInvoker (const char * name ) {
   if( !soc->deleteObject(name,"amiInvoker") ){
      EML_ERROR(CANT_DELETE_OBJECT);
   }
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T amiBroker:: findInvoker (const char * name
		, amiInvoker*& invoker ) {
   socObject* obj;
   if( !soc->findObject(name,"amiInvoker",obj) ){
      invoker = NULL;
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   invoker = AMIINVOKER(obj);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T amiBroker:: getInvoker (IDREF_T id
		, amiInvoker*& invoker ) {
   socObject* obj;
   if( !soc->getObject(id,obj) ){
      invoker = NULL;
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   if( 0 != strcmp(obj->type(),"amiInvoker") ){
      invoker = NULL;
      EML_ERROR(WRONG_OBJECT_TYPE);
   }
   invoker = AMIINVOKER(obj);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
char * amiBroker:: list () {
   socObject* obj;

   printf("\n"
"+---------------------------------------------------------------------"
   "\n"
"|************* AMI - Analysis Module Interface listing ***************"
   "\n"
"+-------+-----------------+-----------------+------+------------------"
   "\n"
"| IDREF | NAME            | TYPE            | RANK |                  "
   "\n"
"+-------+-----------------+-----------------+------+------------------"
    "\n");
   for( int i=0;i<count();i++ ){
      if( soc->getObject(entry(i),obj) ){
         if( 0 == strcmp("amiInvoker",obj->type()) ){
            printf("| %5d | %-15s | %-15s | %4d | \n"
                        ,obj->idRef(),obj->name(),obj->type()
			,AMIINVOKER(obj)->rank()
                        );
         }
      } else {
         printf("* %5d | %-15s | %-15s | %4s | \n"
                        ,entry(i),"**DELETED**","**DELETED**","***");
      }
   }
   printf(
"+-------+-----------------+-----------------+------+------------------"
   "\n\n");

   return ""; // TEMPORARY HACK
}

//----------------------------------
STAFCV_T amiBroker:: newInvoker (const char * name
		, long rank
		, FNC_PTR_T pam
		, const STRING_SEQ_T& specs) {
   IDREF_T id;
   if( soc->idObject(name,"amiInvoker",id) ){
      EML_ERROR(DUPLICATE_OBJECT_NAME);
   }
   static amiInvoker* p;
   p = new amiInvoker(name,rank,pam,specs);
   if( !soc->idObject(name,"amiInvoker",id) ){
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   addEntry(id);
   EML_SUCCESS(STAFCV_OK);

}

//:----------------------------------------------- PRIV FUNCTIONS     --

// ---------------------------------------------------------------------


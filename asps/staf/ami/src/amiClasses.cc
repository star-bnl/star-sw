// static char amiClasses_what[]="@(#)$Id: amiClasses.cc,v 1.22 1998/08/13 02:07:49 perev Exp $";
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
#include <stdlib.h>
#include "asuAlloc.h"
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
   if( rank != (long)specs._length){
      EML_WARNING(BAD_MODULE_RANK);
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
      if(pTABLE) FREE(pTABLE);  /*fix memory leak -akio*/
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

//----------------------------------
// OVERRIDE socObject::listing()
char * amiInvoker::  listing () {
   char* c = socObject::listing();
   char* cc = NULL;
   cc = (char*)MALLOC(79+100);
   sprintf(cc,"%s %ld arg.s",c,rank());
   FREE(c);
   return cc;
}

//:----------------------------------------------- PUB FUNCTIONS      --
//- override socObject::implementsInterface
unsigned char amiInvoker :: implementsInterface (const char * iface) {
   if( 0 == strcmp("amiInvoker",iface)
   ||  socObject::implementsInterface(iface)
   ){ return TRUE; }
   return FALSE;
}

//----------------------------------
STAFCV_T amiInvoker:: call (TABLE_SEQ_T& tbl) {
  int i;
   STAFCV_T status;

//- Check number of tables in sequence.
   if ((long)tbl._length != rank()) {
      EML_CONTEXT("ERROR: PAM = (%s) \n",(char*)Name());
      EML_ERROR(WRONG_PAM_RANK);
   }
//- Create arrays of TAS-structs for tables.
   TABLE_HEAD_ST **h;
   char **d;
   if( tbl._length > 0 ){
      h = new TABLE_HEAD_ST* [tbl._length];
      d = new char* [tbl._length];
   }
   for( i=0;i<(int)tbl._length;i++ ){
//- Check types of tables in sequence.
      if( !((tbl._buffer[i])->isType(myTblSpecs[i])) ){
	 EML_CONTEXT("ERROR: table #%d (%s) is wrong type\n",i
			,(tbl._buffer[i])->Name());
	 delete[] h;
	 delete[] d;
	 EML_ERROR(WRONG_TABLE_TYPE);
      }
//- Convert table objs to 2-struct (TAS-like) tables.
      if( !((tbl._buffer[i])->cvtTasStructs(h[i],d[i])) ){
	 printf("table #%d (%s) can't be converted\n",i
			,(tbl._buffer[i])->Name());
	 delete[] h;
	 delete[] d;
	 EML_ERROR(TABLE_CONVERSION_FAILURE);
      }
   }
//- Call user-written Physics Analysis Module.
   status = ami_pamSwitch(myRank, myPamFtn, h, d);

//- Setting NOK for tables.
   for( i=0;i<(int)tbl._length;i++ ){
      (tbl._buffer[i])->rowCount(h[i]->nok);
   }
//- Deleting TAS-structures.
   if( tbl._length > 0 ){
     // JL fix memory leak
     for (i = 0; i < (int)tbl._length;i++) {
       if (h[i]) FREE(h[i]);
     }
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

  //     char * c = new char[strlen(myTblSpecs[ntbl]) + 1];    /* -akio */
   char * c = (char *)MALLOC(strlen(myTblSpecs[ntbl]) + 1); /* -alloc conflict */
   strcpy(c,myTblSpecs[ntbl]);
   return c;
}

//----------------------------------
AMI_IO_MODE_T 
amiInvoker::tableMode (long ntbl) 
{
  // Just to hush pedantic compilers
  static void *pn = &ntbl;
  
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
//- override socObject::implementsInterface
unsigned char amiBroker :: implementsInterface (const char * iface) {
   if( 0 == strcmp("amiBroker",iface)
   ||  socFactory::implementsInterface(iface)
   ){ return TRUE; }
   return FALSE;
}

//----------------------------------
STAFCV_T amiBroker:: callInvoker (const char * name
		, const STRING_SEQ_T& tnames) {

//- Find the correct invoker.
   amiInvoker* invoker=NULL;
   if( NULL == (invoker = findInvoker(name)) ){
      EML_CONTEXT("ERROR: Are you sure you have a '%s'?\n",name);
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

//				HACK -- MUST BE DELETED LATER!!!

   char *c,*cc;
   char *table_name=NULL;
   long table_size=1;
   long b;
   for( int i=0;i<(int)tnames._length;i++ ){
//- Break up "name(size)" into "name" and size.
      c = tnames._buffer[i];
      cc = strchr(tnames._buffer[i],'(');
      b = strlen(tnames._buffer[i]);
      if( cc )b = (int)(cc - c);
      table_name = (char*)MALLOC(b +1); 
      table_name[0]=0;strncat(table_name,tnames._buffer[i],b); 
      if( b < (long)strlen(tnames._buffer[i]) ){
	 table_size = atoi(tnames._buffer[i] + b + 1);
      }
      else {
	 table_size = 1;
      }
//- Find or create table.
      if( NULL == (tables._buffer[i] = tdm->findTable(table_name)) ){
	 if( AMI_INPUT_MODE == invoker->tableMode(i) ){
	    EML_ERROR(OBJECT_NOT_FOUND);
	 }
	 else {
	    if( NULL == (tables._buffer[i] = tdm->newTable(table_name,
			invoker->tableSpec(i), table_size)) ){
	       if( table_name ){FREE(table_name); table_name = NULL;}
	       EML_ERROR(CANT_CREATE_OBJECT);
	    }
	    else {
	       EML_WARNING(Table created with 1 row.);
	    }
	 }
      }
      if( table_name ){FREE(table_name); table_name = NULL;}
   }
//- Call the actual invoker object.
   STAFCV_T status = invoker->call(tables);/*fix memory leak -akio*/
   delete[] tables._buffer;                  /*fix memory leak -akio*/
   return status;                          /*fix memory leak -akio*/
//   return invoker->call(tables);
//   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T amiBroker:: deleteInvoker (const char * name ) {
   if( !soc->deleteObject(name,"amiInvoker") ){
      EML_ERROR(CANT_DELETE_OBJECT);
   }
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------//INS++:PARSER_WARNING
amiInvoker * amiBroker:: findInvoker (const char * name) {
   amiInvoker* invoker=NULL;
   socObject* obj;
   if( NULL == (obj = soc->findObject(name,"amiInvoker")) ){
      invoker = NULL;
      EML_CONTEXT("ERROR: Are you sure you have a '%s'?\n",name);
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   invoker = AMIINVOKER(obj);
   // EML_SUCCESS(STAFCV_OK);
   return invoker;
}

//----------------------------------
amiInvoker * amiBroker:: getInvoker (IDREF_T id) {
   amiInvoker* invoker=NULL;
   socObject* obj;
   if( NULL == (obj = soc->getObject(id)) ){
      invoker = NULL;
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   if( 0 != strcmp(obj->type(),"amiInvoker") ){
      invoker = NULL;
      EML_ERROR(WRONG_OBJECT_TYPE);
   }
   invoker = AMIINVOKER(obj);
   // EML_SUCCESS(STAFCV_OK);
   return invoker;
}

//----------------------------------
char * amiBroker:: list () {
   char tit[] =
                "\n"
                "+-------------------------------------------"
                "-----------------------------------\n"
                "|****************** "
                "AMI - Analysis Module Interface listing"
                " *******************\n"
                "%s\n";
   
   char *c = socFactory::list();

   char *cc = (char*)MALLOC(strlen(c) +1 +strlen(tit));

   sprintf(cc,tit,c); 
   FREE(c);
   return cc;

}

//----------------------------------
amiInvoker * amiBroker:: newInvoker (const char * name
		, long rank
		, FNC_PTR_T pam
		, const STRING_SEQ_T& specs) {
   IDREF_T id;
   if( soc->idObject(name,"amiInvoker",id) ){
      EML_CONTEXT("ERROR: You already have a '%s'.\n",name);
      EML_ERROR(DUPLICATE_OBJECT_NAME);
   }
   amiInvoker* p;
   p = new amiInvoker(name,rank,pam,specs);
   if( !soc->idObject(name,"amiInvoker",id) ){
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   addEntry(id);
   // EML_SUCCESS(STAFCV_OK);
   return p;

}

//:----------------------------------------------- PRIV FUNCTIONS     --

// ---------------------------------------------------------------------


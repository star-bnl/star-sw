//:Copyright 1995, Lawrence Berkeley Laboratory
//:>--------------------------------------------------------------------
//:FILE:        socClasses.C
//:DESCRIPTION: Service and Object Catalog
//:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:        -- STILL IN DEVELOPMENT --
//:HISTORY:     21jul95-v000a-cet- creation
//:<--------------------------------------------------------------------
#define FILE_VERSION "$Id: socClasses.cc,v 1.30 1998/08/13 02:08:31 perev Exp $"

//:----------------------------------------------- INCLUDES           --
#include <sys/types.h>
#include <stdio.h>
#ifdef WIN32
# include <stdio.h>
# include <iostream.h>
#elif !defined(LINUX)
# include <sys/stream.h>
#else
# include <g++/stream.h>
#endif

#include <math.h>

#define KUIP
#include "asuAlloc.h"
#include "asuLib.h"
#include "emlLib.h"
#include "socClasses.hh"
#include "soc_globals.h"
#include "socdl.h"
#define PP printf(

#undef VALID_IDREF
#define VALID_IDREF(A)  ( (0 <= A && A < count() && A < maxCount()) \
	&& ( myObjs[A] != NULL ) )

//:----------------------------------------------- PROTOTYPES         --
extern "C" const char *id2name(const char *base, long id);
extern "C" const char *shortname(const char *longname, size_t length);

//:#####################################################################
//:=============================================== CLASS              ==
// socObject

//:----------------------------------------------- CTORS & DTOR       --
socObject:: socObject() {
   EML_MESSAGE("Warning -- This CTOR should never be called.");
   myPtr = 0; // This must be set in derived CTOR !!!
   myName = new stafString("UNNAMED");
   myType = new stafString("UNKNOWN");
   myLock = FALSE;
   soc->signIn(this,myIdRef);
}

//----------------------------------
socObject:: socObject(long id) {
// EML_MESSAGE("Alert -- This CTOR called only by soc.");
   myPtr = 0; // This must be set in derived CTOR !!!
   myName = new stafString("soc");
   myType = new stafString("socCatalog");
   myLock = TRUE;
   myIdRef = id;
}

//----------------------------------
socObject:: socObject(const char* name) {
   myPtr = (SOC_PTR_T)this;
   myName = new stafString(name);
   myType = new stafString("socObject");
   myLock = FALSE;
   soc->signIn(this,myIdRef);
}

//----------------------------------
socObject:: socObject(const char* name, const char* type) {
   myPtr = 0; // This must be set in derived CTOR !!!
   myName = new stafString(name);
   myType = new stafString(type);
   myLock = FALSE;
   soc->signIn(this,myIdRef);
}

//----------------------------------
socObject:: socObject(long n, const char* type) {
   myPtr = 0; // This must be set in derived CTOR !!!
   myName = new stafString(id2name(type,n));
   myType = new stafString(type);
   myLock = FALSE;
   soc->signIn(this,myIdRef);
}

//----------------------------------
socObject:: ~socObject() {
}

//:----------------------------------------------- ATTRIBUTES         --
IDREF_T socObject::  idRef () {
   return myIdRef;
}


//----------------------------------
char * socObject::  version () {
   const char *myVersion="dev";
   char *c = (char*)MALLOC(strlen(myVersion)+1);
   strcpy(c,myVersion);
   return c;
}

//----------------------------------
SOC_PTR_T socObject::  ptr () {
   return myPtr;
}

//----------------------------------
char * socObject:: listing () {
   char *c = 0; 
   c = (char*)MALLOC(79+100);
   memset(c,0,79);
   char l='|';
   if(lock())l='-';
   char *nn, *tt;
   sprintf(c,"| %5ld %c %-15s | %-15s |"
   		, idRef(), l, shortname(Name(),15)
		, shortname(Type(),15));
   return c;
}

//:----------------------------------------------- PUB FUNCTIONS      --
unsigned char socObject :: implementsInterface (const char * iface) {
   if( 0 == strcmp("socObject",iface)
   ){ return TRUE; }
   return FALSE;
}

//:----------------------------------------------- PRIV FUNCTIONS     --
// **NONE**

//:#####################################################################
//:=============================================== CLASS              ==
// socFactory
//:----------------------------------------------- CTORS & DTOR       --
socFactory :: socFactory(long maxCount)
		: socObject() {
   myCount = 0;
   myMaxCount = maxCount;
   idRefs = new IDREF_T [myMaxCount];
}

//----------------------------------
socFactory :: socFactory(const char * name, const char * type)
		: socObject(name, type) {
   myCount = 0;
   idRefs = new IDREF_T [OBJ_MAX_COUNT];
}

//----------------------------------
socFactory :: ~socFactory() {
   delete[] idRefs;
}

//:----------------------------------------------- ATTRIBUTES         --
long socFactory :: count () {
   return myCount;
}

//----------------------------------
long socFactory :: maxCount () {
   return myMaxCount;
}

//----------------------------------
// over-ride socObject::listing()
char * socFactory::  listing () {
   char* c = socObject::listing();
   char* cc = NULL;
   cc = (char*)MALLOC(79+100);
   memset(cc,0,79);
   sprintf(cc,"%s %ld/%ld obj.s",c,count(),maxCount());
   FREE(c);
   return cc;
}

//:----------------------------------------------- PUB FUNCTIONS      --
//- override socObject::implementsInterface
unsigned char socFactory :: implementsInterface (const char * iface) {
   if( 0 == strcmp("socFactory",iface)
   ||  socObject::implementsInterface(iface)
   ){ return TRUE; }
   return FALSE;
}

//----------------------------------
char * socFactory :: list () {
   char *l=NULL;	// individual line
   socObject *o;	// object
   char *c=NULL;

   c = (char*)MALLOC(80*(4+myCount+1)+1);
   char *cc;

   strcpy (c,
		"+-------+-----------------+-----------------"
		"+----------------------------------\n"
		"| IDREF | NAME:OBJECT     | TYPE:CLASS      "
		"| DESCRIPTION                      \n"
		"+-------+-----------------+-----------------"
		"+----------------------------------\n"
   );
  
   int lc=3;		// line count
   for( int i=0;i<myCount;i++ ){
      o = soc->getObject(entryID(i)); 
      if (o) {
      	 l = o->listing();
	 cc = c + (80*lc++);
	 sprintf(cc,"%-79s\n",l);
	 FREE(l);
      }
   }
   cc = c + (80*lc++);
   strcpy(cc,
		"+-------+-----------------+-----------------"
		"+----------------------------------\n"
   );
   return c;
}

//----------------------------------
STAFCV_T socFactory :: addEntry (IDREF_T idRef) {
   if(myCount >= myMaxCount)return FALSE;
   idRefs[myCount++] = idRef;
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T socFactory :: deleteEntry (IDREF_T idRef) {
   for( int i=0;i<myCount;i++ ){
      if( idRef == idRefs[i] ){
	 idRefs[i]=-1; /*-SOC_E_INVALID_IDREF-*/
         soc->deleteID(idRef);
	 EML_SUCCESS(STAFCV_OK);
      }
   }
   EML_ERROR(INVALID_IDREF);
}

//----------------------------------
STAFCV_T socFactory :: unaddEntry (IDREF_T idRef) {
   for( int i=0;i<myCount;i++ ){
      if( idRef == idRefs[i] ){
	 idRefs[i]=-1; /*-SOC_E_INVALID_IDREF-*/
	 EML_SUCCESS(STAFCV_OK);
      }
   }
   EML_ERROR(INVALID_IDREF);
}

//----------------------------------
IDREF_T socFactory :: entryID (long n) {
   if(n<myCount && idRefs[n]>=0){
      return idRefs[n];
   }
   else{
      return -1; /*-SOC_E_INVALID_IDREF-*/
   }
}

//:----------------------------------------------- PROT FUNCTIONS     --
//:**NONE**

//:----------------------------------------------- PRIV FUNCTIONS     --
//:**NONE**

//:#####################################################################
//:=============================================== CLASS              ==
// socCatalog

//:----------------------------------------------- CTORS & DTOR       --
socCatalog:: socCatalog() 
        : socFactory(OBJ_MAX_COUNT)
	, socObject((IDREF_T)0) {

   myPtr = (SOC_PTR_T)this;
   myObjs = new socObject* [maxCount()];
   nextIDRef = FIRST_IDREF;
   IDREF_T id;
//VP   signIn(this,id);
}

//----------------------------------
socCatalog:: ~socCatalog() {
	for( long i=count()-1; i>0; i-- ){
  		if(myObjs[i])delete myObjs[i];
	}
  	delete[] myObjs;
}

//:----------------------------------------------- ATTRIBUTES         --
char * socCatalog:: version() {
	char * myVersion=FILE_VERSION;
	char *c=(char*)MALLOC(strlen(myVersion) +1);
	strcpy(c,myVersion);
	return c;
}

//:----------------------------------------------- PUB FUNCTIONS      --
STAFCV_T 
socCatalog::bind (const char * pname) {
   int status;

   status = soc_dl_load((char*)pname);

//VP   status = soc_dl_init((char*)pname);
//VP   status = soc_dl_start((char*)pname);

   return status;
}

//----------------------------------
STAFCV_T socCatalog :: release (const char * pname){
   return soc_dl_stop((char*)pname);
}


//----------------------------------
//- override socObject::implementsInterface
unsigned char socCatalog :: implementsInterface (const char * iface) {
   if( 0 == strcmp("socCatalog",iface)
   ||  socFactory::implementsInterface(iface)
   ){ return TRUE; }
   return FALSE;
}

//----------------------------------
STAFCV_T socCatalog:: deleteID (IDREF_T id) {
   if( !VALID_IDREF(id) ){
      EML_ERROR(INVALID_IDREF);
   }
   if(myObjs[id]->lock()){
      EML_ERROR(OBJECT_LOCKED);
   }
   delete myObjs[id];
   myObjs[id] = NULL;
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T socCatalog:: deleteObject (
		const char * name, const char * type) {
   IDREF_T id;
   idObject(name,type,id);
   if( !VALID_IDREF(id) ){
      EML_ERROR(INVALID_IDREF);
   }
   if(myObjs[id]->lock()){
      EML_ERROR(OBJECT_LOCKED);
   }
   deleteID(id);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
socObject* socCatalog:: findObject (
		const char * name, const char * type) {
   socObject *obj=NULL;
   IDREF_T id;
   idObject(name,type,id);
   if( !VALID_IDREF(id) ){
      obj = NULL;
      return NULL; //BUG-TOO VERBOSE      EML_ERROR(INVALID_IDREF);
   }
   obj = myObjs[id];
   return obj;
}

//----------------------------------
socObject* socCatalog:: getObject (IDREF_T id) {
   socObject *obj=NULL;
   if( !VALID_IDREF(id) ){
      obj = NULL;
      return NULL;
   }
   obj = myObjs[id];
   return obj;
}

//----------------------------------
STAFCV_T socCatalog:: idObject (const char * name
		, const char * type, IDREF_T& id) {
// THIS COULD BE IMPLEMENTED BY A HASHTABLE LOOKUP !!
   const char *n, *t;
   socObject *o;
   for( int i=0; i<myCount; i++ ){
     o = myObjs[i];
      if( ! o) 		continue;
      n=o->Name();
      if (n[0] != name[0])	continue; 
      if(strcmp(n,name) )	continue;
      t=o->Type();
      if (type[0]!='-') {
        if (t[0] != type[0]) 	continue;
        if (strcmp(t,type))	continue;} 
      id = i;
      if(i != o->idRef()){	//- HACK???
	printf("MAJOR ERROR REPORT!!! (%d,%ld)\n",i,id=myObjs[i]->idRef());
	exit(13); }
      EML_SUCCESS(STAFCV_OK);
   }
   id = -1; 		/*- SOC_E_IDREF_NOTFOUND -*/
   return FALSE;	//- HACK: This error is too sensitive
}

//----------------------------------
//- OVER-RIDE socFactory:: list()
char * socCatalog :: list () {

   char tit[]=
		"\n"
		"+-------------------------------------------"
		"-----------------------------------\n"
		"|******************* "
		"SOC - Service & Object Catalog listing"
		" *******************\n"
		"%s\n";
   
   
   char *c = socFactory::list();

   char *cc = (char*)MALLOC(strlen(c) +1 + strlen(tit));

   sprintf(cc,tit,c); 
   FREE(c);
   return cc;
}

//----------------------------------
socObject* socCatalog:: newObject (const char * name) {
   static socObject* p;
   p = new socObject(name,"socObject");
   return p;
}

//----------------------------------
STAFCV_T socCatalog:: signIn (socObject* obj, IDREF_T& id) {
   myObjs[count()] = obj;
   id = nextIDRef++;
   addEntry(id);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T socCatalog:: signOut (IDREF_T id) {
   if( !VALID_IDREF(id) ){
      EML_ERROR(INVALID_IDREF);
   }
   if(myObjs[id]->lock()){
      EML_ERROR(OBJECT_LOCKED);
   }
   myObjs[id] = NULL;
   EML_SUCCESS(STAFCV_OK);
}
 
//:----------------------------------------------- PRIV FUNCTIONS     --
//:**NONE**


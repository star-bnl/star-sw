//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:        dioClasses.C
//:DESCRIPTION: DIO Classes
//:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:        -- STILL IN DEVELOPMENT --
//HISTORY:      30dec96-v020a-cet- NEW_DSL -> OLD_DSL option
//:HISTORY:     21aug96-v010a-cet- debug xdrrec_create for Sun4OS5
//:HISTORY:     12dec95-v000a-cet- creation
//:<--------------------------------------------------------------------

//:----------------------------------------------- INCLUDES           --
//- NEEDED FOR openServer() -??
#include <sys/types.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <netinet/in.h>
#include <errno.h>
#include <signal.h>

#include <string.h>
#include "asuAlloc.h"
#include "dioClasses.hh"
#include "tcplib.h"	//- WHG's functions

//:----------------------------------------------- MACROS             --
#include "dio_macros.h"

//:----------------------------------------------- PROTOTYPES         --
extern "C" int klose(int);	//-HACK- C++ fails on close(int)
//extern "C" static void reaper();
extern "C" void signal_reaper();

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

   if( !(DIO_CLOSE_STATE != myState) ){
      EML_ERROR(BAD_MODE_OR_STATE);
   }

   DIO_STATE_T saveState=myState;
   myState = DIO_UNKNOWN_STATE;

/*- Destroy xdr. -*/
   if( &myXDR != NULL ) XDR_DESTROY(&myXDR);

   if( DIO_OPEN_STATE == saveState ){
      saveState = DIO_CLOSE_STATE;
   }
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T dioStream:: getEvent (tdmDataset* destination) {

   DIO_STATE_T saveState=myState;

   if( !(DIO_OPEN_STATE == myState)
   ||  !(DIO_READ_MODE == myMode || DIO_UPDATE_MODE == myMode)
   ){
      EML_ERROR(BAD_MODE_OR_STATE);
   }
   myState = DIO_READ_STATE;

//-(" Get pointer to destination dataset. \n");
   DSL_PTR_T ddd=0;
   if( !destination->cvtDslPointer(ddd) ){
      EML_ERROR(BAD_DATASET);
   }
//-(" Convert pointer to DS_DATASET_T. \n");
   DS_DATASET_T *pDest=(DS_DATASET_T*)ddd;

//-(" Read data here. \n");
DS_DATASET_T *pDS=NULL;
bool_t result;
#ifndef OLD_DSL
   if( !xdr_dataset_type(&myXDR, &pDS)		/* get descriptor */
#else  /*OLD_DSL*/
   if( !xdr_dataset_type(&myXDR, &pDS,0)	/* get descriptor */
#endif /*OLD_DSL*/
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
STAFCV_T dioStream:: open (DIO_MODE_T mode) {
//- This function MUST be overridden by specific subclass.
   EML_ERROR(PURE_VIRTUAL);
}

//----------------------------------
STAFCV_T dioStream:: putEvent (tdmDataset* source) {
   if( !(DIO_OPEN_STATE == myState)
   ||  !(DIO_WRITE_MODE == myMode || DIO_UPDATE_MODE == myMode)
   ){
      EML_ERROR(BAD_MODE_OR_STATE);
   }
   myState = DIO_WRITE_STATE;

//- Get pointer to source dataset -**
   DSL_PTR_T ddd;
   if( !source->cvtDslPointer(ddd) ){
      myState = DIO_OPEN_STATE;
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
   close();
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
   myState = DIO_UNKNOWN_STATE;

/*- Close file and destroy xdr. -*/
   dioStream::close();
   fclose(myFile);

   if( DIO_OPEN_STATE == saveState ){
      myState = DIO_CLOSE_STATE;
   }
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T dioFileStream:: open (DIO_MODE_T mode) {

   xdr_op xdr_mode;
   char fo_mode[3];

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
      printf("(%s)(%s)\n",myFileName,fo_mode);
      EML_ERROR(CANT_OPEN_FILE);
   }

//- Create XDR pointer. -**
   xdrstdio_create(&myXDR, myFile, xdr_mode);
//-? if( myXDR == NULL ) EML_ERROR(BAD_XDR); -??

   myMode = mode;
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
dioSockStream:: dioSockStream(const char * name, const char * hostName
			, long port)
		: dioStream()
		, socObject(name,"dioSockStream") {
   myPtr = (SOC_PTR_T)this;

//- Socket stuff;
   myState = DIO_UNKNOWN_STATE;
   myBufferSize = -1; /*-DIO_E_UNIMPLEMENTED-*/
   myHost = (char*)ASUALLOC(strlen(hostName) +1);
   strcpy(myHost,hostName);
   myPort = port;
   myMaxHandshakes = 10; //-default
}

//----------------------------------
dioSockStream:: ~dioSockStream() {
   close();
}

//:----------------------------------------------- ATTRIBUTES         --
long dioSockStream::  port () {
   return myPort;
}

//----------------------------------
char * dioSockStream::  host () {
   char *c=(char*)ASUALLOC(strlen(myHost) +1);
   strcpy(c,myHost);
   return c;
}

//----------------------------------
long dioSockStream::  socketNumber () {
   return mySocket;
}

//----------------------------------
long dioSockStream::  bufferSize () {
   return myBufferSize;
}

//----------------------------------
void dioSockStream:: maxHandshakes (long count) {
   if(count >= 0)myMaxHandshakes = count;;
}

//----------------------------------
long dioSockStream::  maxHandshakes () {
   return myMaxHandshakes;
}

//:----------------------------------------------- PUB FUNCTIONS      --
STAFCV_T dioSockStream:: acknowledgeRequest() {

    bool_t  ak=TRUE;
    enum xdr_op save_op;

    save_op = myXDR.x_op;
//-- server: sending acknowledgement
    myXDR.x_op = XDR_ENCODE;
    if (!xdr_bool(&myXDR,&ak)) {
	perror("server send acknowledgement");
	return STAFCV_BAD;
    }
    if (!xdrrec_endofrecord(&myXDR, 1)) {
	printf("xdr_endofrecord failed for encode\n");
	return STAFCV_BAD;
    }
    myXDR.x_op = save_op;
    return STAFCV_OK;
}

//----------------------------------
STAFCV_T dioSockStream:: requestAcknowledge() {

    bool_t  rq=TRUE, ak;
    enum xdr_op save_op;

    save_op = myXDR.x_op;
//-- client: sending request
    myXDR.x_op = XDR_ENCODE;
    if (!xdr_bool(&myXDR,&rq)) {
	perror("client send request");
	return STAFCV_BAD;
    }
    if (!xdrrec_endofrecord(&myXDR, 1)) {
	printf("xdr_endofrecord failed for encode\n");
	return STAFCV_BAD;
    }
//-- client: waiting for acknowledgement
    myXDR.x_op = XDR_DECODE;
/*  printf("xdrrec_skiprecord: %d\n", xdrrec_skiprecord(&myXDR)); */
    xdrrec_skiprecord(&myXDR); /* REPLACE ^^^ */
    if (!xdr_bool(&myXDR,&ak)) {
	perror("client get ack");
	return STAFCV_BAD;
    }
    myXDR.x_op = save_op;
    if(ak)return STAFCV_OK;
    return STAFCV_BAD;
}

//----------------------------------
STAFCV_T dioSockStream:: close () {

   DIO_STATE_T saveState=myState;
   myState = DIO_UNKNOWN_STATE;

/*- Close socket and destroy xdr. -*/
   dioStream::close();
/*-klose(mySocket);	HACK-This needs to be a message to server-*/

   if( DIO_OPEN_STATE == saveState ){
      myState = DIO_CLOSE_STATE;
   }
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T dioSockStream:: open (DIO_MODE_T mode) {

   int status;

//- Open network socket. -**
   if( !(DIO_OPEN_STATE == myState)
   &&  !(DIO_CONNECT_STATE == myState)
   ){
      if( (status = tcpConnect(myHost, myPort, &mySocket)) ){
	 perror("tcpConnect failed");
	 myState = DIO_UNKNOWN_STATE;
	 EML_ERROR(CONNECTION_FAILURE);
      }
      myState = DIO_CONNECT_STATE;
   }

//- Create XDR pointer. -**
   memset((char*)&myXDR, 0, sizeof(myXDR));
//-21aug96- xdrrec_create(&myXDR, 0, 0, &mySocket, tcpRead, tcpWrite);
   xdrrec_create(&myXDR, 0, 0, (char*)&mySocket, tcpRead, tcpWrite);
//-? if( myXDR == NULL ) EML_ERROR(BAD_XDR); -??

//- Set XDR mode -**
   switch (mode){
   case DIO_READ_MODE:
      myXDR.x_op = XDR_DECODE;
      break;
   case DIO_WRITE_MODE:
      myXDR.x_op = XDR_ENCODE;
      break;
   case DIO_UPDATE_MODE:	//- Not implemented
   case DIO_UNKNOWN_MODE:
   default:
      EML_ERROR(INVALID_MODE);
      break;
   }

   myMode = mode;
   myState = DIO_OPEN_STATE;
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T dioSockStream:: getEvent (tdmDataset* destination) {
   long retrys=0;
   if( 0 < maxHandshakes() ){
      if( !requestAcknowledge() ){
	 if( maxHandshakes() > 1 ){
	    EML_LOG_ERROR(RETRYING_HANDSHAKE);
	 }
	 else {
	    EML_ERROR(HANDSHAKE_FAILED);
	 }
	 while( (!requestAcknowledge()) 
	 		&& (retrys++ < maxHandshakes())
	 ){
	    printf(".");fflush(0);	//- heartbeat();	
	    if( retrys >= maxHandshakes() ){
	       printf("\n");
	       EML_ERROR(HANDSHAKE_FAILED);
	    }
	 }
	 printf("\n");
      }
   }
/* printf("xdrrec_skiprecord: %d\n", xdrrec_skiprecord(&myXDR)); */
   xdrrec_skiprecord(&myXDR); /* REPLACE ^^^ */
   return dioStream::getEvent(destination);
}

//----------------------------------
//?STAFCV_T dioSockStream:: putEvent (tdmDataset* destination) {
//?   STAFCV_T status = dioStream::putEvent(destination);
//?   if( !xdrrec_endofrecord(&myXDR, 1) ){
//?      EML_ERROR(ENDOFRECORD_FAILURE);
//?   }
//?   return status;
//?}

//:----------------------------------------------- PROT FUNCTIONS     --
int dioSockStream:: openServer() {
   extern int errno;
   int length, listenSocket, acceptSocket;
   struct sockaddr_in localAddr, remoteAddr;

   if ((listenSocket = socket(AF_INET, SOCK_STREAM, 0)) < 0)
   		return socketCallFailed;

   memset((char *) &localAddr, 0, sizeof(localAddr));
   localAddr.sin_family = htons(AF_INET);;
   localAddr.sin_port = htons(port());

   if (bind(listenSocket, (struct sockaddr *) & localAddr,
   		sizeof(localAddr)) < 0) {
      klose(listenSocket);
      return bindFailure;
   }
   if (listen(listenSocket, 5) < 0) {
      perror("listen failed\n");
      return listenFailure;
   }

/* signal(SIGCHLD, reaper);// eliminate zombies */
   signal_reaper();
}

//----------------------------------
int dioSockStream:: openClient() {
   return tcpConnect(myHost, myPort, &mySocket);
}

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
         } else if( 0 == strcmp("dioSockStream",obj->type()) ){
            printf("| %5d | %-15s | %-15s | %s %s \n"
                        ,obj->idRef(),obj->name(),obj->type()
			, dio_mode2text(DIOSOCKSTREAM(obj)->mode())
			, dio_state2text(DIOSOCKSTREAM(obj)->state())
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

//- Generic Streams ---------------------------------------------------
STAFCV_T dioFactory:: deleteStream (const char * name) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//----------------------------------
STAFCV_T dioFactory:: findStream (const char * name
		, dioStream*& stream) {
   socObject* obj;
   if( soc->findObject(name,"dioFileStream",obj) ){
      stream = DIOFILESTREAM(obj);
      EML_SUCCESS(STAFCV_OK);
   }
   if( soc->findObject(name,"dioSockStream",obj) ){
      stream = DIOSOCKSTREAM(obj);
      EML_SUCCESS(STAFCV_OK);
   }
   stream = NULL;
   EML_ERROR(OBJECT_NOT_FOUND);
}

//----------------------------------
STAFCV_T dioFactory:: getStream (IDREF_T id, dioStream*& stream) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
   socObject* obj;
   if( !soc->getObject(id,obj) ){
      stream = NULL;
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   char *t=obj->type();
   if( (0 == strcmp(t,"dioFileStream")) ){
      ASUFREE(t);
      stream = DIOFILESTREAM(obj);
      EML_SUCCESS(STAFCV_OK);
   }
   if( (0 == strcmp(t,"dioSockStream")) ){
      ASUFREE(t);
      stream = DIOSOCKSTREAM(obj);
      EML_SUCCESS(STAFCV_OK);
   }
   ASUFREE(t);
   stream = NULL;
   EML_ERROR(WRONG_OBJECT_TYPE);
}

//- File Streams ------------------------------------------------------
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

//- Socket Streams ----------------------------------------------------
//----------------------------------
STAFCV_T dioFactory:: deleteSockStream (const char * name ){
   if( !soc->deleteObject(name,"dioSockStream") ){
      EML_ERROR(CANT_DELETE_OBJECT);
   }
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T dioFactory:: findSockStream (const char * name
		, dioSockStream*& sockStream ){
   socObject* obj;
   if( !soc->findObject(name,"dioSockStream",obj) ){
      sockStream = NULL;
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   sockStream = DIOSOCKSTREAM(obj);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T dioFactory:: getSockStream (IDREF_T id
		, dioSockStream*& sockStream ){
   socObject* obj;
   if( !soc->getObject(id,obj) ){
      sockStream = NULL;
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   if( 0 != strcmp(obj->type(),"dioSockStream") ){
      sockStream = NULL;
      EML_ERROR(WRONG_OBJECT_TYPE);
   }
   sockStream = DIOSOCKSTREAM(obj);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T dioFactory:: newSockStream (const char * name
		, const char * hostName, long port) {
   IDREF_T id;
   if( soc->idObject(name,"dioSockStream",id) ){
      EML_ERROR(DUPLICATE_OBJECT_NAME);
   }
   static dioSockStream* p;
   p = new dioSockStream(name,hostName,port);
   if( !soc->idObject(name,"dioSockStream",id) ){
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   addEntry(id);
/***
   if( !p->open(mode) ){
      p->close();
      soc->deleteObject(name,"dioSockStream");
      EML_ERROR(CANT_OPEN_SOCKET);
   }
***/
   EML_SUCCESS(STAFCV_OK);

}
//:----------------------------------------------- PRIV FUNCTIONS     --

// ---------------------------------------------------------------------


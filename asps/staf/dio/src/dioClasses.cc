//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:        dioClasses.C
//:DESCRIPTION: DIO Classes
//:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:        -- STILL IN DEVELOPMENT --
//:HISTORY:     02oct97-v030a-hjw- support for socket writing (server mode)
//:HISTORY:     30dec96-v020a-cet- NEW_DSL -> OLD_DSL option
//:HISTORY:     21aug96-v010a-cet- debug xdrrec_create for Sun4OS5
//:HISTORY:     12dec95-v000a-cet- creation
//:<--------------------------------------------------------------------

//:----------------------------------------------- INCLUDES           --
#include <sys/types.h>
#ifndef  WIN32 
# include <arpa/inet.h>
# include <sys/socket.h>
# include <sys/time.h>
# include <sys/resource.h>
# include <sys/wait.h>
# include <netinet/in.h>
#else
# include <windows.h>
# include <winsock.h>
#endif 	/* WIN32 */

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

//:#####################################################################
//:=============================================== CLASS              ==
// dioStream
//:----------------------------------------------- CTORS & DTOR       --
dioStream:: dioStream()
		: socObject() {
   memset(&myXDR,0,sizeof(XDR));
   myMode = DIO_UNKNOWN_MODE;
   myState = DIO_UNKNOWN_STATE;
   memset(&myXDR,0,sizeof(XDR));
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

//----------------------------------
char * dioStream::  location () {
   char * c="unknown";
   char * cc;
   cc = (char*)MALLOC(strlen(c) +1);
   strcpy(cc,c);
   return cc;
}

//----------------------------------
// OVERRIDE socObject::listing()
char * dioStream::  listing () {
   char* c = socObject::listing();
   char* cc = NULL;
   char* m = dio_mode2text(mode());
   const char* s = dio_state2text(state());
   char* l = location();

   cc = (char*)MALLOC(79+100);
   memset(cc,0,79);
   sprintf(cc,"%s (%c,%c) %-24s",c,m[0],s[0],l);
   FREE(c);
   FREE(l);
   return cc;
}

//:----------------------------------------------- PUB FUNCTIONS      --
//- override socObject::implementsInterface
unsigned char dioStream :: implementsInterface (const char * iface) {
   if( 0 == strcmp("dioStream",iface)
   ||  socObject::implementsInterface(iface)
   ){ return TRUE; }
   return FALSE;
}

//----------------------------------
STAFCV_T dioStream:: close () {

   if( DIO_CLOSE_STATE == myState ){
      EML_CONTEXT("Remark: This stream is already closed.\n");
      EML_SUCCESS(STAFCV_OK);
   }


/*- Destroy xdr. -*/
   if( myState != DIO_UNKNOWN_STATE ) { // hjw June 17 1998.  I did this
//VP temporary skip     XDR_DESTROY(&myXDR);     // to prevent crash due to un-initialized
                              // myXDR when client socketStream is refused
   }                          // by server.

   myState = DIO_CLOSE_STATE;
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T dioStream:: getEvent (tdmDataset* destination) {

   DIO_STATE_T saveState=myState;

   if( !(DIO_OPEN_STATE == myState)
   ||  !(DIO_READ_MODE == myMode || DIO_UPDATE_MODE == myMode)
   ){
      EML_CONTEXT("ERROR: Read-write mode must be READ or UPDATE.\n"
      "Open-close state must be OPEN.\n");
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
    EML_CONTEXT("ERROR: stream either not open or not write/update mode.\n");
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

//:#####################################################################
//:=============================================== CLASS              ==
// dioFileStream

//:----------------------------------------------- CTORS & DTOR       --
dioFileStream:: dioFileStream(const char * name, const char * fileName)
		: dioStream()
		, socObject(name, "dioFileStream") {
   myPtr = (SOC_PTR_T)this;
   myFileName = (char*)MALLOC(strlen(fileName) +1);
   strcpy(myFileName,fileName);
}

//----------------------------------
dioFileStream:: ~dioFileStream() {
   close();
   FREE(myFileName);
}

//:----------------------------------------------- ATTRIBUTES         --
char * dioFileStream::  fileName () {
   char * c = (char*)MALLOC(strlen(myFileName) +1);
   strcpy(c,myFileName);
   return c;
}

//----------------------------------
//-OVERRIDE dioStream:: location()
char * dioFileStream::  location () {
   return fileName();
}

//:----------------------------------------------- PUB FUNCTIONS      --
//- override socObject::implementsInterface
unsigned char dioFileStream:: implementsInterface (const char * iface) {
   if( 0 == strcmp("dioFileStream",iface)
   ||  dioStream::implementsInterface(iface)
   ){ return TRUE; }
   return FALSE;
}

//----------------------------------
STAFCV_T dioFileStream:: close () {

   if (myState == DIO_CLOSE_STATE)  {
     EML_SUCCESS(STAFCV_OK);}
      

/*- Close file and destroy xdr. -*/
   dioStream::close();
   fclose(myFile);

   myState = DIO_CLOSE_STATE;
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
   if( ! myXDR.x_ops  ) EML_ERROR(BAD_XDR); 

   myMode = mode;
   myState = DIO_OPEN_STATE;
   EML_SUCCESS(STAFCV_OK);
}

//:----------------------------------------------- PRIV FUNCTIONS     --
// **NONE**

// ---------------------------------------------------------------------

//:#####################################################################
//:=============================================== CLASS              ==
// dioTapeStream
//:----------------------------------------------- CTORS & DTOR       --
dioTapeStream:: dioTapeStream(const char * name, const char * tape)
		: dioStream()
		, socObject(name,"dioTapeStream") {
   myPtr = (SOC_PTR_T)this;
   myTapeName = (char*)MALLOC(strlen(tape) +1);
   strcpy(myTapeName,tape);
}
dioTapeStream:: ~dioTapeStream() {
   FREE(myTapeName);
}

//:----------------------------------------------- ATTRIBUTES         --
char * dioTapeStream::  tapeName () {
   char * c = (char*)MALLOC(strlen(myTapeName) +1);
   strcpy(c,myTapeName);
   return c;
}

//----------------------------------
//-OVERRIDE dioStream:: location()
char * dioTapeStream::  location () {
   return tapeName();
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
//- override socObject::implementsInterface
unsigned char dioTapeStream:: implementsInterface (const char * iface) {
   if( 0 == strcmp("dioTapeStream",iface)
   ||  dioStream::implementsInterface(iface)
   ){ return TRUE; }
   return FALSE;
}

//:----------------------------------------------- PROT FUNCTIONS     --
//:----------------------------------------------- PRIV FUNCTIONS     --

//:#####################################################################
//:=============================================== CLASS              ==
// dioSockStream
//:----------------------------------------------- CTORS & DTOR       --
dioSockStream:: dioSockStream(const char * name, const char * hostName
			, unsigned long port)
		: dioStream()
		, socObject(name,"dioSockStream") {
   myPtr = (SOC_PTR_T)this;

//- Socket stuff;
   myState = DIO_UNKNOWN_STATE;
   myBufferSize = -1; /*-DIO_E_UNIMPLEMENTED-*/
   myHost = (char*)MALLOC(strlen(hostName) +1);
   strcpy(myHost,hostName);
   myPort = port;
   myMaxHandshakes = 10; //-default
}

//----------------------------------
dioSockStream:: ~dioSockStream() {
   close();
}

//----------------------------------
//-OVERRIDE dioStream:: location()
char * dioSockStream::  location () {
   char * h=host();
   long p=port();
   char *cc = (char*)MALLOC(strlen(h)+5 +1);
   sprintf(cc,"%s:%4d",h,p);
   FREE(h);
   return cc;
}

//:----------------------------------------------- ATTRIBUTES         --
long dioSockStream::  port () {
   return myPort;
}

//----------------------------------
char * dioSockStream::  host () {
   char *c=(char*)MALLOC(strlen(myHost) +1);
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
//- override socObject::implementsInterface
unsigned char dioSockStream:: implementsInterface (const char * iface) {
   if( 0 == strcmp("dioSockStream",iface)
   ||  dioStream::implementsInterface(iface)
   ){ return TRUE; }
   return FALSE;
}

//----------------------------------
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

/*- Close socket and destroy xdr. -*/
   dioStream::close();
/*-klose(mySocket);	HACK-This needs to be a message to server-*/

   myState = DIO_CLOSE_STATE;
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T dioSockStream:: open (DIO_MODE_T mode) {

   int status;
   char *cc, *ccc;

  if( !(myMode == DIO_UNKNOWN_MODE)
  &&  !(mode == myMode)
  ){
    EML_CONTEXT("ERROR: dioSockStream mode cannot change from (%s) to (%s)\n"
		,cc=dio_mode2text(myMode)
		,ccc=dio_mode2text(mode));
    FREE(cc); FREE(ccc);
    EML_ERROR(BAD_MODE);
  }

//- Open network socket. -**
   if( !(DIO_OPEN_STATE == myState)
   &&  !(DIO_CONNECT_STATE == myState)
   ){
      if(mode==DIO_READ_MODE) {
        if( (status = tcpConnect(myHost, myPort, &mySocket)) ){
           perror("tcpConnect failed");
           myState = DIO_UNKNOWN_STATE;
           EML_ERROR(CONNECTION_FAILURE);
        }
      } else if(mode==DIO_WRITE_MODE) {
        EML_MESSAGE("Waiting for connection from client.\n");
        if( (status = tcpStartServer(myPort, &mySocket)) ){
           perror("tcpStartServer failed");
           myState = DIO_UNKNOWN_STATE;
           EML_ERROR(BEGIN_SERVER_FAILURE);
        }
        EML_MESSAGE("Connection made.\n");
      } else {
        EML_ERROR(UNSUPPORTED_READWRITE_MODE);
      }
      myState = DIO_CONNECT_STATE;
   }

//- Create XDR pointer. -**
   memset((char*)&myXDR, 0, sizeof(myXDR));
//-21aug96- xdrrec_create(&myXDR, 0, 0, &mySocket, tcpRead, tcpWrite);
   // xdrrec_create was here, I moved it down about 17 lines hjw 02oct97
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
      EML_CONTEXT("ERROR: The mode (read/write/update) is wrong.\n");
      EML_ERROR(INVALID_MODE);
      break;
   }

   // Moved the create to after setting myXDR.x_op, in analogy with
   // dioFileStream::open.   hjw 02oct97
   xdrrec_create(&myXDR, 0, 0, (char*)&mySocket, tcpRead, tcpWrite);

   myMode = mode;
   myState = DIO_OPEN_STATE;
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
// hjw 02oct97
STAFCV_T dioSockStream:: putEvent (tdmDataset* destination) { 

   STAFCV_T status;

   status = acknowledgeRequest();
   if( status != STAFCV_OK ) EML_ERROR(ACKNOWLEDGEREQUEST_FAILURE);

   status = dioStream::putEvent(destination);
   if( status != STAFCV_OK ) EML_ERROR(PUTEVENT_FAILURE)
   if( !xdrrec_endofrecord(&myXDR, 1) ){
      EML_ERROR(ENDOFRECORD_FAILURE);
   }

   return status;
}

//----------------------------------
STAFCV_T dioSockStream:: getEvent (tdmDataset* destination) {
   long retrys=0;
   if( 0 < maxHandshakes() ){
      if( !requestAcknowledge() ){
	 if( maxHandshakes() > 1 ){
	    EML_WARNING(RETRYING_HANDSHAKE);
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

//:----------------------------------------------- PROT FUNCTIONS     --
// int dioSockStream:: openServer() {
//    extern int errno;
//    int length, listenSocket, acceptSocket;
//    struct sockaddr_in localAddr, remoteAddr;
// 
//    if ((listenSocket = socket(AF_INET, SOCK_STREAM, 0)) < 0)
//    		return socketCallFailed;
// 
//    memset((char *) &localAddr, 0, sizeof(localAddr));
//    localAddr.sin_family = htons(AF_INET);;
//    localAddr.sin_port = htons(port());
// 
//    if (bind(listenSocket, (struct sockaddr *) & localAddr,
//    		sizeof(localAddr)) < 0) {
//       klose(listenSocket);
//       return bindFailure;
//    }
//    if (listen(listenSocket, 5) < 0) {
//       perror("listen failed\n");
//       return listenFailure;
//    }
// 
// /* signal(SIGCHLD, reaper);// eliminate zombies */
//    signal_reaper();
//    return 1;		//INS++: RETURN_INCONSISTENT - BUGFIX?
// }

//----------------------------------
// int dioSockStream:: openClient() {
//    return tcpConnect(myHost, myPort, &mySocket);
// }

//:----------------------------------------------- PRIV FUNCTIONS     --

//:#####################################################################
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
//----------------------------------
//- override socObject::implementsInterface
unsigned char dioFactory :: implementsInterface (const char * iface) {
   if( 0 == strcmp(type(),iface)
   ||  socFactory::implementsInterface(iface)
   ){ return TRUE; }
   return FALSE;
}

//----------------------------------
char * dioFactory:: list () {

   char tit[] =
                "\n"
                "+-------------------------------------------"
                "-----------------------------------\n"
                "|********************* "
                "DIO - Dataset Input/Output listing"
                " *********************\n"
                "%s\n";

   char *c = socFactory::list();

   char *cc = (char*)MALLOC(strlen(c) +1 + strlen(tit));

   sprintf(cc,tit,c);
   FREE(c);
   return cc;

}

//- Generic Streams ---------------------------------------------------
STAFCV_T dioFactory:: deleteStream (const char * name) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//----------------------------------
dioStream* dioFactory:: findStream (const char * name) {
   socObject* obj;
   dioStream* stream;
   if( NULL != (obj = soc->findObject(name,"dioFileStream")) ){
      stream = DIOFILESTREAM(obj);
      // EML_SUCCESS(STAFCV_OK);
      return stream;
   }
   if( NULL != (obj = soc->findObject(name,"dioSockStream")) ){
      stream = DIOSOCKSTREAM(obj);
      // EML_SUCCESS(STAFCV_OK);
      return stream;
   }
   stream = NULL;
   // EML_ERROR(OBJECT_NOT_FOUND);
   return NULL;
}

//----------------------------------
dioStream* dioFactory:: getStream (IDREF_T id) {
   dioStream* stream;
   socObject* obj;
   if( NULL == (obj = soc->getObject(id)) ){
      stream = NULL;
      // EML_ERROR(OBJECT_NOT_FOUND);
      return NULL;
   }
   char *t=obj->type();
   if( (0 == strcmp(t,"dioFileStream")) ){
      FREE(t);
      stream = DIOFILESTREAM(obj);
      // EML_SUCCESS(STAFCV_OK);
      return stream;
   }
   if( (0 == strcmp(t,"dioSockStream")) ){
      FREE(t);
      stream = DIOSOCKSTREAM(obj);
      // EML_SUCCESS(STAFCV_OK);
      return stream;
   }
   FREE(t);
   stream = NULL;
   // EML_ERROR(WRONG_OBJECT_TYPE);
   return NULL;
}

//- File Streams ------------------------------------------------------
STAFCV_T dioFactory:: deleteFileStream (const char * name ){
   if( !soc->deleteObject(name,"dioFileStream") ){
      EML_ERROR(CANT_DELETE_OBJECT);
   }
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
dioFileStream* dioFactory:: findFileStream (const char * name) {
   dioFileStream* fileStream;
   socObject* obj;
   if( NULL == (obj = soc->findObject(name,"dioFileStream")) ){
      fileStream = NULL;
      // EML_ERROR(OBJECT_NOT_FOUND);
      return NULL;
   }
   fileStream = DIOFILESTREAM(obj);
   // EML_SUCCESS(STAFCV_OK);
   return fileStream;
}

//----------------------------------
dioFileStream * dioFactory:: getFileStream (IDREF_T id ){
   dioFileStream* fileStream;
   socObject* obj;
   if( NULL == (obj = soc->getObject(id)) ){
      fileStream = NULL;
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   if( 0 != strcmp(obj->type(),"dioFileStream") ){
      fileStream = NULL;
      EML_ERROR(WRONG_OBJECT_TYPE);
   }
   fileStream = DIOFILESTREAM(obj);
   // EML_SUCCESS(STAFCV_OK);
   return fileStream;
}

//----------------------------------
dioFileStream * dioFactory:: newFileStream (const char * name 
		, const char * fileName) {
   IDREF_T id;
   if( soc->idObject(name,"dioFileStream",id) ){
      EML_CONTEXT("ERROR: You already have a '%s'.\n",name);
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
   // EML_SUCCESS(STAFCV_OK);
   return p;

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
dioSockStream * dioFactory:: findSockStream (const char * name){
   dioSockStream* sockStream;
   socObject* obj;
   if( NULL == (obj = soc->findObject(name,"dioSockStream")) ){
      sockStream = NULL;
      EML_CONTEXT("ERROR: Are you sure you have a sock stream '%s'?\n",name);
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   sockStream = DIOSOCKSTREAM(obj);
   // EML_SUCCESS(STAFCV_OK);
   return sockStream;
}

//----------------------------------
dioSockStream *  dioFactory:: getSockStream (IDREF_T id ){
   dioSockStream* sockStream;
   socObject* obj;
   if( NULL == (obj = soc->getObject(id)) ){
      sockStream = NULL;
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   if( 0 != strcmp(obj->type(),"dioSockStream") ){
      sockStream = NULL;
      EML_ERROR(WRONG_OBJECT_TYPE);
   }
   sockStream = DIOSOCKSTREAM(obj);
   // EML_SUCCESS(STAFCV_OK);
   return sockStream;
}

//----------------------------------
dioSockStream *  dioFactory:: newSockStream (const char * name
		, const char * hostName, long port) {
   IDREF_T id;
   if( soc->idObject(name,"dioSockStream",id) ){
      EML_CONTEXT("ERROR: You already have a '%s'.\n",name);
      EML_ERROR(DUPLICATE_OBJECT_NAME);
   }
   static dioSockStream* p;
   p = new dioSockStream(name,hostName,port);
   if( !soc->idObject(name,"dioSockStream",id) ){
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   addEntry(id);
/***      The open is performed in the kam function.
   if( !p->open(mode) ){
      p->close();
      soc->deleteObject(name,"dioSockStream");
      EML_ERROR(CANT_OPEN_SOCKET);
   }
***/
   // EML_SUCCESS(STAFCV_OK);
   return p;

}
//:----------------------------------------------- PRIV FUNCTIONS     --

// ---------------------------------------------------------------------


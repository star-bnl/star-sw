/*:Copyright 1995, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        dioClasses.hh
**:DESCRIPTION: DIO Classes
**:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
**:BUGS:        -- STILL IN DEVELOPMENT --
**:HISTORY:     12dec95-v000a-cet- creation
**:<--------------------------------------------------------------------
*/

#ifdef __cplusplus
#ifndef DIOCLASSES_HH
#define DIOCLASSES_HH

//:----------------------------------------------- INCLUDES           --
#include <stream.h>
#include "dstype.h"
#include "dsxdr.h"
#include "asuLib.h"
#include "socLib.h"
#include "duiLib.h"
#include "dio_types.h"

//:=============================================== CLASS              ==
class dioStream: public virtual socObject {
public:
//:----------------------------------------------- CTORS & DTOR       --
   dioStream();
   virtual ~dioStream();

//:----------------------------------------------- ATTRIBUTES         --
   virtual DIO_MODE_T mode ();
   virtual DIO_STATE_T state ();
   virtual char * location();

//- OVERRIDE VIRTUAL
   virtual char * listing();

//:----------------------------------------------- PUB FUNCTIONS      --
//- OVERRIDE VIRTUALS
   virtual unsigned char implementsInterface (const char * iface);

   virtual STAFCV_T close ();
   virtual STAFCV_T getEvent (tdmDataset* destination);
   virtual STAFCV_T open (DIO_MODE_T mode);
   virtual STAFCV_T putEvent (tdmDataset* source);

protected:
//:----------------------------------------------- PRIV VARIABLES     --
   DIO_MODE_T myMode;
   DIO_STATE_T myState;

   XDR myXDR;

//:----------------------------------------------- PRIV FUNCTIONS     --
};

//:=============================================== CLASS              ==
class dioFileStream: public virtual dioStream {

public:
//:----------------------------------------------- CTORS & DTOR       --
   dioFileStream(const char * name, const char * fileName);
   virtual ~dioFileStream();

//:----------------------------------------------- ATTRIBUTES         --
   virtual char * fileName ();

//- OVERRIDE VIRTUAL
   virtual char * location();

//:----------------------------------------------- PUB FUNCTIONS      --
//- OVERRIDE VIRTUALS
   virtual unsigned char implementsInterface (const char * iface);
   virtual STAFCV_T close ();
   virtual STAFCV_T open (DIO_MODE_T mode);

protected:
//:----------------------------------------------- PRIV VARIABLES     --
   FILE* myFile;
   char *myFileName;

//:----------------------------------------------- PRIV FUNCTIONS     --

};

//:=============================================== CLASS              ==
class dioTapeStream: public virtual dioStream {

public:
//:----------------------------------------------- CTORS & DTOR       --
   dioTapeStream(const char * name, const char * tapeName);
   virtual ~dioTapeStream();

//:----------------------------------------------- ATTRIBUTES         --
   virtual char * tapeName ();
   virtual void bufferSize (long bufferSize);
   virtual long bufferSize ();

//- OVERRIDE VIRTUAL
   virtual char * location();

//:----------------------------------------------- PUB FUNCTIONS      --
//- OVERRIDE VIRTUALS
   virtual unsigned char implementsInterface (const char * iface);

protected:
//:----------------------------------------------- PRIV VARIABLES     --
   FILE* myTape;
   char *myTapeName;
   long myBufferSize;

//:----------------------------------------------- PRIV FUNCTIONS     --
};

//:=============================================== CLASS              ==
class dioSockStream: public virtual dioStream {

public:
//:----------------------------------------------- CTORS & DTOR       --
   dioSockStream(const char * name, const char * hostName
		, unsigned long port=7253);
   virtual ~dioSockStream();

//:----------------------------------------------- ATTRIBUTES         --
   virtual long port ();
   virtual char * host ();
   virtual long socketNumber ();
   virtual long bufferSize ();
   virtual void maxHandshakes (long count);
   virtual long maxHandshakes ();

//- OVERRIDE VIRTUAL
   virtual char * location();

//:----------------------------------------------- PUB FUNCTIONS      --
//- OVERRIDE VIRTUALS
   virtual unsigned char implementsInterface (const char * iface);

    virtual STAFCV_T acknowledgeRequest();
    virtual STAFCV_T requestAcknowledge();

//:- override virtuals
   virtual STAFCV_T close ();
   virtual STAFCV_T open (DIO_MODE_T mode);
   virtual STAFCV_T getEvent (tdmDataset* destination);
   virtual STAFCV_T putEvent (tdmDataset* destination);

protected:
//:----------------------------------------------- PRIV VARIABLES     --
   long myPort;
   char * myHost;
   int mySocket;		// << INTEGER, not LONG
   long myBufferSize;
   long myMaxHandshakes;

//:----------------------------------------------- PRIV FUNCTIONS     --
   // hjw 02oct97, not used.  virtual int openServer();
   // hjw 02oct97, not used.  virtual int openClient();
};

//:=============================================== CLASS              ==
class dioFactory: public virtual socFactory {

public:
//:----------------------------------------------- CTORS & DTOR       --
   dioFactory(const char * name);
   virtual ~dioFactory();

//:----------------------------------------------- ATTRIBUTES         --
//:**NONE**

//:----------------------------------------------- PUB FUNCTIONS      --
//- OVERRIDE VIRTUALS
   virtual unsigned char implementsInterface (const char * iface);
   virtual char * list ();

//:- Stream ------------------------------------
   virtual STAFCV_T deleteStream (const char * name);
   virtual dioStream* findStream (const char * name);
   virtual dioStream* getStream (IDREF_T id);

//:- FileStream --------------------
   virtual STAFCV_T deleteFileStream (const char * name);
   virtual dioFileStream* findFileStream (const char * name);
   virtual dioFileStream* getFileStream (IDREF_T id);
   virtual dioFileStream* newFileStream (const char * name
                , const char * fileName);

//:- SockStream --------------------
   virtual STAFCV_T deleteSockStream (const char * name);
   virtual dioSockStream* findSockStream (const char * name);
   virtual dioSockStream* getSockStream (IDREF_T id);
   virtual dioSockStream* newSockStream (const char * name
		, const char * hostName, long port);

protected:
//:----------------------------------------------- PRIV VARIABLES     --
//:----------------------------------------------- PRIV FUNCTIONS     --

};

//----------------------------------------------------------------------
CORBA_TIE(dioFileStream)
CORBA_TIE(dioFactory)

#endif /*DIOCLASSES_HH*/
#endif /*__cplusplus*/


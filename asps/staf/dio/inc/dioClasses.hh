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
#include "dui_types.h"
#include "dui_globals.h"
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

//:----------------------------------------------- PUB FUNCTIONS      --
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

//:----------------------------------------------- PUB FUNCTIONS      --
//:- override virtuals
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

//:----------------------------------------------- PUB FUNCTIONS      --
//:**NONE**

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
   dioSockStream(const char * name, const char * hostName, long port);
   virtual ~dioSockStream();

//:----------------------------------------------- ATTRIBUTES         --
   virtual long port ();
   virtual char * host ();
   virtual long socketNumber ();
   virtual long bufferSize ();
   virtual void maxHandshakes (long count);
   virtual long maxHandshakes ();


//:----------------------------------------------- PUB FUNCTIONS      --
    virtual STAFCV_T acknowledgeRequest();
    virtual STAFCV_T requestAcknowledge();

//:- override virtuals
   virtual STAFCV_T close ();
   virtual STAFCV_T open (DIO_MODE_T mode);
   virtual STAFCV_T getEvent (tdmDataset* destination);

protected:
//:----------------------------------------------- PRIV VARIABLES     --
   long myPort;
   char * myHost;
   int mySocket;		// << INTEGER, not LONG
   long myBufferSize;
   long myMaxHandshakes;

//:----------------------------------------------- PRIV FUNCTIONS     --
   virtual int openServer();
   virtual int openClient();
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
   virtual char * list ();

//:- Stream ------------------------------------
   virtual STAFCV_T deleteStream (const char * name);
   virtual STAFCV_T findStream (const char * name, dioStream*& stream);
   virtual STAFCV_T getStream (IDREF_T id, dioStream*& stream);

//:- FileStream --------------------
   virtual STAFCV_T deleteFileStream (const char * name);
   virtual STAFCV_T findFileStream (const char * name
                , dioFileStream*& fileStream);
   virtual STAFCV_T getFileStream (IDREF_T id
                , dioFileStream*& fileStream);
   virtual STAFCV_T newFileStream (const char * name
                , const char * fileName);

//:- SockStream --------------------
   virtual STAFCV_T deleteSockStream (const char * name);
   virtual STAFCV_T findSockStream (const char * name
                , dioSockStream*& sockStream);
   virtual STAFCV_T getSockStream (IDREF_T id
                , dioSockStream*& sockStream);
   virtual STAFCV_T newSockStream (const char * name
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


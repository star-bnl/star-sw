/*:Copyright 1995, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         dio_kam.c
*:DESCRIPTION:  C KUIP Action Modules for DIO
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      11nov96-v001a-cet- seperate KAM and non-KAM func.s
*:HISTORY:      12dec95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/
#undef CORBA

/*-------------------------------------------- INCLUDES             --*/

#include <stdlib.h>
#include <stdio.h>

#include "kuip.h"

#include "asuLib.h"
#include "emlLib.h"
#include "dio_macros.h"
#include "dio_types.h"
#include "dio_globals.h"

/*-------------------------------------------- TYPEDEFS             --*/
/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_dio_count_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* DIO/COUNT
*:<---------------------------------------------------------------------
*/
void kam_dio_count_()
{
   long npars = ku_npar();      /* number of KUIP parameters */

   STAFCV_T status = dio_count();
}

STAFCV_T dio_count()
{
   printf("DIO:\tObject count = %d \n",dio->count());
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_dio_list_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* DIO/LIST
*:<---------------------------------------------------------------------
*/
void kam_dio_list_()
{
   long npars = ku_npar();      /* number of KUIP parameters */

   STAFCV_T status = dio_list();
}

STAFCV_T dio_list()
{
   printf("%s",dio->list() );
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_dio_newfilestream_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/NEWFILESTREAM NAME
*:<---------------------------------------------------------------------
*/
void kam_dio_newfilestream_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* filestream name */
   char* file = ku_gets();      /* data file name */
   char* mode = ku_getc();      /* I/O mode */

   STAFCV_T status = dio_newfilestream(name, file, mode);
}

STAFCV_T dio_newfilestream(char* name, char* file, char* mode)
{
   DIO_MODE_T iomode=dio_text2mode(mode);
   dioFileStream* stream;

#ifndef NOTCL
   switch (file[0]) {
      case '-':
      switch (file[1]) {
	 case 'b': case 'B':
	    file = NULL;
	    if( !dio_tcltk_browsefile(&name,&file,&iomode)
	    ||  !(file != NULL)
	    ){
	       EML_ERROR(KAM_NO_FILE_SELECTED);
	    }
      }
   }
#endif /*NOTCL*/

   if( !dio->newFileStream(name,file) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   if( !(DIO_UNKNOWN_MODE == iomode) ){
      if( !dio->findFileStream(name, stream) ){
	 EML_ERROR(KAM_OBJECT_NOT_FOUND);
      }
      if( !stream->open(iomode) ){
	 EML_ERROR(KAM_METHOD_FAILURE);
      }
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_dio_newsockstream_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TDM/NEWSOCKSTREAM ALIAS HOST PORT
*:<---------------------------------------------------------------------
*/
void kam_dio_newsockstream_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* sockStream name */
   char* host = ku_getc();      /* remote host name */
   long port = ku_geti();	/* socket port number */

   STAFCV_T status = dio_newsockstream(name, host, port);
}

STAFCV_T dio_newsockstream(char* name, char* host, long port)
{
   dioSockStream* stream;

   if( !dio->newSockStream(name,host,port) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

//######################################################################
void kam_diofilestream_open_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* filestream name */
   char* mode = ku_getc();      /* I/O mode */

   STAFCV_T status = diofilestream_open(name, mode);
}

STAFCV_T diofilestream_open(char* name, char* mode)
{
   DIO_MODE_T iomode=dio_text2mode(mode);
   dioFileStream* stream;

   if( !(DIO_UNKNOWN_MODE == iomode) ){
      if( !dio->findFileStream(name, stream) ){
	 EML_ERROR(KAM_OBJECT_NOT_FOUND);
      }
      if( !stream->open(iomode) ){
	 EML_ERROR(KAM_METHOD_FAILURE);
      }
   }
   EML_SUCCESS(STAFCV_OK);
}
//######################################################################

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_diofilestream_close_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* DIO/FILESTREAM/CLOSE NAME
*:<---------------------------------------------------------------------
*/
void kam_diofilestream_close_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* stream name */

   STAFCV_T status = diofilestream_close(name);
}

STAFCV_T diofilestream_close(char* name)
{
   dioFileStream* stream;

   if( !dio->findFileStream(name, stream) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   if( !stream->close() ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_diofilestream_getevent_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* DIO/FILESTREAM/GETEVENT NAME [ PATH ]
*:<---------------------------------------------------------------------
*/
void kam_diofilestream_getevent_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* stream name */
   char* dest = ku_gets();      /* destination dataset */

   STAFCV_T status = diofilestream_getevent(name, dest);
}

STAFCV_T diofilestream_getevent(char* name, char* dest)
{
   dioFileStream* stream;
   tdmDataset* destination=NULL;

//-DEBUG-("Find file stream (%s).\n",name);
   if( !dio->findFileStream(name, stream) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }

//-DEBUG-("Find destination dataset (%s)(%p).\n",dest,destination);
   if( !tdm->findDataset(dest, destination)
   ||  !(NULL != destination)
   ){
      if( !tdm->newDataset(dest, 100) 		// HACK - 100
      ||  !tdm->findDataset(dest, destination)
      ){
	 EML_ERROR(KAM_OBJECT_NOT_FOUND);
      }
   }

//-DEBUG-("Get the event.\n");
   if( !stream->getEvent(destination) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_diofilestream_putevent_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* DIO/FILESTREAM/GETEVENT NAME [ PATH ]
*:<---------------------------------------------------------------------
*/
void kam_diofilestream_putevent_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* stream name */
   char* sour = ku_gets();      /* source dataset */

   STAFCV_T status = diofilestream_putevent(name, sour);
}

STAFCV_T diofilestream_putevent(char* name, char* sour)
{
   dioFileStream* stream;
   tdmDataset* source=NULL;

   if( !dio->findFileStream(name, stream) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   if( !tdm->findDataset(sour, source)
   ||  !( NULL != source )
   ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   if( !stream->putEvent(source) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_diofilestream_mode_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* DIO/FILESTREAM/MODE NAME
*:<---------------------------------------------------------------------
*/
void kam_diofilestream_mode_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* stream name */

   STAFCV_T status = diofilestream_mode(name);
}

STAFCV_T diofilestream_mode(char* name)
{
   dioFileStream* stream;

   if( !dio->findFileStream(name, stream) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("DIO:\tStream mode = (%s) \n"
		, dio_mode2text(stream->mode()));
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_diofilestream_state_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* DIO/FILESTREAM/MODE NAME
*:<---------------------------------------------------------------------
*/
void kam_diofilestream_state_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* stream name */

   STAFCV_T status = diofilestream_state(name);
}

STAFCV_T diofilestream_state(char* name)
{
   dioFileStream* stream;

   if( !dio->findFileStream(name, stream) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("DIO:\tStream state = (%s) \n"
		, dio_state2text(stream->state()));
   EML_SUCCESS(STAFCV_OK);
}

/*-------------------------------------------------------------------*/
void kam_diofilestream_filename_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char* name = ku_gets();      /* stream name */

   STAFCV_T status = diofilestream_filename(name);
}

STAFCV_T diofilestream_filename(char* name)
{
   dioFileStream* stream;

   if( !dio->findFileStream(name, stream) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("DIO:\tFile name = (%s) \n"
		, stream->fileName());
   EML_SUCCESS(STAFCV_OK);
}
 
//######################################################################
// Socket Streams
//######################################################################
/*-------------------------------------------------------------------*/
void kam_diosockstream_open_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* stream name */
   char* mode = ku_getc();      /* I/O mode */

   STAFCV_T status = diosockstream_open(name, mode);
}

STAFCV_T diosockstream_open(char* name, char* mode)
{
   DIO_MODE_T iomode=dio_text2mode(mode);
   dioStream* stream;

   if( !(DIO_UNKNOWN_MODE == iomode) ){
      if( !dio->findStream(name, stream) ){
	 EML_ERROR(KAM_OBJECT_NOT_FOUND);
      }
      if( !stream->open(iomode) ){
	 EML_ERROR(KAM_METHOD_FAILURE);
      }
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_diosockstream_close_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* stream name */

   STAFCV_T status = diosockstream_close(name);
}

STAFCV_T diosockstream_close(char* name)
{
   dioStream* stream;

   if( !dio->findStream(name, stream) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   if( !stream->close() ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_diosockstream_getevent_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* stream name */
   char* dest = ku_gets();      /* destination dataset */

   STAFCV_T status = diosockstream_getevent(name, dest);
}

STAFCV_T diosockstream_getevent(char* name, char* dest)
{
   dioStream* stream;
   tdmDataset* destination=NULL;

//-("Find stream (%s).\n",name);
   if( !dio->findStream(name, stream) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }

//-("Find destination dataset (%s)(%p).\n",dest,destination);
   if( !tdm->findDataset(dest, destination)
   ||  !(NULL != destination)
   ){
      if( !tdm->newDataset(dest, 100) 		// HACK - 100
      ||  !tdm->findDataset(dest, destination)
      ){
	 EML_ERROR(KAM_OBJECT_NOT_FOUND);
      }
   }

//-("Get the event.\n");
   if( !stream->getEvent(destination) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_diosockstream_putevent_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* stream name */
   char* sour = ku_gets();      /* source dataset */

   STAFCV_T status = diosockstream_putevent(name, sour);
}

STAFCV_T diosockstream_putevent(char* name, char* sour)
{
   dioStream* stream;
   tdmDataset* source=NULL;

   if( !dio->findStream(name, stream) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   if( !tdm->findDataset(sour, source)
   ||  !( NULL != source )
   ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   if( !stream->putEvent(source) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_diosockstream_mode_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* stream name */

   STAFCV_T status = diosockstream_mode(name);
}

STAFCV_T diosockstream_mode(char* name)
{
   dioStream* stream;

   if( !dio->findStream(name, stream) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("DIO:\tStream mode = (%s) \n"
		, dio_mode2text(stream->mode()));
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_diosockstream_state_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* stream name */

   STAFCV_T status = diosockstream_state(name);
}

STAFCV_T diosockstream_state(char* name)
{
   dioStream* stream;

   if( !dio->findStream(name, stream) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("DIO:\tStream state = (%s) \n"
		, dio_state2text(stream->state()));
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_diosockstream_host_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char* name = ku_gets();      /* stream name */

   STAFCV_T status = diosockstream_host(name);
}

STAFCV_T diosockstream_host(char* name)
{
   dioSockStream* stream;

   if( !dio->findSockStream(name, stream) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("DIO:\tHost name = (%s) \n"
		, stream->host());
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_diosockstream_port_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char* name = ku_gets();      /* stream name */

   STAFCV_T status = diosockstream_port(name);
}

STAFCV_T diosockstream_port(char* name)
{
   dioSockStream* stream;

   if( !dio->findSockStream(name, stream) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("DIO:\tPort number = (%d) \n"
		, stream->port());
   EML_SUCCESS(STAFCV_OK);
}

/*-------------------------------------------------------------------*/
void kam_diosockstream_maxhandshakes_()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char* name = ku_gets();      /* stream name */
   long count = ku_geti();      /* maximum count value */

   STAFCV_T status = diosockstream_maxhandshakes(name, count);
}

STAFCV_T diosockstream_maxhandshakes(char* name, long count)
{
   dioSockStream* stream;

   if( !dio->findSockStream(name, stream) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   if( count >= 0 ){
      stream->maxHandshakes(count);
   }
   else {
      printf("DIO:\tWill try %d handshakes \n"
		,stream->maxHandshakes());
   }
   EML_SUCCESS(STAFCV_OK);
}


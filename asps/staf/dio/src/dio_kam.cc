/*:Copyright 1995, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         dio_kam.c
*:DESCRIPTION:  C KUIP Action Modules for DIO
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      12dec95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/
#undef CORBA

/*-------------------------------------------- INCLUDES             --*/

#include <stdlib.h>
#include <stdio.h>

#include "kuip.h"

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
void kam_dio_count_(){kam_dio_count();}
int kam_dio_count()
{
   long npars = ku_npar();      /* number of KUIP parameters */

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
void kam_dio_list_(){kam_dio_list();}
int kam_dio_list()
{
   long npars = ku_npar();      /* number of KUIP parameters */

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
void kam_dio_newfilestream_(){kam_dio_newfilestream();}
int kam_dio_newfilestream()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* filestream name */
   char* file = ku_gets();      /* data file name */
   char* mode = ku_getc();      /* I/O mode */

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
void kam_dio_newsockstream_(){kam_dio_newsockstream();}
int kam_dio_newsockstream()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* sockStream name */
   char* host = ku_getc();      /* remote host name */
   long port = ku_geti();	/* socket port number */

   dioSockStream* stream;

   if( !dio->newSockStream(name,host,port) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

//######################################################################
void kam_diofilestream_open_(){kam_diofilestream_open();}
int kam_diofilestream_open()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* filestream name */
   char* mode = ku_getc();      /* I/O mode */

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
void kam_diofilestream_close_(){kam_diofilestream_close();}
int kam_diofilestream_close()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* stream name */

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
void kam_diofilestream_getevent_(){kam_diofilestream_getevent();}
int kam_diofilestream_getevent()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* stream name */
   char* dest = ku_gets();      /* destination dataset */

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
void kam_diofilestream_putevent_(){kam_diofilestream_putevent();}
int kam_diofilestream_putevent()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* stream name */
   char* sour = ku_gets();      /* source dataset */

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
void kam_diofilestream_mode_(){kam_diofilestream_mode();}
int kam_diofilestream_mode()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* stream name */

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
void kam_diofilestream_state_(){kam_diofilestream_state();}
int kam_diofilestream_state()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* stream name */

   dioFileStream* stream;

   if( !dio->findFileStream(name, stream) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("DIO:\tStream state = (%s) \n"
		, dio_state2text(stream->state()));
   EML_SUCCESS(STAFCV_OK);
}

/*-------------------------------------------------------------------*/
void kam_diofilestream_filename_(){kam_diofilestream_filename();}
int kam_diofilestream_filename()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char* name = ku_gets();      /* stream name */

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
void kam_diosockstream_open_(){kam_diosockstream_open();}
int kam_diosockstream_open()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* stream name */
   char* mode = ku_getc();      /* I/O mode */

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
void kam_diosockstream_close_(){kam_diosockstream_close();}
int kam_diosockstream_close()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* stream name */

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
void kam_diosockstream_getevent_(){kam_diosockstream_getevent();}
int kam_diosockstream_getevent()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* stream name */
   char* dest = ku_gets();      /* destination dataset */

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
void kam_diosockstream_putevent_(){kam_diosockstream_putevent();}
int kam_diosockstream_putevent()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* stream name */
   char* sour = ku_gets();      /* source dataset */

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
void kam_diosockstream_mode_(){kam_diosockstream_mode();}
int kam_diosockstream_mode()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* stream name */

   dioStream* stream;

   if( !dio->findStream(name, stream) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("DIO:\tStream mode = (%s) \n"
		, dio_mode2text(stream->mode()));
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_diosockstream_state_(){kam_diosockstream_state();}
int kam_diosockstream_state()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* stream name */

   dioStream* stream;

   if( !dio->findStream(name, stream) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("DIO:\tStream state = (%s) \n"
		, dio_state2text(stream->state()));
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_diosockstream_host_(){kam_diosockstream_host();}
int kam_diosockstream_host()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char* name = ku_gets();      /* stream name */

   dioSockStream* stream;

   if( !dio->findSockStream(name, stream) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("DIO:\tHost name = (%s) \n"
		, stream->host());
   EML_SUCCESS(STAFCV_OK);
}
 
/*-------------------------------------------------------------------*/
void kam_diosockstream_port_(){kam_diosockstream_port();}
int kam_diosockstream_port()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char* name = ku_gets();      /* stream name */

   dioSockStream* stream;

   if( !dio->findSockStream(name, stream) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("DIO:\tPort number = (%d) \n"
		, stream->port());
   EML_SUCCESS(STAFCV_OK);
}

/*-------------------------------------------------------------------*/
void kam_diosockstream_handshake_(){kam_diosockstream_handshake();}
int kam_diosockstream_handshake()
{
   long npars = ku_npar(); /* no. of KUIP param.s */
   char* name = ku_gets();      /* stream name */
   char* truth = ku_getc();      /* truth value */

   dioSockStream* stream;

   if( !dio->findSockStream(name, stream) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   switch (truth[0]) {
   case '-': case '?':
       if( stream->requiresHandshake() ){
	  printf("DIO:\tDOES require handshake \n");
       } else {
	  printf("DIO:\tDOES NOT require handshake \n");
       }
       break;
   case 'T':
       stream->requiresHandshake(TRUE);
       break;
   case 'F':
       stream->requiresHandshake(FALSE);
       break;
   default:
       EML_ERROR(KAM_INVALID_BOOLEAN);
       break;
   }
   EML_SUCCESS(STAFCV_OK);
}


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
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
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

   if( !dio->list() ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
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

   if( !dio->newFileStream(name,file,iomode) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

//######################################################################
void kam_diofilestream_open_(){kam_diofilestream_open();}
int kam_diofilestream_open()
{
   EML_ERROR(KAM_NOT_YET_IMPLEMENTED);
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
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
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
   char* path = ku_gets();      /* DUI path */

   dioFileStream* stream;

   if( !dio->findFileStream(name, stream) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   if( !stream->getEvent(path) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
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
   char* path = ku_gets();      /* DUI path */

   dioFileStream* stream;

   if( !dio->findFileStream(name, stream) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   if( !stream->putEvent(path) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
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
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
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
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}



/*:Copyright 1995, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         ami_kam.c
*:DESCRIPTION:  C KUIP Action Modules for AMI
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

#include "asuAlloc.h"

#include "ami_macros.h"
#include "ami_types.h"
#include "ami_globals.h"

/*-------------------------------------------- TYPEDEFS             --*/
/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_ami_count_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* AMI/COUNT
*:<---------------------------------------------------------------------
*/
void kam_ami_count_(){kam_ami_count();}
int kam_ami_count()
{
   long npars = ku_npar();      /* number of KUIP parameters */

   printf("AMI:\tObject count = %d \n",ami->count());
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_ami_list_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* AMI/LIST
*:<---------------------------------------------------------------------
*/
void kam_ami_list_(){kam_ami_list();}
int kam_ami_list()
{
   long npars = ku_npar();      /* number of KUIP parameters */

   printf("%s",ami->list());
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_ami_call_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* AMI/MODULE/RANK
*:<---------------------------------------------------------------------
*/
void kam_ami_call_(){kam_ami_call();}
int kam_ami_call()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* PAM name */
   STRING_SEQ_T tables;		/* table names */

//- Get table names. -**
   tables._length = tables._maximum = npars - 1;
   tables._buffer = new char*[tables._length];
   for( int i=0;i<tables._length;i++ ){
      tables._buffer[i] = NULL;
      char* tbl = ku_gets();
      tables._buffer[i] = new char[strlen(tbl) +1];
      strcpy(tables._buffer[i],tbl);
   }

//- Tell the AMI Broker to invoke the PAM.
   if( !ami->callInvoker(name, tables) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);

}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_amiinvoker_rank_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* AMI/MODULE/RANK PAM
*:<---------------------------------------------------------------------
*/
void kam_amiinvoker_rank_(){kam_amiinvoker_rank();}
int kam_amiinvoker_rank()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* PAM name */

   amiInvoker* pam;		/* amiInvoker object */

   if( !ami->findInvoker(name, pam) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   printf("AMI:\tAnalysis module rank = %d \n",pam->rank());
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_amiinvoker_show_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* AMI/MODULE/TBLSPEC PAM [ NTBL ]
*:<---------------------------------------------------------------------
*/
/*=HACK========================== THIS SUBROUTINE SHOULD BE CHANGED. =*/
void kam_amiinvoker_show_(){kam_amiinvoker_show();}
int kam_amiinvoker_show()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* PAM name */
   char* opts = ku_gets();	/* Options */

   amiInvoker* pam;		/* amiInvoker object */

   if( !ami->findInvoker(name, pam) ){
      EML_ERROR(KAM_OBJECT_NOT_FOUND);
   }
   int rank = pam->rank();
   char *c;
   for( int i=0;i<rank;i++ ){
      printf("AMI:\tTable Specification = ...\n%s .\n"
		,c = pam->tableSpec(i));
      ASUFREE(c);
   }
   EML_SUCCESS(STAFCV_OK);
}

/***********************************************************************
************************************************************************
********	THE FOLLOWING COMMANDS ARE NOT IMPLEMENTED.	********
********	SHOULD THEY BE?					********
************************************************************************
***********************************************************************/

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_amiinvoker_init_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* AMI/MODULE/INIT PAM
*:<---------------------------------------------------------------------
*/
void kam_amiinvoker_init_(){kam_amiinvoker_init();}
int kam_amiinvoker_init()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* PAM name */

   EML_ERROR(KAM_NOT_YET_IMPLEMENTED);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_amiinvoker_start_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* AMI/MODULE/START PAM
*:<---------------------------------------------------------------------
*/
void kam_amiinvoker_start_(){kam_amiinvoker_start();}
int kam_amiinvoker_start()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* PAM name */

   EML_ERROR(KAM_NOT_YET_IMPLEMENTED);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_amiinvoker_stop_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* AMI/MODULE/STOP PAM
*:<---------------------------------------------------------------------
*/
void kam_amiinvoker_stop_(){kam_amiinvoker_stop();}
int kam_amiinvoker_stop()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* PAM name */

   EML_ERROR(KAM_NOT_YET_IMPLEMENTED);
}


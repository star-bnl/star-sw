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
void kam_ami_count_()
{
   long npars = ku_npar();      /* number of KUIP parameters */

	STAFCV_T status = ami_count();
}
/*------------------------------------*/
STAFCV_T ami_count()
{
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
void kam_ami_list_()
{
   long npars = ku_npar();      /* number of KUIP parameters */

	STAFCV_T status = ami_list();
}
/*------------------------------------*/
STAFCV_T ami_list()
{
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
void kam_ami_call_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* pname = ku_gets();	/* PAM name */
   char **tnames;		/* array of TABLE names */

   tnames = new char*[npars-1];
   for( int np=1;np<npars;np++ ){
      tnames[np-1] = ku_gets();
   }
	STAFCV_T status = ami_call(pname,npars-1,tnames);
}
/*------------------------------------*/
STAFCV_T ami_call(char* name,long ntabs,char **tnames)
{
   STRING_SEQ_T tbls;		/* table names */

//- Get table names. -**
   tbls._length = tbls._maximum = ntabs;
   tbls._buffer = tnames;

//- Tell the AMI Broker to invoke the PAM.
//- WARNING!!! - PAM status already recorded!!!
   return ami->callInvoker(name, tbls);
// if( !ami->callInvoker(name, tbls) ){
//    EML_ERROR(KAM_METHOD_FAILURE);
// }
// EML_SUCCESS(STAFCV_OK);

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
void kam_amiinvoker_rank_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* PAM name */

	STAFCV_T status = amiinvoker_rank(name);
}
/*------------------------------------*/
STAFCV_T amiinvoker_rank(char* name)
{
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
void kam_amiinvoker_show_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* PAM name */
   char* opts = ku_gets();	/* Options */

	STAFCV_T status = amiinvoker_show(name, opts);
}
/*------------------------------------*/
STAFCV_T amiinvoker_show(char* name, char* opts)
{
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
void kam_amiinvoker_init_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* PAM name */

	STAFCV_T status = amiinvoker_init(name);
}
/*------------------------------------*/
STAFCV_T amiinvoker_init(char* name)
{
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
void kam_amiinvoker_start_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* PAM name */

	STAFCV_T status = amiinvoker_start(name);
}
/*------------------------------------*/
STAFCV_T amiinvoker_start(char* name)
{
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
void kam_amiinvoker_stop_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();	/* PAM name */

	STAFCV_T status = amiinvoker_stop(name);
}
/*------------------------------------*/
STAFCV_T amiinvoker_stop(char* name)
{
   EML_ERROR(KAM_NOT_YET_IMPLEMENTED);
}


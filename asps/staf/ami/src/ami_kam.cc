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
void 
kam_ami_count_()
{
  ami_count();
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
void 
kam_ami_list_()
{
  ami_list();
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_amimodule_call_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* AMI/MODULE/RANK
*:<---------------------------------------------------------------------
*/
void 
kam_amimodule_call_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* pname = ku_gets();	/* PAM name */
   char **tnames;		/* array of TABLE names */

   tnames = new char*[npars-1];
   for( int np=1;np<npars;np++ ){
      tnames[np-1] = ku_gets();
   }
   ami_call(pname,npars-1,tnames);
   delete[] tnames;
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
void 
kam_ami_call_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* pname = ku_gets();	/* PAM name */
   char **tnames;		/* array of TABLE names */

   tnames = new char*[npars-1];
   for( int np=1;np<npars;np++ ){
      tnames[np-1] = ku_gets();
   }
   EML_CONTEXT("ERROR: This is an obsolete command.\n"
	       "Please use AMI/MODULE/CALL instead.\n");
   EML_WARNING(OBSOLETE_COMMAND);
   ami_call(pname,npars-1,tnames);
   delete[] tnames;
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_amimodule_rank_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* AMI/MODULE/RANK PAM
*:<---------------------------------------------------------------------
*/
void 
kam_amimodule_rank_()
{
   char* name = ku_gets();	/* PAM name */
   
   amimodule_rank(name);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_amimodule_show_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* AMI/MODULE/TBLSPEC PAM [ NTBL ]
*:<---------------------------------------------------------------------
*/
/*=HACK========================== THIS SUBROUTINE SHOULD BE CHANGED. =*/
void 
kam_amimodule_show_()
{
   char* name = ku_gets();	/* PAM name */

   amimodule_show(name);
}

/***********************************************************************
************************************************************************
********	THE FOLLOWING COMMANDS ARE NOT IMPLEMENTED.	********
********	SHOULD THEY BE?					********
************************************************************************
***********************************************************************/

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_amimodule_init_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* AMI/MODULE/INIT PAM
*:<---------------------------------------------------------------------
*/
void 
kam_amimodule_init_()
{
   char* name = ku_gets();	/* PAM name */

   amimodule_init(name);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_amimodule_start_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* AMI/MODULE/START PAM
*:<---------------------------------------------------------------------
*/
void 
kam_amimodule_start_()
{
   char* name = ku_gets();	/* PAM name */

   amimodule_start(name);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_amimodule_stop_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* AMI/MODULE/STOP PAM
*:<---------------------------------------------------------------------
*/
void 
kam_amimodule_stop_()
{
   char* name = ku_gets();	/* PAM name */

   amimodule_stop(name);
}


//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:	ami_init.C
//:DESCRIPTION:	Functions  to initialize AMI 
//:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:	-- STILL IN DEVELOPMENT --
//:HISTORY:	08dec95-v000a-cet- creation
//:<--------------------------------------------------------------------

#include "ami_macros.h"
#include "ami_types.h"
#include "amiClasses.hh"
#include "ami_globals.h"

#include "fortranc.h"

#define ami_def_ F77_NAME(ami_def,AMI_DEF)
extern CC_P void type_of_call ami_def_();

amiBroker *ami;
extern CC_P int ami_load(amiBroker* broker); //automatically generated

//:>--------------------------------------------------------------------
//:ROUTINE:	int ami_init()
//:DESCRIPTION:	Initialize AMI
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int ami_init()
{
#ifndef QUIET_ASP
   EML_MESSAGE("AMI:Initializing. ");
#endif

/*- Define the AMI KUIP commands. -*/
   ami_def_();

   return TRUE;
}

//:>--------------------------------------------------------------------
//:ROUTINE:	int ami_start()
//:DESCRIPTION:	Start AMI
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int ami_start()
{
#ifndef QUIET_ASP
   EML_MESSAGE("AMI:Starting. ");
#endif

/*- Create the AMI Broker. -*/
   ami = new amiBroker("ami");

/*- Load all the amiInvokers for this Broker. -*/
   return ami_load(ami);
}

//:>--------------------------------------------------------------------
//:ROUTINE:	int ami_stop()
//:DESCRIPTION:	Stop AMI
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int ami_stop()
{
#ifndef QUIET_ASP
   EML_MESSAGE("AMI:Stopping. ");
#endif

/*- Delete the AMI Broker.
   delete ami;
unecessary -- soc will do it. -*/

   return TRUE;
}


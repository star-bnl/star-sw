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
   EML_MESSAGE("Initializing AMI.");

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
   EML_MESSAGE("Starting AMI.");

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
   EML_MESSAGE("Stopping AMI.");

/*- Delete the AMI Broker.
   delete ami;
unecessary -- soc will do it. -*/

   return TRUE;
}


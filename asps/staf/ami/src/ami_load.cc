//:Copyright 1996, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:	ami_load.C
//:DESCRIPTION:	Function  to load amiInvokers into amiBroker.
//:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:	v000a-Should be automatically generated.
//:HISTORY:	05mar96-v002c-cet- NULL for amiMoast
//:HISTORY:	13feb96-v001c-cet- pam_init to pam_load_ami
//:HISTORY:	07feb96-v001b-cet- add a fifth PAM
//:HISTORY:	05feb96-v001a-cet- load four PAMs
//:HISTORY:	03jan96-v000a-cet- creation
//:<--------------------------------------------------------------------

/*------------------------------------------------ INCLUDES         --*/
#include "amiLib.h"

/*------------------------------------------------ PROTOTYPES       --*/
extern CC_P int ami_load(amiBroker *broker);

//:>--------------------------------------------------------------------
//:ROUTINE:	int ami_load
//:DESCRIPTION:	Load amiInvokers into amiBroker.
//:ARGUMENTS:	amiBroker *broker
//:RETURN VALUE:TRUE or FALSE
//:<--------------------------------------------------------------------
int 
ami_load(amiBroker *broker)
{
  static void *pb = &broker;
  printf("*****************************************************\n");
  printf("*****************************************************\n");
  printf("*****                                           *****\n");
  printf("*****               WARNING !!!                 *****\n");
  printf("*****                                           *****\n");
  printf("*****  You are loading no analysis modules.     *****\n");
  printf("*****                                           *****\n");
  printf("*****************************************************\n");
  printf("*****************************************************\n");
  return TRUE;
}


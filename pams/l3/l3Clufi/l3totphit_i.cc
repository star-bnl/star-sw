/*------------------------------------------------------------------
FILE:         l3totphit_i.cc
DESCRIPTION:  Interface functions for l3totphit
AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
BUGS:         Should be automatically generated
HISTORY:      putDateHere-v000a-hpl- Creation.
*/
/*------------------------------------------- INCLUDES            */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "l3totphit.h"
/*------------------------------------------ TYPEDEFS             */
/*------------------------------------------ GLOBALS              */
/*------------------------------------------ PROTOTYPES           */
/*
*:>----------------------------------------------------------------
*:ROUTINE:      STAFCV_T l3totphit_load_ami()
*:DESCRIPTION:  Initialize l3totphit
*:ARGUMENTS:    amiBroker *broker       - broker for AMI object
*:RETURN VALUE: TRUE or FALSE
*:<----------------------------------------------------------------
*/
STAFCV_T l3totphit_load_ami(amiBroker *broker)
{
L3TOTPHIT_FT l3totphit_call = l3totphit_;
  STRING_SEQ_T specs;
  specs._length = specs._maximum = L3TOTPHIT_RANK;
  specs._buffer = new char*[L3TOTPHIT_RANK];

  specs._buffer[0] = new char[strlen(HITARRAY_SPEC)+1];
  strcpy(specs._buffer[0],HITARRAY_SPEC);
  specs._buffer[1] = new char[strlen(TCL_TPHIT_SPEC)+1];
  strcpy(specs._buffer[1],TCL_TPHIT_SPEC);

 broker->deleteInvoker("l3totphit");  broker->newInvoker("l3totphit",L3TOTPHIT_RANK
               ,(FNC_PTR_T)l3totphit_call ,specs             );
  for( int i=0;i<specs._maximum;i++ ){
     delete specs._buffer[i];
  }
  delete[] specs._buffer;
  printf("l3totphit module loaded\n");
  return TRUE;
}

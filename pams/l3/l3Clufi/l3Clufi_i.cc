/*------------------------------------------------------------------
FILE:         l3Clufi_i.cc
DESCRIPTION:  Interface functions for l3Clufi
AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
BUGS:         Should be automatically generated
HISTORY:      putDateHere-v000a-hpl- Creation.
*/
/*------------------------------------------- INCLUDES            */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "l3Clufi.h"
/*------------------------------------------ TYPEDEFS             */
/*------------------------------------------ GLOBALS              */
/*------------------------------------------ PROTOTYPES           */
/*
*:>----------------------------------------------------------------
*:ROUTINE:      STAFCV_T l3Clufi_load_ami()
*:DESCRIPTION:  Initialize l3Clufi
*:ARGUMENTS:    amiBroker *broker       - broker for AMI object
*:RETURN VALUE: TRUE or FALSE
*:<----------------------------------------------------------------
*/
STAFCV_T l3Clufi_load_ami(amiBroker *broker)
{
L3CLUFI_FT l3Clufi_call = l3Clufi_;
  STRING_SEQ_T specs;
  specs._length = specs._maximum = L3CLUFI_RANK;
  specs._buffer = new char*[L3CLUFI_RANK];

  specs._buffer[0] = new char[strlen(PIXELARRAY_SPEC)+1];
  strcpy(specs._buffer[0],PIXELARRAY_SPEC);
  specs._buffer[1] = new char[strlen(HITARRAY_SPEC)+1];
  strcpy(specs._buffer[1],HITARRAY_SPEC);

 broker->deleteInvoker("l3Clufi");  broker->newInvoker("l3Clufi",L3CLUFI_RANK
               ,(FNC_PTR_T)l3Clufi_call ,specs             );
  for( int i=0;i<specs._maximum;i++ ){
     delete specs._buffer[i];
  }
  delete[] specs._buffer;
  printf("l3Clufi module loaded\n");
  return TRUE;
}

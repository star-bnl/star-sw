/*------------------------------------------------------------------
FILE:         sft_am_i.cc
DESCRIPTION:  Interface functions for sft_am
AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
BUGS:         Should be automatically generated
HISTORY:      putDateHere-v000a-hpl- Creation.
*/
/*------------------------------------------- INCLUDES            */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sft_am.h"
/*------------------------------------------ TYPEDEFS             */
/*------------------------------------------ GLOBALS              */
/*------------------------------------------ PROTOTYPES           */
/*
*:>----------------------------------------------------------------
*:ROUTINE:      STAFCV_T sft_am_load_ami()
*:DESCRIPTION:  Initialize sft_am
*:ARGUMENTS:    amiBroker *broker       - broker for AMI object
*:RETURN VALUE: TRUE or FALSE
*:<----------------------------------------------------------------
*/
STAFCV_T sft_am_load_ami(amiBroker *broker)
{
SFT_AM_FT sft_am_call = sft_am_;
  STRING_SEQ_T specs;
  specs._length = specs._maximum = SFT_AM_RANK;
  specs._buffer = new char*[SFT_AM_RANK];

  specs._buffer[0] = new char[strlen(SFT_PAR_SPEC)+1];
  strcpy(specs._buffer[0],SFT_PAR_SPEC);
  specs._buffer[1] = new char[strlen(SCS_SPT_SPEC)+1];
  strcpy(specs._buffer[1],SCS_SPT_SPEC);
  specs._buffer[2] = new char[strlen(SFT_VERTEX_SPEC)+1];
  strcpy(specs._buffer[2],SFT_VERTEX_SPEC);

  broker->newInvoker("sft_am",SFT_AM_RANK
               ,(FNC_PTR_T)sft_am_call ,specs);
  for( int i=0;i<specs._maximum;i++ ){
     delete specs._buffer[i];
  }
  delete[] specs._buffer;
  printf("sft_am module loaded\n");
  return TRUE;
}

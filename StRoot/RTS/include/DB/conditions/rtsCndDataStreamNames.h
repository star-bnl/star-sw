#ifndef _RTSCNDDATASTREAMNAMES_H_
#define _RTSCNDDATASTREAMNAMES_H_

#include "rtsDbConstants.h"

struct rtsCndDataStreamNames
{
  //////////////////////

  // 10/04 - replace indexing with hash
  //int idx_rn;
  unsigned int hash;

  int idx_stream;
  //////////////////////

  char dataStreamNames[DB_MAX_STR_LEN];
};

#endif

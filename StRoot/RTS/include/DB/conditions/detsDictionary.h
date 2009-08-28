#ifndef _DETSDICTIONARY_H_
#define _DETSDICTIONARY_H_

#include "rtsDbConstants.h"

struct detsDict
{
  int first_idx_rn;

  int det_idx;
  
  char dbField[DB_MAX_STR_LEN];
  char label[DB_MAX_STR_LEN];
};

#endif

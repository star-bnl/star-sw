#ifndef SpyJPped_h
#define SpyJPped_h

#include <TArrayF.h>

#include "SpyGeneric.h"

class SpyJPped: public SpyGeneric {
 public:
  SpyJPped();
  bool sense(FILE *fd);
}; 

#endif

#ifndef SpyJPfreq_h
#define SpyJPfreq_h

#include <TArrayF.h>

#include "SpyGeneric.h"

class SpyJPfreq: public SpyGeneric {
 public:
  SpyJPfreq();
  bool sense(FILE *fd);
}; 

#endif

#ifndef SpyCorruption_h
#define SpyCorruption_h

#include <TArrayF.h>

#include "SpyGeneric.h"

class SpyCorruption: public SpyGeneric {
  TArrayF a0,d0;// last histo and last difference
  int lastSum;
 public:
  SpyCorruption();
  bool sense(FILE *fd);
}; 

#endif

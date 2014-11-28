#ifndef SpyCopyCat_h
#define SpyCopyCat_h


#include "SpyGeneric.h"
class EztEmcRawData;

class SpyCopyCat: public SpyGeneric { // for one data block
  TH1F *h;
  char type;
  int  icr; 
  void incrHist(int i0, int i1);
  int nEve;
 public:
  SpyCopyCat(char x, int y);
  bool sense(FILE *fd);
  void accumulate(EztEmcRawData *e, EztEmcRawData *t);
}; 

#endif

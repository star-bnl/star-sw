#ifndef SpyGeneric_h
#define SpyGeneric_h

class TH1;

#include <TString.h>

class SpyGeneric {
 protected:
  TH1 * h;
  TString text;

 public:
  SpyGeneric(){};
  SpyGeneric* set(TH1 * h0, TString tx="" );
  virtual  ~SpyGeneric(){};
  virtual bool sense(FILE *fd)=0;
};

#endif 

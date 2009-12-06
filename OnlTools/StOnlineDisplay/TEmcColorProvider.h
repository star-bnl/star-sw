#ifndef STAR_TEmcColorProvider
#define STAR_TEmcColorProvider

#include "TEmcSizeProvider.h"

//_____________________________________________________________________________
class TEmcColorProvider  : public TEmcSizeProvider {
public:
   TEmcColorProvider(btow_t **src=0,void  **available=0,Int_t *len=0) 
    : TEmcSizeProvider (src,available,len) { }
   virtual ~TEmcColorProvider() {}
   virtual Int_t NextAttribute();
   virtual void ComputerScale() {  fScale = 4095/1000; }
};

#endif

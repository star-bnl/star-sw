#ifdef _VANILLA_ROOT_

/// PicoDst headers
#include "StThreeVector.h"

#ifdef __ROOT__
#include <TBuffer.h>
#include <TClass.h>

ClassImpT(StThreeVector,float);

//_________________
template <> void StThreeVector<float>::Streamer(TBuffer &R__b) {
//	Stream an object of class StThreeVectorD.
   if (R__b.IsReading()) {
     UInt_t R__s, R__c;
     Version_t R__v = R__b.ReadVersion(&R__s, &R__c);
     if (R__v > 2){Class()->ReadBuffer(R__b, this, R__v, R__s, R__c); return;}
     R__b >> mX1;
     R__b >> mX2;
     R__b >> mX3;
   }
   else Class()->WriteBuffer(R__b,this);
}

//_________________
template <> void StThreeVector<double>::Streamer(TBuffer &R__b) {
//	Stream an object of class StThreeVectorD.
   if (R__b.IsReading()) {
     UInt_t R__s, R__c;
     Version_t R__v = R__b.ReadVersion(&R__s, &R__c);
     if (R__v > 2){Class()->ReadBuffer(R__b, this, R__v, R__s, R__c); return;}
     R__b >> mX1;
     R__b >> mX2;
     R__b >> mX3;
   }
   else Class()->WriteBuffer(R__b,this);
}
#endif

#endif //ifdef _VANILLA_ROOT_

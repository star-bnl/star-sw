#include "StMatrix.hh"
#ifdef __ROOT__
#include "TBuffer.h"
#include "TClass.h"
ClassImpT(StMatrix,float);
//________________________________________________________________________________
template <> void StMatrix<float>::Streamer(TBuffer &R__b)
{
   if (R__b.IsReading()) {
     UInt_t R__s, R__c;
     Version_t R__v = R__b.ReadVersion(&R__s, &R__c);
     if (R__v > 2){Class()->ReadBuffer(R__b, this, R__v, R__s, R__c); return;}
     if (mElement) delete [] mElement;
     mElement = 0;
     R__b.ReadArray(mElement);  // allocates memory if mElement = 0
     R__b >> mRow;
     R__b >> mCol;
     R__b >> mSize;
   }
   else Class()->WriteBuffer(R__b,this);
}
//________________________________________________________________________________
template <> void StMatrix<double>::Streamer(TBuffer &R__b)
{
   if (R__b.IsReading()) {
     UInt_t R__s, R__c;
     Version_t R__v = R__b.ReadVersion(&R__s, &R__c);
     if (R__v > 2){Class()->ReadBuffer(R__b, this, R__v, R__s, R__c); return;}
     if (mElement) delete [] mElement;
     mElement = 0;
     R__b.ReadArray(mElement);  // allocates memory if mElement = 0
     R__b >> mRow;
     R__b >> mCol;
     R__b >> mSize;
   }
   else Class()->WriteBuffer(R__b,this);
}
#endif

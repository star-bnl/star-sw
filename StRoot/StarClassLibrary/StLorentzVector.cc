#include "StLorentzVector.hh"
#ifdef __ROOT__
#include "TBuffer.h"
#include "TClass.h"
ClassImpT(StLorentzVector,double);
ClassImpT(StLorentzVector,float);
//________________________________________________________________________________
template <> void StLorentzVector<float>::Streamer(TBuffer &R__b)
{
//	Stream an object of class StLorentzVectorD.
   if (R__b.IsReading()) {
     UInt_t R__s, R__c;
     Version_t R__v = R__b.ReadVersion(&R__s, &R__c);
     if (R__v > 2){Class()->ReadBuffer(R__b, this, R__v, R__s, R__c); return;}
     if (R__v==1) R__b.SetBufferOffset(R__b.Length()+10);	//skip TObject
     mThreeVector.Streamer(R__b);
     R__b >> mX4;
   }
   else Class()->WriteBuffer(R__b,this);
}
//________________________________________________________________________________
template <> void StLorentzVector<double>::Streamer(TBuffer &R__b)
{
//	Stream an object of class StLorentzVectorD.
   if (R__b.IsReading()) {
     UInt_t R__s, R__c;
     Version_t R__v = R__b.ReadVersion(&R__s, &R__c);
     if (R__v > 2){Class()->ReadBuffer(R__b, this, R__v, R__s, R__c); return;}
     if (R__v==1) R__b.SetBufferOffset(R__b.Length()+10);	//skip TObject
     mThreeVector.Streamer(R__b);
     R__b >> mX4;
   }
   else Class()->WriteBuffer(R__b,this);
}
#endif

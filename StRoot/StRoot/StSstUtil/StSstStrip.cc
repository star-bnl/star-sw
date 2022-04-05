//$Id: StSstStrip.cc,v 1.2 2016/06/14 19:01:48 bouchet Exp $
//
//$Log: StSstStrip.cc,v $
//Revision 1.2  2016/06/14 19:01:48  bouchet
//coverity : UNINIT_CTOR
//
//Revision 1.1  2015/06/23 16:26:20  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:32  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein

#include <string.h>
#include "StSstStrip.hh"

//________________________________________________________________________________
StSstStrip::StSstStrip(Int_t rNStrip, Int_t rDigitSig, Float_t rSigma, Int_t rPedestal, Int_t *rIdMcHit,Int_t *rIdMcTrack) {
  memset (first, 0, last - first);
  mNStrip    = rNStrip;
  mDigitSig  = rDigitSig;
  mSigma     = rSigma;
  mPedestal  = rPedestal;
  for(Int_t e=0;e<5;e++)  {
    mIdMcHit[e] = rIdMcHit[e];  
    mIdMcTrack[e] = rIdMcTrack[e];
    mIdHit[e] = 0;
  }
  mMcStrip = 0;
  mNHits = 1;
  mAnalogSig = 0;
  mPrevStrip = 0;
  mNextStrip = 0;
}
//________________________________________________________________________________
StSstStrip::StSstStrip(Int_t rNStrip, Int_t rDigitSig, Float_t rSigma, Int_t rPedestal) {
  memset (first, 0, last - first);
  mNStrip    = rNStrip;
  mDigitSig  = rDigitSig;  
  mSigma     = rSigma;
  mPedestal  = rPedestal;
  for(Int_t e=0;e<5;e++) {
    mIdMcHit[e] = 0;
    mIdMcTrack[e] = 0;
    mIdHit[e] =0;
  }
  mMcStrip = 0;
  mNHits = 1;
  mAnalogSig = 0;
  mPrevStrip = 0;
  mNextStrip = 0;
}
//________________________________________________________________________________
StSstStrip::StSstStrip(Int_t rNStrip, Int_t rIdHit, Int_t rMcHit, Int_t rMcTrack, Float_t rAnalogSig) {
  memset (first, 0, last - first);
  mNStrip        = rNStrip;
  mIdHit[0]      = rIdHit;
  mIdMcHit[0]    = rMcHit;
  mIdMcTrack[0]  = rMcTrack;
  for(Int_t e=1;e<5;e++) {
    mIdMcHit[e] = 0;
    mIdMcTrack[e] = 0;
    mIdHit[e] = 0;
  }
  mNHits         = 1;
  mAnalogSig     = rAnalogSig;
  mDigitSig      = 0;  
  mSigma     = 0;
  mPedestal  = 0;
  mMcStrip = 0;
  mPrevStrip =0;
  mNextStrip = 0;
}
//________________________________________________________________________________
StSstStrip::StSstStrip(Int_t rNStrip, Int_t rMcStrip, Int_t rDigitSig, Float_t rAnalogSig, Int_t *rIdMcHit) {
  memset (first, 0, last - first);
  mNStrip        = rNStrip;
  mMcStrip       = rMcStrip;
  mDigitSig      = rDigitSig;  
  mAnalogSig     = rAnalogSig;
  for(Int_t e=0;e<5;e++)  mIdMcHit[e] = rIdMcHit[e];
  for(Int_t e=0;e<5;e++) {
    mIdMcTrack[e] = 0;
    mIdHit[e] = 0;
  }
  mNHits = 1;
  mSigma     = 0;
  mPedestal  = 0;
  mPrevStrip = 0;
  mNextStrip = 0;
}
//_________________
StSstStrip::StSstStrip(Int_t rNStrip, Int_t rMcStrip, Int_t rDigitSig, Float_t rAnalogSig, Int_t *rIdMcHit,Int_t *rIdMcTrack) {
  memset (first, 0, last - first);
  mNStrip        = rNStrip;
  mMcStrip       = rMcStrip;
  mDigitSig      = rDigitSig;  
  mAnalogSig     = rAnalogSig;
  for(Int_t e=0;e<5;e++)  {
    mIdHit[e] =0;
    mIdMcHit[e]   = rIdMcHit[e];  
    mIdMcTrack[e] = rIdMcTrack[e];
  }
  mSigma     = 0;
  mPedestal  = 0;
  mNHits = 1;
  mPrevStrip = 0;
  mNextStrip = 0;
}

//________________________________________________________________________________
StSstStrip::StSstStrip(Int_t rNStrip, Int_t rMcStrip, Float_t rAnalogSig) {
  memset (first, 0, last - first);
  mNStrip        = rNStrip;
  mMcStrip       = rMcStrip;
  mAnalogSig     = rAnalogSig;
  mDigitSig  = 0;
  mSigma     = 0;
  mPedestal  = 0;
  for(Int_t e=0;e<5;e++)  {
    mIdMcHit[e] = 0;
    mIdMcTrack[e] = 0;
    mIdHit[e] = 0;
  }
  mNHits = 1;
  mPrevStrip = 0;
  mNextStrip = 0;
}
//________________________________________________________________________________
StSstStrip::StSstStrip(Int_t rNStrip, Int_t rDigitSig) {
  memset (first, 0, last - first);
  mNStrip        = rNStrip;
  mDigitSig      = rDigitSig;  
  mSigma     = 0;
  mPedestal  = 0;
  for(Int_t e=0;e<5;e++)  {
    mIdMcHit[e] = 0;
    mIdMcTrack[e] = 0;
    mIdHit[e] = 0;
  }
  mMcStrip = 0;
  mNHits = 1;
  mAnalogSig = 0;
  mPrevStrip = 0;
  mNextStrip = 0;
}
//________________________________________________________________________________
void StSstStrip::copyTo(StSstStrip *ptrClone) {
  memcpy (ptrClone->first, first, last - first);
  ptrClone->mPrevStrip = ptrClone->mNextStrip = 0;
}
//________________________________________________________________________________



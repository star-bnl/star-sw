// $Id: StSsdStrip.cc,v 1.2 2008/10/20 19:25:32 bouchet Exp $
//
// $Log: StSsdStrip.cc,v $
// Revision 1.2  2008/10/20 19:25:32  bouchet
// propagate idMcTrack used for IdTruth
//
// Revision 1.1  2006/10/16 16:43:29  bouchet
// StSsdUtil regroups now methods for the classes StSsdStrip, StSsdCluster and StSsdPoint
//
// Revision 1.6  2005/03/18 14:15:51  lmartin
// missing CVS header added
//

#include <string.h>
#include "StSsdStrip.hh"

//________________________________________________________________________________
StSsdStrip::StSsdStrip(Int_t rNStrip, Int_t rDigitSig, Float_t rSigma, Int_t rPedestal, Int_t *rIdMcHit,Int_t *rIdMcTrack) {
  memset (first, 0, last - first);
  mNStrip    = rNStrip;
  mDigitSig  = rDigitSig;
  mSigma     = rSigma;
  mPedestal  = rPedestal;
  for(Int_t e=0;e<5;e++)  {
    mIdMcHit[e] = rIdMcHit[e];  
    mIdMcTrack[e]  = rIdMcTrack[e];
  }
}
//________________________________________________________________________________
StSsdStrip::StSsdStrip(Int_t rNStrip, Int_t rDigitSig, Float_t rSigma, Int_t rPedestal) {
  memset (first, 0, last - first);
  mNStrip    = rNStrip;
  mDigitSig  = rDigitSig;  
  mSigma     = rSigma;
  mPedestal  = rPedestal;
  for(Int_t e=0;e<5;e++) mIdMcHit[e] = 0;
}
//________________________________________________________________________________
StSsdStrip::StSsdStrip(Int_t rNStrip, Int_t rIdHit, Int_t rMcHit, Int_t rMcTrack, Float_t rAnalogSig) {
  memset (first, 0, last - first);
  mNStrip        = rNStrip;
  mIdHit[0]      = rIdHit;
  mIdMcHit[0]    = rMcHit;
  mIdMcTrack[0]  = rMcTrack;
  mNHits         = 1;
  mAnalogSig     = rAnalogSig;
}
//________________________________________________________________________________
StSsdStrip::StSsdStrip(Int_t rNStrip, Int_t rMcStrip, Int_t rDigitSig, Float_t rAnalogSig, Int_t *rIdMcHit) {
  memset (first, 0, last - first);
  mNStrip        = rNStrip;
  mMcStrip       = rMcStrip;
  mDigitSig      = rDigitSig;  
  mAnalogSig     = rAnalogSig;
  for(Int_t e=0;e<5;e++)  mIdMcHit[e] = rIdMcHit[e];
}
//_________________
StSsdStrip::StSsdStrip(Int_t rNStrip, Int_t rMcStrip, Int_t rDigitSig, Float_t rAnalogSig, Int_t *rIdMcHit,Int_t *rIdMcTrack) {
  memset (first, 0, last - first);
  mNStrip        = rNStrip;
  mMcStrip       = rMcStrip;
  mDigitSig      = rDigitSig;  
  mAnalogSig     = rAnalogSig;
  for(Int_t e=0;e<5;e++)  {
    mIdMcHit[e]   = rIdMcHit[e];  
    mIdMcTrack[e] = rIdMcTrack[e];
  }
}

//________________________________________________________________________________
StSsdStrip::StSsdStrip(Int_t rNStrip, Int_t rMcStrip, Float_t rAnalogSig) {
  memset (first, 0, last - first);
  mNStrip        = rNStrip;
  mMcStrip       = rMcStrip;
  mAnalogSig     = rAnalogSig;
}
//________________________________________________________________________________
StSsdStrip::StSsdStrip(Int_t rNStrip, Int_t rDigitSig) {
  memset (first, 0, last - first);
  mNStrip        = rNStrip;
  mDigitSig      = rDigitSig;  
}
//________________________________________________________________________________
void StSsdStrip::copyTo(StSsdStrip *ptrClone) {
  memcpy (ptrClone->first, first, last - first);
  ptrClone->mPrevStrip = ptrClone->mNextStrip = 0;
}
//________________________________________________________________________________



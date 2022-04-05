//$Id: StSstStrip.hh,v 1.1 2015/06/23 16:26:19 jeromel Exp $
//
//$Log: StSstStrip.hh,v $
//Revision 1.1  2015/06/23 16:26:19  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:32  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein

#ifndef STSSTSTRIP_HH
#define STSSTSTRIP_HH

#define SST_MAXIDMCHIT 5
#include "Rtypes.h"
class StSstStrip
{
 public:

  StSstStrip(Int_t rNStrip, Int_t rDigitSig, Float_t rSigma, Int_t rPedestal);
  StSstStrip(Int_t rNStrip, Int_t rDigitSig, Float_t rSigma, Int_t rPedestal, Int_t *rMcHit,Int_t *rMcTrack);
  // Sls
  StSstStrip(Int_t rNStrip, Int_t rIdHit,    Int_t rMcHit,   Int_t rMcTrack,  Float_t rAnalogSig);
  // Spa
  StSstStrip(Int_t rNStrip, Int_t rMcStrip, Int_t rDigitSig, Float_t rAnalogSig, Int_t *rIdMcHit,Int_t *rMcTrack);
  StSstStrip(Int_t rNStrip, Int_t rMcStrip, Int_t rDigitSig, Float_t rAnalogSig, Int_t *rIdMcHit);
  StSstStrip(Int_t rNStrip, Int_t rMcStrip, Float_t rAnalogSig);
  StSstStrip(Int_t rNStrip, Int_t rDigitSig);

  ~StSstStrip() {}
  void        setNStrip(Int_t rNStrip)                 { mNStrip = rNStrip; }
  void        setMcStrip(Int_t rMcStrip)               { mMcStrip = rMcStrip; }
  void        setIdHit(Int_t rIdHit, Int_t iR)         { mIdHit[iR] = rIdHit; }
  void        setIdMcHit(Int_t rIdMcHit, Int_t iR)     { mIdMcHit[iR] = rIdMcHit; }
  void        setIdMcTrack(Int_t rIdMcTrack, Int_t iR) { mIdMcTrack[iR] = rIdMcTrack; }
  void        setDigitSig(Int_t rDigitSig)             { mDigitSig = rDigitSig; }
  void        setNHits(Int_t rNHits)                   { mNHits = rNHits; }
  void        setAnalogSig(Float_t rAnalogSig)         { mAnalogSig = rAnalogSig; }
  void        setPrevStrip(StSstStrip *rPrevStrip)     { mPrevStrip = rPrevStrip; }
  void        setNextStrip(StSstStrip *rNextStrip)     { mNextStrip = rNextStrip; }
  
  void        setSigma(Float_t rSigma)                 {  mSigma = rSigma; }
  void        setPedestal(Int_t iPedestal)             {  mPedestal = iPedestal; }
  
  Int_t       getNStrip()                              { return mNStrip; }
  Int_t       getMcStrip()                             { return mMcStrip; }
  Int_t       getIdHit(Int_t iR)                       { return mIdHit[iR]; }
  Int_t       getIdMcHit(Int_t iR)                     { return mIdMcHit[iR]; }
  Int_t       getIdMcTrack(Int_t iR)                   { return mIdMcTrack[iR]; }
  Int_t       getDigitSig()                            { return mDigitSig; }
  Int_t       getNHits()                               { return mNHits; }
  Float_t     getAnalogSig()                           { return mAnalogSig; }
  StSstStrip* getPrevStrip()                           { return mPrevStrip; }
  StSstStrip* getNextStrip()                           { return mNextStrip; }
  
  Float_t     getSigma()                               { return mSigma; }
  Int_t       getPedestal()                            { return mPedestal; }
  void        copyTo(StSstStrip *ptrClone);
  void        print();  
  
private:
  Char_t      first[1];
  Int_t       mNStrip;
  Int_t       mMcStrip;
  Int_t       mIdHit[5];
  Int_t       mIdMcHit[5];
  Int_t       mIdMcTrack[5];
  Int_t       mDigitSig;
  Int_t       mNHits;
  Int_t       mPedestal;
  Float_t     mSigma;
  Float_t     mAnalogSig;
  StSstStrip *mPrevStrip;
  StSstStrip *mNextStrip;
  Char_t      last[1];
};

#endif

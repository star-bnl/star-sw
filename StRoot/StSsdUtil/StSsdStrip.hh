// $Id: StSsdStrip.hh,v 1.3 2008/10/20 19:25:48 bouchet Exp $
//
// $Log: StSsdStrip.hh,v $
// Revision 1.3  2008/10/20 19:25:48  bouchet
// propagate idMcTrack used for IdTruth
//
// Revision 1.2  2008/05/07 22:48:36  bouchet
// calculation of quality of hits used embedding
//
// Revision 1.1  2006/10/16 16:43:29  bouchet
// StSsdUtil regroups now methods for the classes StSsdStrip, StSsdCluster and StSsdPoint
//
// Revision 1.6  2005/03/18 14:15:51  lmartin
// missing CVS header added
//

#ifndef STSSDSTRIP_HH
#define STSSDSTRIP_HH

#define SSD_MAXIDMCHIT 5
#include "Rtypes.h"
class StSsdStrip
{
 public:

  StSsdStrip(Int_t rNStrip, Int_t rDigitSig, Float_t rSigma, Int_t rPedestal);
  StSsdStrip(Int_t rNStrip, Int_t rDigitSig, Float_t rSigma, Int_t rPedestal, Int_t *rMcHit,Int_t *rMcTrack);
  // Sls
  StSsdStrip(Int_t rNStrip, Int_t rIdHit,    Int_t rMcHit,   Int_t rMcTrack,  Float_t rAnalogSig);
  // Spa
  StSsdStrip(Int_t rNStrip, Int_t rMcStrip, Int_t rDigitSig, Float_t rAnalogSig, Int_t *rIdMcHit,Int_t *rMcTrack);
  StSsdStrip(Int_t rNStrip, Int_t rMcStrip, Int_t rDigitSig, Float_t rAnalogSig, Int_t *rIdMcHit);
  StSsdStrip(Int_t rNStrip, Int_t rMcStrip, Float_t rAnalogSig);
  StSsdStrip(Int_t rNStrip, Int_t rDigitSig);

  ~StSsdStrip() {}
  void        setNStrip(Int_t rNStrip)                 { mNStrip = rNStrip; }
  void        setMcStrip(Int_t rMcStrip)               { mMcStrip = rMcStrip; }
  void        setIdHit(Int_t rIdHit, Int_t iR)         { mIdHit[iR] = rIdHit; }
  void        setIdMcHit(Int_t rIdMcHit, Int_t iR)     { mIdMcHit[iR] = rIdMcHit; }
  void        setIdMcTrack(Int_t rIdMcTrack, Int_t iR) { mIdMcTrack[iR] = rIdMcTrack; }
  void        setDigitSig(Int_t rDigitSig)             { mDigitSig = rDigitSig; }
  void        setNHits(Int_t rNHits)                   { mNHits = rNHits; }
  void        setAnalogSig(Float_t rAnalogSig)         { mAnalogSig = rAnalogSig; }
  void        setPrevStrip(StSsdStrip *rPrevStrip)     { mPrevStrip = rPrevStrip; }
  void        setNextStrip(StSsdStrip *rNextStrip)     { mNextStrip = rNextStrip; }

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
  StSsdStrip* getPrevStrip()                           { return mPrevStrip; }
  StSsdStrip* getNextStrip()                           { return mNextStrip; }

  Float_t     getSigma()                               { return mSigma; }
  Int_t       getPedestal()                            { return mPedestal; }
  void        copyTo(StSsdStrip *ptrClone);
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
  StSsdStrip *mPrevStrip;
  StSsdStrip *mNextStrip;
  Char_t      last[1];
};

#endif

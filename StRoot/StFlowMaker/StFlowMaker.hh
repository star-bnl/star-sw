#ifndef StFlowMaker_HH
#define StFlowMaker_HH

///////////////////////////////////////////////////////////////////////////////
//
// StFlowMaker.hh
//  $Id: StFlowMaker.hh,v 1.2 1999/11/11 23:08:58 posk Exp $
//
// Description: 
//  Interface to StEvent for StFlowEvent and base class for
//    StFlowTagMaker and StFlowAnalysisMaker
//
// Environment:
//  Software developed for the STAR Detector at LBNL
//
// Author List: 
//  Raimond Snellings and Art Poskanzer, LBNL, 6/99
//
// History:
//  $Log: StFlowMaker.hh,v $
//  Revision 1.2  1999/11/11 23:08:58  posk
//  Rearrangement of files.
//
//  Revision 1.1  1999/11/04 19:02:14  snelling
//  First check in of StFlowMaker. It contains the common code from
//  StFlowTagMaker and StFlowAnalysisMaker.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include "StChain.h"
#include "StMaker.h"
#include "StEvent.h"
//#include "StFlowEvent.h"
//#include "StGlobalTrack.h"
#include "FlowTag.h"
//#include "StFlowCutEvent.hh"
//#include "StFlowCutTrack.hh"
#include "TString.h"
#include "TVector2.h"
class StFlowEvent;

class StFlowMaker : public StMaker {

public:

          StFlowMaker(const Char_t *name="Flow");
  virtual ~StFlowMaker();

  Int_t   Init();
  void    PrintInfo();
  Int_t   Make();
  Int_t   Finish();

  FlowTag_st*  TagPointer() const;        // returns pointer to the tag table
  StFlowEvent* FlowEventPointer() const;  // returbs pointer to the StFlowEvent
  Int_t    Tags();
  Double_t PhiWeight(Float_t mPhi, Int_t eventN, Int_t harN) const;
  TVector2 Q(Int_t eventN, Int_t harN);
  Float_t  q(Int_t eventN, Int_t harN);
  Float_t  MeanPt(Int_t eventN, Int_t harN);

protected:

  FlowTag_st*  mFlowTag;    //! the tag table to fill
  StEvent*     mEvent;      //! pointer to DST data
  StFlowEvent* mFlowEvent;  //! pointer to micro-DST data
  TString      MakerName;

  // C++ way to define constants in the header
  enum {nHars = 4, nSubs = 4};
  enum {nPhiBins = 60};

  Double_t mPhiWgt[nSubs/2][nHars][nPhiBins]; // To make event palne isotropic

  TVector2 mQSub[nSubs][nHars];     // flow vector sub-events
  TVector2 mQ[nSubs/2][nHars];      // flow vector
  Float_t  mMulSub[nSubs][nHars];   // multiplicity sub-events
  Float_t  mSumPtSub[nSubs][nHars]; // Pt sum sub-events
  Float_t  mPsiSub[nSubs][nHars];   // event plane angle subevents
  Float_t  mMul[nSubs/2][nHars];    // multiplicity
  Float_t  mSumPt[nSubs/2][nHars];  // Pt sum
  Float_t  mQMod[nSubs/2][nHars];   // flow vector magnitude
  Float_t  m_q[nSubs/2][nHars];     // Q/sqroot(Mul)
  Float_t  mPsi[nSubs/2][nHars];    // event plane angle

private:

  StFlowEvent*  fillFlowEvent();

  ClassDef(StFlowMaker, 1)           // macro for rootcint

};

inline FlowTag_st* StFlowMaker::TagPointer() const {return mFlowTag;}
inline StFlowEvent* StFlowMaker::FlowEventPointer() const {return mFlowEvent;}
inline TVector2 StFlowMaker::Q(Int_t eventN, Int_t harN) 
  {return mQ[eventN][harN];}
inline Float_t StFlowMaker::q(Int_t eventN, Int_t harN) 
  {return m_q[eventN][harN];}
inline Float_t StFlowMaker::MeanPt(Int_t eventN, Int_t harN) 
  {return (mMul[eventN][harN]) ? mSumPt[eventN][harN]/mMul[eventN][harN] : 0.;}

#endif

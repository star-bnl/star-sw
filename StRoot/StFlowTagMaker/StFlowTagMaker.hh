#ifndef StFlowTagMaker_HH
#define StFlowTagMaker_HH

///////////////////////////////////////////////////////////////////////////////
//
// StFlowTagMaker.hh
// $Id: StFlowTagMaker.hh,v 1.1 1999/11/11 23:13:02 posk Exp $
//
// Description: 
//  Maker to fill Tag database for flow analysis
//
// Environment:
//  Software developed for the STAR Detector at LBNL
//
// Author List: 
//  Raimond Snellings and Art Poskanzer, LBNL, 6/99
//
//////////////////////////////////////////////////////////////////////
//
// History:
// $Log: StFlowTagMaker.hh,v $
// Revision 1.1  1999/11/11 23:13:02  posk
// Rearrangement of files.
//
// Revision 1.5  1999/09/24 01:23:07  fisyak
// Reduced Include Path
//
// Revision 1.4  1999/08/17 21:47:42  fisyak
// iostream => iostream.h for HP
//
// Revision 1.3  1999/08/09 21:43:06  snelling
// removed parameters from cxx file
//
//
///////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
//#include "StMaker.h"
#include "../StFlowMaker/StFlowMaker.hh"
//#include "FlowTag.h"
//#include "StEvent.h"
//#include "StGlobalTrack.h"
//#include "StChain.h"
//#include "TString.h"
//#include "TH1.h"
//#include "TH2.h"
//#include "TProfile.h"
class TH1F;
class TH1D;
class TProfile;

class StFlowTagMaker : public StFlowMaker 
{

public:

          StFlowTagMaker(const Char_t *name="FlowTag");
  virtual ~StFlowTagMaker();
  Int_t   Init();
  Int_t   Make();
  void    PrintInfo();
  Int_t   Finish();

private:

  void  fillFlowTag();
  Int_t eventPlane(long Multiplicity, Float_t *mPseudoRapidity,
	  Float_t *mPhiAngle, Double_t *mQx, Double_t *mQy,
	  Double_t *mEventPlaneAngle, Int_t OrderParameter, 
          Int_t eventN);
  void  swap(long &a,long &b);
  void  indexx(long n,Float_t arr[], long indx[]);
  Int_t makeHistograms();

  // output Tag info to screen
  void  printTag(ostream& = cout);

  // C++ way to define constants
  enum {nHarmonics = 5, nSubEvents = 4};

  // limits of histograms
  enum {nPhiBins = 100, nEtaBins = 100, nPtBins = 100}; 
  enum {nPsiBins = 100, nSumPtBins = 100, nMultBins = 100, n_qBins = 100}; 

  struct histHarmonic {
    TH1F *mHistPsi;
    TH1F *mHistFlowTagSumPt;
    TH1D *mHistFlowTagMult;
    TH1F *mHist_q;
  };

  struct histSubEvent;
  friend struct histSubEvent;
  struct histSubEvent {
    TH1F *mHistPhi;
    TH1F *mHistPseudoRapidity;
    TH1F *mHistPt;
    TProfile *mHistResolution;
    struct histHarmonic histHarmonics[nHarmonics];
  };

  struct histSubEvent histSubEvents[nSubEvents]; //!

  ClassDef(StFlowTagMaker, 1)                    // macro for rootcint
};

#endif










#ifndef StFlowAnalysisMaker_HH
#define StFlowAnalysisMaker_HH

///////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowAnalysisMaker.hh,v 1.1 1999/11/11 23:16:46 posk Exp $
//
// Author: Art Poskanzer and Raimond Snellings, LBNL, Aug 1999
// Description:  Maker to analyze Flow using the FlowTags
//
///////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowAnalysisMaker.hh,v $
// Revision 1.1  1999/11/11 23:16:46  posk
// Rearrangement of files.
//
// Revision 1.7  1999/11/05 00:02:03  posk
// Changed the flow vector, Q, to a TVector2.
//
// Revision 1.6  1999/10/05 16:54:11  posk
// Added getPhiWeight method for making the event plane isotropic.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include "../StFlowMaker/StFlowMaker.hh"
//#include "StMaker.h"
//#include "FlowTag.h"
class TH1F;
class TH1D;
class TH2F;
class TH2D;
class TH3F;
class TProfile;
//#include "TVector2.h"

class StEvent;

class StFlowAnalysisMaker : public StFlowMaker {

public:

           StFlowAnalysisMaker(const Char_t* name="FlowAnalysis");
  virtual  ~StFlowAnalysisMaker();
  Int_t    Init();
  Int_t    Make();
  void     PrintInfo();
  Int_t    Finish();
  Float_t  getRes(Int_t eventN, Int_t harN) const;
  Float_t  getResErr(Int_t eventN, Int_t harN) const;

private:

  // C++ way to define constants in the header
  enum {nHars = 4, nSubs = 4};
 
  // structures for histograms
  // for each harminic and each sub-event
  struct histSubHars {
    TH1F*     mHistPsiSubs;
  };

  // for each harminic and each event
  struct histFullHars {
    TH1D*     mHistPhi;
    TH1D*     mHistPhiWgt;
    TH1D*     mHistPhiFlat;
    TH1F*     mHistPhiCorr;
    TH1F*     mHistPsiSubCorr;
    TH1F*     mHistPsiSubCorrDiff;
    TH1F*     mHistPsi;
    TH1F*     mHistMult;
    TH1F*     mHistMeanPt;
    TH1F*     mHist_q;
    TH2D*     mHistYield2D;
    TH2D*     mHistSum_v2D;
    TH2F*     mHist_vObs2D;
    TH2F*     mHist_v2D;
    TProfile* mHistBinEta;
    TProfile* mHistBinPt;
  };

  struct histSubs;	
  friend struct histSubs;
  struct histSubs {
    struct histSubHars histSubHar[nHars];
  };
  struct histSubs histSub[nSubs]; //!

  // for each event
  struct histFulls;	
  friend struct histFulls;
  struct histFulls {
    TProfile* mHistCos;
    TH1F*     mHistRes;
    TH3F*     mHistEtaPtPhi3D;
    struct histFullHars histFullHar[nHars];
  };
  struct histFulls histFull[nSubs/2]; //!

  //private:

  void fillTagHistograms();
  void fillFlowHistograms();

  Float_t  mRes[nSubs/2][nHars];    // event plane resolution
  Float_t  mResErr[nSubs/2][nHars]; // event plane resolution error

  ClassDef(StFlowAnalysisMaker, 1)  // macro for rootcint
};

inline Float_t StFlowAnalysisMaker::getRes(Int_t eventN, Int_t harN) const 
  {return mRes[eventN][harN];}
inline Float_t StFlowAnalysisMaker::getResErr(Int_t eventN, Int_t harN) const 
  {return mResErr[eventN][harN];}

#endif

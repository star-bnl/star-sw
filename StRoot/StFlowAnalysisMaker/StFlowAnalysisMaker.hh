#ifndef StFlowAnalysisMaker_HH
#define StFlowAnalysisMaker_HH

///////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowAnalysisMaker.hh,v 1.3 1999/12/04 00:15:40 posk Exp $
//
// Author: Art Poskanzer and Raimond Snellings, LBNL, Aug 1999
// Description:  Maker to analyze Flow using the FlowTags
//
///////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowAnalysisMaker.hh,v $
// Revision 1.3  1999/12/04 00:15:40  posk
// Works with StFlowEvent which works with the new StEvent
//
// Revision 1.2  1999/11/24 18:14:07  posk
// Now reads event quantities with StFlowEvent methods
//
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
#include "StMaker.h"
#include "../StFlowMaker/StFlowEvent.hh"
//#include "../StFlowMaker/StFlowMaker.hh"
#include "../StFlowTagMaker/StFlowTagMaker.hh"
#include "TVector2.h"
class TH1F;
class TH1D;
class TH2F;
class TH2D;
class TH3F;
class TProfile;
class StEvent;

class StFlowAnalysisMaker : public StMaker {

public:

           StFlowAnalysisMaker(const Char_t* name="FlowAnalysis");
  virtual  ~StFlowAnalysisMaker();
  Int_t    Init();
  Int_t    Make();
  void     PrintInfo();
  Int_t    Finish();
  Float_t  Res(Int_t eventN, Int_t harN) const;
  Float_t  ResErr(Int_t eventN, Int_t harN) const;

private:

  StFlowEvent*  pFlowEvent; //!
  FlowTag_st*   pFlowTag;   //!

  enum {nHars = 4, nSels = 2, nSubs = 2};
  enum {nPhiBins = 60};

  TVector2 mQ[nSels][nHars];               // flow vector
  Float_t  mPsiSub[nSels+nSubs][nHars];    // event plane angle subevents
  Float_t  mPsi[nSels][nHars];             // event plane angle
  Float_t  mMult[nSels][nHars];            // multiplicity
  Float_t  mMeanPt[nSels][nHars];          // mean Pt
  Float_t  m_q[nSels][nHars];              // Q/sqrt(Mul)
  TVector2 mQSub[nSels+nSubs][nHars];      // flow vector sub-events
  Float_t  mMeanPtSub[nSels+nSubs][nHars]; // mean Pt
  Float_t  mMultSub[nSels+nSubs][nHars];   // multiplicity
 
  // for histograms
  TH3F*     mHistEtaPtPhi3D; //!
  TH2D*     mHistYieldAll2D; //!
  TProfile* mHistBinEta;     //!
  TProfile* mHistBinPt;      //!
  
  // for each harmonic, each selection, and each sub-event
  struct histSubHars {
    TH1F*     mHistPsiSubs;
  };
  struct histSubs;	
  friend struct histSubs;
  struct histSubs {
    struct histSubHars histSubHar[nHars];
  };
  struct histSubs histSub[nSels+nSubs]; //!

  // for each harmonic and each selection
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
  };

  // for each selection
  struct histFulls;	
  friend struct histFulls;
  struct histFulls {
    TProfile* mHistCos;
    TH1F*     mHistRes;
    struct histFullHars histFullHar[nHars];
  };
  struct histFulls histFull[nSels]; //!

  static const Float_t phiMin;
  static const Float_t phiMax;
  static const Float_t etaMin;
  static const Float_t etaMax;
  static const Float_t ptMin;
  static const Float_t ptMax;
  static const Float_t psiMin;
  static const Float_t psiMax; 
  static const Float_t meanPtMin;
  static const Float_t meanPtMax;
  static const Float_t multMin;
  static const Float_t multMax;
  static const Float_t qMin;
  static const Float_t qMax;
  static const Int_t nPhi3DBins;
//  static const Int_t nPhiBins;
  static const Int_t nEtaBins;
  static const Int_t nPtBins;
  static const Int_t nPsiBins;
  static const Int_t nMeanPtBins;
  static const Int_t nMultBins;
  static const Int_t n_qBins;

  Int_t fillFromTags();
  Int_t fillFromFlowEvent();
  void  fillEventHistograms();
  void  fillParticleHistograms();

  Float_t  mRes[nSels][nHars];      // event plane resolution
  Float_t  mResErr[nSels][nHars];   // event plane resolution error

  ClassDef(StFlowAnalysisMaker, 1)  // macro for rootcint
};

inline Float_t StFlowAnalysisMaker::Res(Int_t eventN, Int_t harN) const 
  {return mRes[eventN][harN];}
inline Float_t StFlowAnalysisMaker::ResErr(Int_t eventN, Int_t harN) const 
  {return mResErr[eventN][harN];}

#endif

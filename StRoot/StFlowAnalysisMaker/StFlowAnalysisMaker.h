///////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowAnalysisMaker.h,v 1.9 2000/03/02 22:55:34 posk Exp $
//
// Authors: Art Poskanzer and Raimond Snellings, LBNL, Aug 1999
//
///////////////////////////////////////////////////////////////////////////////
//
// Description:  Maker to analyze Flow using the FlowTags and/or StFlowEvent
//
///////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowAnalysisMaker.h,v $
// Revision 1.9  2000/03/02 22:55:34  posk
// Changed header file extensions from .hh to .h .
//
// Revision 1.11  2000/02/29 21:55:14  posk
// Removed static const int& statements.
//
// Revision 1.10  2000/02/18 23:44:53  posk
// Added PID and centrality.
//
// Revision 1.9  2000/02/04 16:26:42  posk
// Added correct calculation of event plane resolution for large flow.
//
// Revision 1.8  2000/01/24 23:02:13  posk
// Merged updates
//
// Revision 1.7  2000/01/14 01:35:52  snelling
// changed include path ../FlowMaker/ to FlowMaker/
//
// Revision 1.6  2000/01/13 21:50:24  posk
// Updates and corrections.
//
// Revision 1.5  1999/12/21 21:29:47  posk
// Updated the README file.
//
// Revision 1.4  1999/12/21 01:19:28  posk
// Added more histograms.
//
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

#ifndef StFlowAnalysisMaker_H
#define StFlowAnalysisMaker_H
#include <iostream.h>
#include <stdlib.h>
#include "StMaker.h"
#include "StFlowMaker/StFlowEvent.h"
#include "StFlowTagMaker/StFlowTagMaker.h"
#include "StFlowMaker/StFlowConstants.h"
#include "TVector2.h"
class TH1F;
class TH1D;
class TH2F;
class TH2D;
class TH3F;
class TProfile;

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

  void     FillFromTags();
  void     FillFromFlowEvent();
  void     FillEventHistograms();
  void     FillParticleHistograms();

  TVector2 mQ[Flow::nSels][Flow::nHars];                     // flow vector
  Float_t  mPsi[Flow::nSels][Flow::nHars];                   // event plane angle
  UInt_t   mMult[Flow::nSels][Flow::nHars];                  // multiplicity
  Float_t  mMeanPt[Flow::nSels][Flow::nHars];                // mean Pt
  Float_t  m_q[Flow::nSels][Flow::nHars];                    // Q/sqrt(Mult)
  TVector2 mQSub[Flow::nSels+Flow::nSubs][Flow::nHars];      // flow vector subs
  Float_t  mPsiSub[Flow::nSels+Flow::nSubs][Flow::nHars];    // plane angle subs
  Float_t  mMeanPtSub[Flow::nSels+Flow::nSubs][Flow::nHars]; // mean Pt subs
  UInt_t   mMultSub[Flow::nSels+Flow::nSubs][Flow::nHars];   // multiplicity subs
  Float_t  mRes[Flow::nSels][Flow::nHars];      // event plane resolution
  Float_t  mResErr[Flow::nSels][Flow::nHars];   // event plane resolution error
 
  StFlowEvent*  pFlowEvent; //! pointer to StFlowEvent
  FlowTag_st*   pFlowTag;   //! pointer to StEvent

  // for single histograms
  TH1F*     mHistCharge;        //!
  TH1F*     mHistDca;           //!
  TH1F*     mHistChi2;          //!
  TH1F*     mHistFitPts;        //!
  TH1F*     mHistMaxPts;        //!
  TH1F*     mHistFitOverMax;    //!
  TH1F*     mHistOrigMult;      //!
  TH1F*     mHistMult;          //!
  TH1F*     mHistMultOverOrig;  //!
  TH1F*     mHistVertexZ;       //!
  TH2F*     mHistVertexXY2D;    //!
  TH1F*     mHistEtaSym;        //!
  TH3F*     mHistEtaPtPhi3D;    //!
  TH2D*     mHistYieldAll2D;    //!
  TProfile* mHistBinEta;        //!
  TProfile* mHistBinPt;         //!
  TProfile* mHistCosPhi;        //!
  TH1F*     mHistPidPiPlus;     //!
  TH1F*     mHistPidPiMinus;    //!
  TH1F*     mHistPidProton;     //!
  TProfile* mHistPidMult;       //!
  TH1F*     mHistCent;          //!
  
  // for each harmonic, each selection, and each sub-event
  struct histSubHars {
    TH1F*     mHistPsiSubs;
  };
  struct histSubs;	
  friend struct histSubs;
  struct histSubs {
    struct histSubHars histSubHar[Flow::nHars];
  };
  struct histSubs histSub[Flow::nSels+Flow::nSubs]; //!

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
    TProfile* mHist_vObsEta;
    TProfile* mHist_vObsPt;
    TH2F*     mHist_v2D;
    TProfile* mHist_vEta;
    TProfile* mHist_vPt;
  };

  // for each selection
  struct histFulls;	
  friend struct histFulls;
  struct histFulls {
    TProfile* mHistCos;
    TH1F*     mHistRes;
    struct histFullHars histFullHar[Flow::nHars];
  };
  struct histFulls histFull[Flow::nSels]; //!

  static const Float_t etaMin;                  // histogram limits
  static const Float_t etaMax;
  static const Float_t ptMin;
  static const Float_t ptMax;
  static const Float_t qMax;

  TString      MakerName;

  ClassDef(StFlowAnalysisMaker, 1)              // macro for rootcint
};

inline Float_t StFlowAnalysisMaker::Res(Int_t eventN, Int_t harN) const 
  { return mRes[eventN][harN]; }

inline Float_t StFlowAnalysisMaker::ResErr(Int_t eventN, Int_t harN) const 
  { return mResErr[eventN][harN]; }

#endif

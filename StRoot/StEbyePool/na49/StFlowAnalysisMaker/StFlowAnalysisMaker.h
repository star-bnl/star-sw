///////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowAnalysisMaker.h,v 1.4 2002/11/15 22:18:03 posk Exp $
//
// Authors: Art Poskanzer, LBNL, and Alexander Wetzler, IKF, Dec 2000
//
///////////////////////////////////////////////////////////////////////////////
//
// Description:  Maker to analyze Flow using StFlowEvent
//
///////////////////////////////////////////////////////////////////////////////

#ifndef StFlowAnalysisMaker_H
#define StFlowAnalysisMaker_H
#include <iostream.h>
#include "StMaker.h"
#include "StFlowMaker/StFlowConstants.h"
#include "TVector2.h"
#include "TString.h"
class StFlowEvent;
class StFlowSelection;
class TH1F;
class TH1D;
class TH2F;
class TH2D;
class TH3F;
class TProfile;
class TProfile2D;

class StFlowAnalysisMaker : public StMaker {

public:

           StFlowAnalysisMaker(const Char_t* name="FlowAnalysis");
           StFlowAnalysisMaker(const Char_t* name,
			       const StFlowSelection& pFlowSelect);
  virtual  ~StFlowAnalysisMaker();

  Int_t    Init();
  Int_t    Make();
  Int_t    Finish();
  Float_t  Res(Int_t eventN, Int_t harN) const;
  Float_t  ResErr(Int_t eventN, Int_t harN) const;
  static   void SetV21();

private:

  bool     FillFromFlowEvent();
  void     FillEventHistograms();
  void     FillParticleHistograms();

  TVector2 mQ[Flow::nSels][Flow::nHars];                     //! flow vector
  Float_t  mPsi[Flow::nSels][Flow::nHars];                   //! event plane angle
  UInt_t   mMult[Flow::nSels][Flow::nHars];                  //! multiplicity
  Float_t  mSumPt2[Flow::nSels][Flow::nHars];                //! sum pt^2
  Float_t  m_q[Flow::nSels][Flow::nHars];                    //! Q/sqrt(Mult)
  TVector2 mQSub[Flow::nSels*Flow::nSubs][Flow::nHars];      //! flow vector subs
  Float_t  mPsiSub[Flow::nSels*Flow::nSubs][Flow::nHars];    //! plane angle subs
  UInt_t   mMultSub[Flow::nSels*Flow::nSubs][Flow::nHars];   //! multiplicity subs
  Float_t  mRes[Flow::nSels][Flow::nHars];      //! event plane resolution
  Float_t  mResErr[Flow::nSels][Flow::nHars];   //! event plane resolution error
  Float_t  mResK2[Flow::nSels];      //! event plane resolution for k=2
  Float_t  mResK2Err[Flow::nSels];   //! event plane resolution error for k=2
  static   Bool_t mV21;              // Flag for v2 relative to 1st har. plane
  StFlowEvent*     pFlowEvent;  //! pointer to StFlowEvent
  StFlowSelection* pFlowSelect; //! selection object

  // for single histograms
  TH1F*     mHistCharge;               //!
  TH1F*     mHistBx;                   //!
  TH1F*     mHistBy;                   //!
  TH1F*     mHistChi2;                 //!
  TH1F*     mHistFitPts;               //!
  TH1F*     mHistMaxPts;               //!
  TH1F*     mHistFitOverMax;           //!
  TH1F*     mHistOrigMult;             //!
  TH1F*     mHistMult;                 //!
  TH1F*     mHistMultOverOrig;         //!
  TH1F*     mHistMultPart;             //!
  TH1F*     mHistVertexZ;              //!
  TH2F*     mHistVertexXY2D;           //!
  TH1F*     mHistEtaSym;               //!
  TH3F*     mHistYPtPhi3D;             //!
  TH2D*     mHistYieldAll2D;           //!
  TH2D*     mHistYieldPart2D;          //!
  TProfile* mHistBinY;                 //!
  TProfile* mHistBinPt;                //!
  TProfile* mHistCosPhi;               //!
  TProfile* mHistPidMult;              //!
  TH1F*     mHistEVeto;                //!
  TH1F*     mHistCent;                 //!
  TH2F*     mHistEtaSymVerZ2D;         //!
  TH1F*     mHistEtaSymVerZ;           //!
  TH2F*     mHistMeanDedx2Dpos;        //!
  TH2F*     mHistMeanDedx2Dneg;        //!
  TH2F*     mHistMeanDedxPiPlus2D;     //!
  TH2F*     mHistMeanDedxPiMinus2D;    //!
  TH2F*     mHistMeanDedxProton2D;     //!
  TH2F*     mHistMeanDedxPbar2D;       //!
  TH2F*     mHistMeanDedxPositron2D;   //!
  TH2F*     mHistMeanDedxElectron2D;   //!

  
  // for each harmonic, each selection, and each sub-event
  struct histSubHars {
    TH1F*     mHistPsiSubs;
  };
  struct histSubs;	
  friend struct histSubs;
  struct histSubs {
    struct histSubHars histSubHar[Flow::nHars];
  };
  struct histSubs histSub[Flow::nSels*Flow::nSubs]; //!

  // for each harmonic and each selection
  struct histFullHars {
    TH1D*       mHistPhi;
    TH1D*       mHistPhiWgt;
    TH1D*       mHistPhiFlat;
    TH1F*       mHistPhiCorr;
    TH1F*       mHistPsiSubCorr;
    TH1F*       mHistPsiSubCorrDiff;
    TH1F*       mHistPsi;
    TH1F*       mHistMult;
    TH1F*       mHistSumPt2;
    TH1F*       mHistPtY;
    TH1F*       mHist_q;
    TH2D*       mHistYield2D;
    TProfile2D* mHist_vObs2D;
    TProfile*   mHist_vObsY;
    TProfile*   mHist_vObsPt;
    TH2D*       mHist_v2D;
    TH1D*       mHist_vY;
    TH1D*       mHist_vPt;
    TProfile2D* mHistMeanCos;
    TProfile2D* mHistMeanSin;
    TProfile2D* mHistMeanCosFlat;
    TProfile2D* mHistMeanSinFlat;
  };

  // for each selection
  struct histFulls;	
  friend struct histFulls;
  struct histFulls {
    TProfile* mHistCos;
    TH1F*     mHistRes;
    TProfile* mHist_vObs;
    TH1D*     mHist_v;
    struct histFullHars histFullHar[Flow::nHars];
  };
  struct histFulls histFull[Flow::nSels]; //!

  TString      MakerName;

  ClassDef(StFlowAnalysisMaker, 1)              // macro for rootcint
};

inline Float_t StFlowAnalysisMaker::Res(Int_t eventN, Int_t harN) const 
  { return mRes[eventN][harN]; }

inline Float_t StFlowAnalysisMaker::ResErr(Int_t eventN, Int_t harN) const 
  { return mResErr[eventN][harN]; }

inline void StFlowAnalysisMaker::SetV21() { mV21 = kTRUE; }

#endif

///////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowAnalysisMaker.h,v $
// Revision 1.4  2002/11/15 22:18:03  posk
// updates.
//
// Revision 1.3  2001/11/06 17:55:29  posk
// Only sin terms at 40 GeV.
//
// Revision 1.2  2001/08/17 22:03:32  posk
// Now can also do 40 GeV data.
//
// Revision 1.1  2001/02/22 23:34:25  posk
// NA49 version of STAR flow software.
//
// Revision 1.25  2000/09/29 22:53:16  posk
//
///////////////////////////////////////////////////////////////////////////////

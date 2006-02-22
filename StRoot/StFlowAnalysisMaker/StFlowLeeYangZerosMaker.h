///////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowLeeYangZerosMaker.h,v 1.1 2006/02/22 19:09:30 posk Exp $
//
// Authors: Markus Oldenberg and Art Poskanzer, LBNL
//
///////////////////////////////////////////////////////////////////////////////
//
// Description:  Maker to analyze Flow by the LeeYangZeros method
//
///////////////////////////////////////////////////////////////////////////////

#ifndef StFlowLeeYangZerosMaker_H
#define StFlowLeeYangZerosMaker_H
#include <Stiostream.h>
#include "StMaker.h"
#include "StFlowMaker/StFlowConstants.h"
#include "TVector2.h"
#include "TString.h"
#include "StTimer.hh"
#include "TComplex.h"
class StFlowEvent;
class StFlowSelection;
class TH1F;
class TH1D;
class TProfile;
class TProfile2D;

class StFlowLeeYangZerosMaker : public StMaker {

  /*!
    \class StFlowLeeYangZerosMaker
    Makes histograms for the LYZ flow analysis. It reads event and particle quantities
    from StFlowEvent. For each harmonic and each selection makes histograms
    of the anisotropic flow, v, vs. y and p_t. Selection=1 uses the Sum Generating Function,
    Selection=2 uses the Product Generating Function.
  */
  
public:

  /// Default constructor
  StFlowLeeYangZerosMaker(const Char_t* name="FlowLeeYangZeros");

  /// Constructor with selection object
  StFlowLeeYangZerosMaker(const Char_t* name,
			  const StFlowSelection& pFlowSelect);
  StFlowLeeYangZerosMaker(const StFlowLeeYangZerosMaker &from) {};

  /// Destructor
  virtual  ~StFlowLeeYangZerosMaker();

  Int_t    Init();
  Int_t    Make();
  Int_t    Finish();
  void     SetHistoRanges(Bool_t ftpc_included = kFALSE);
  void     SetPtRange_for_vEta(Float_t lo, Float_t hi);
  void     SetEtaRange_for_vPt(Float_t lo, Float_t hi);
  virtual  const char *GetCVS() const {static const char cvs[]=
    "Tag $Name:  $  StFlowLeeYangZerosMaker.h, "__DATE__" "__TIME__ ;
    return cvs;}

private:

  Bool_t   FillFromFlowEvent();
  void     FillParticleHistograms();
  Bool_t   mFirstPass;
#ifndef __CINT__
  TVector2 mQ[Flow::nSels][Flow::nHars];                     //! flow vector
  Double_t mQ2[Flow::nSels][Flow::nHars];                    //! flow vector modulus square
  Int_t    mNEvents[Flow::nSels][Flow::nHars];               //! number of events
  Int_t    mPtBinsPart;                                      //! pt bins
  Float_t  mQtheta[Flow::nSels][Flow::nHars][Flow::nTheta];  //! Q^{\theta}
  Float_t  mr0theta[Flow::nSels][Flow::nHars][Flow::nTheta]; //! r_0^{\theta} from first pass
  TComplex mGr0theta[Flow::nSels][Flow::nHars][Flow::nTheta];//! G(r_0)^{\theta}
#endif /*__CINT__*/
  Int_t            mMult;                                    //! multiplicity
  TString          xLabel;      //! label axis with rapidity or pseudorapidity 
  StFlowEvent*     pFlowEvent;  //! pointer to StFlowEvent
  StFlowSelection* pFlowSelect; //! selection object

  // for single histograms
  TH1D*     mHistYieldPartPt;          //!
  TH1D*     mHistYieldPartEta;         //!
  TH1F*     mHistMult;                 //!

  // for each harmonic, each selection, and each theta
  struct histThetas {
    TH1F*       mHistGtheta;
    TProfile*   mHistReGtheta;
    TProfile*   mHistImGtheta;    
    TProfile2D* mHistReNumer2D;
    TProfile*   mHistReNumerEta;
    TProfile2D* mHistImNumer2D;
    TProfile*   mHistImNumerEta;
  };

  // for each harmonic and each selection
  struct histFullHars {
    TProfile*   mHistPro_vEta;
    TProfile*   mHistPro_vPt;
    TProfile*   mHistPro_Vtheta;
    TProfile*   mHistPro_r0theta;
    TH1D*       mHist_vEta;
    TH1D*       mHist_vPt;
    TH1D*       mHist_Vtheta;
    TH1D*       mHist_r0theta;
    TProfile*   mHistReDenom;
    TProfile*   mHistImDenom;
    struct histThetas histTheta[Flow::nTheta]; 
  };

  // for each selection
  struct histFulls;	
  friend struct histFulls;
  struct histFulls {
    TProfile*   mHistPro_V;
    TProfile*   mHistPro_vr0;
    TH1D*       mHist_V;
    TH1D*       mHist_vr0;
    TH1F*       mHist_v;
    struct histFullHars histFullHar[Flow::nHars];
  };
  struct histFulls histFull[Flow::nSels]; //!

  TString      MakerName;

  Float_t mEtaMin;
  Float_t mEtaMax;
    Int_t mNEtaBins;

  Float_t mPtRange_for_vEta[2];
  Float_t mEtaRange_for_vPt[2];

/*   StTimer timeInit; */
/*   StTimer timeEvent; */
/*   StTimer timePart; */
/*   StTimer timeFinish; */

  ClassDef(StFlowLeeYangZerosMaker,0)              // macro for rootcint
};

#endif

///////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCorrelationMaker.h,v 1.2 2003/09/02 17:57:58 perev Exp $
//
// Authors: Art Poskanzer and Raimond Snellings
//
///////////////////////////////////////////////////////////////////////////////
//
// Description:  Maker to analyze Azimuthal Correaltions using StFlowEvent
//
///////////////////////////////////////////////////////////////////////////////

#ifndef StFlowCorrelationMaker_H
#define StFlowCorrelationMaker_H
#include <Stiostream.h>
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

class StFlowCorrelationMaker : public StMaker {

public:

           StFlowCorrelationMaker(const Char_t* name="FlowAnalysis");
           StFlowCorrelationMaker(const Char_t* name,
			       const StFlowSelection& pFlowSelect);
  virtual  ~StFlowCorrelationMaker();

  Int_t    Init();
  Int_t    Make();
  Int_t    Finish();
  virtual  const char *GetCVS() const {static const char cvs[]=
    "Tag $Name:  $ $Id: StFlowCorrelationMaker.h,v 1.2 2003/09/02 17:57:58 perev Exp $ built "__DATE__" "__TIME__ ;
    return cvs;}

private:

  void     FillFromFlowEvent();
  void     FillEventHistograms();
  void     FillParticleHistograms();

  TVector2 mQ[Flow::nSels][Flow::nHars];                     //! flow vector
  Float_t  mPsi[Flow::nSels][Flow::nHars];                   //! event plane angle
  UInt_t   mMult[Flow::nSels][Flow::nHars];                  //! multiplicity
  Float_t  m_q[Flow::nSels][Flow::nHars];                    //! Q/::sqrt(Mult)
  TVector2 mQSub[Flow::nSels*Flow::nSubs][Flow::nHars];      //! flow vector subs
  Float_t  mPsiSub[Flow::nSels*Flow::nSubs][Flow::nHars];    //! plane angle subs
  UInt_t   mMultSub[Flow::nSels*Flow::nSubs][Flow::nHars];   //! multiplicity subs
  TString          xLabel;      //! label axis with rapidity or pseudorapidity 
  StFlowEvent*     pFlowEvent;  //! pointer to StFlowEvent
  StFlowSelection* pFlowSelect; //! selection object

  // for single histograms
  TH1F*     mHistCharge;               //!
  TH1F*     mHistDcaTpc;               //!
  TH1F*     mHistDcaGlobalTpc;         //!
  TH1F*     mHistChi2Tpc;              //!
  TH1F*     mHistFitPtsTpc;            //!
  TH1F*     mHistMaxPtsTpc;            //!
  TH1F*     mHistFitOverMaxTpc;        //!
  TH1F*     mHistOrigMult;             //!
  TH1F*     mHistMultEta;              //!
  TH1F*     mHistMult;                 //!
  TH1F*     mHistMultOverOrig;         //!
  TH1F*     mHistMultPart;             //!
  TH1F*     mHistVertexZ;              //!
  TH2F*     mHistVertexXY2D;           //!
  TH1F*     mHistEtaSym;               //!
  TH2D*     mHistYieldAll2D;           //!
  TH2D*     mHistYieldPart2D;          //!
  TH1F*     mHistCent;                 //!
  TH2F*     mHistEtaSymVerZ2D;         //!
  TH1F*     mHistEtaSymVerZ;           //!
  TH2F*     mHistCTBvsZDC2D;           //!

  TH2F*     mHistMeanDedx2D;           //!
  TH2F*     mHistMeanDedxPiPlus2D;     //!
  TH2F*     mHistMeanDedxPiMinus2D;    //!
  TH2F*     mHistMeanDedxProton2D;     //!
  TH2F*     mHistMeanDedxPbar2D;       //!
  TH2F*     mHistMeanDedxKplus2D;      //!
  TH2F*     mHistMeanDedxKminus2D;     //!
  TH2F*     mHistMeanDedxDeuteron2D;   //!
  TH2F*     mHistMeanDedxAntiDeuteron2D;//!
  TH2F*     mHistMeanDedxPositron2D;   //!
  TH2F*     mHistMeanDedxElectron2D;   //!

  TH1F*     mHistPidPiPlus;            //!
  TH1F*     mHistPidPiMinus;           //!
  TH1F*     mHistPidProton;            //!
  TH1F*     mHistPidAntiProton;        //!
  TH1F*     mHistPidKplus;             //!
  TH1F*     mHistPidKminus;            //!
  TH1F*     mHistPidDeuteron;          //!
  TH1F*     mHistPidAntiDeuteron;      //!
  TH1F*     mHistPidElectron;          //!
  TH1F*     mHistPidPositron;          //!
  TH1F*     mHistPidPiPlusPart;        //!
  TH1F*     mHistPidPiMinusPart;       //!
  TH1F*     mHistPidProtonPart;        //!
  TH1F*     mHistPidAntiProtonPart;    //!
  TH1F*     mHistPidKplusPart;         //!
  TH1F*     mHistPidKminusPart;        //!
  TH1F*     mHistPidDeuteronPart;      //!
  TH1F*     mHistPidAntiDeuteronPart;  //!
  TH1F*     mHistPidElectronPart;      //!
  TH1F*     mHistPidPositronPart;      //!
  TProfile* mHistPidMult;              //!

  TH1F*     mHistDeltaPhi;             //!
  
  TString      MakerName;

  ClassDef(StFlowCorrelationMaker, 1)              // macro for rootcint
};

#endif

///////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowCorrelationMaker.h,v $
// Revision 1.2  2003/09/02 17:57:58  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.1  2001/01/31 19:47:28  snelling
// A simple correlation program so far only used for simulations (no mixing)
//
///////////////////////////////////////////////////////////////////////////////

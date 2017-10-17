///////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowScalarProdMaker.h,v 1.7 2014/08/06 11:43:14 jeromel Exp $
//
// Authors: method proposed by Art and Sergei, code written by Aihong
//          Frame adopted from Art and Raimond's StFlowAnalysisMaker.
///////////////////////////////////////////////////////////////////////////////
//
// Description:  Maker to analyze Flow using the scalar product method
//
///////////////////////////////////////////////////////////////////////////////

#ifndef StFlowScalarProdMaker_H
#define StFlowScalarProdMaker_H
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
//class TH3F;
class TProfile;
class TProfile2D;

class StFlowScalarProdMaker : public StMaker {

public:

           StFlowScalarProdMaker(const Char_t* name="FlowScalarProdAnalysis");
           StFlowScalarProdMaker(const Char_t* name,
			       const StFlowSelection& pFlowSelect);
           StFlowScalarProdMaker(const StFlowScalarProdMaker &from){};
  virtual  ~StFlowScalarProdMaker();

  Int_t    Init();
  Int_t    Make();
  Int_t    Finish();
  void     SetHistoRanges(Bool_t ftpc_included = kFALSE);
  virtual  const char *GetCVS() const {static const char cvs[]=
    "Tag $Name:  $ $Id: StFlowScalarProdMaker.h,v 1.7 2014/08/06 11:43:14 jeromel Exp $ built " __DATE__ " " __TIME__ ;
    return cvs;}

private:

  void     FillFromFlowEvent();
  void     FillEventHistograms();
  void     FillParticleHistograms();
#ifndef __CINT__
  TVector2 mQ[Flow::nSels][Flow::nHars];                     //! flow vector
  TVector2 mQSub[Flow::nSels*Flow::nSubs][Flow::nHars];      //! flow vector subs

  Float_t  mRes[Flow::nSels][Flow::nHars];      //! event plane resolution 
  Float_t  mResErr[Flow::nSels][Flow::nHars];   //! event plane resolution er
#endif /*__CINT__*/
  TString          xLabel;      //! label axis with rapidity or pseudorapidity 
  StFlowEvent*     pFlowEvent;  //! pointer to StFlowEvent
  StFlowSelection* pFlowSelect; //! selection object

  // for each harmonic and each selection
  struct histFullHars {
    TProfile2D* mHist_vObs2D;
    TProfile*   mHist_vObsEta;
    TProfile*   mHist_vObsPt;
    TH2D*       mHist_v2D;
    TH1D*       mHist_vEta;
    TH1D*       mHist_vPt;
  };

  // for each selection
  struct histFulls;	
  friend struct histFulls;
  struct histFulls {
    TProfile* mHistRes;
    TProfile* mHist_vObs;
    TH1D*     mHist_v;
    struct histFullHars histFullHar[Flow::nHars];
  };
  struct histFulls histFull[Flow::nSels]; //!

  TString      MakerName;

  Float_t mEtaMin;
  Float_t mEtaMax;
    Int_t mNEtaBins;

  ClassDef(StFlowScalarProdMaker,0)              // macro for rootcint
};

#endif

///////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowScalarProdMaker.h,v $
// Revision 1.7  2014/08/06 11:43:14  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.6  2003/09/10 19:47:15  perev
// ansi corrs
//
// Revision 1.5  2003/09/02 17:58:11  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.4  2003/01/10 16:40:52  oldi
// Several changes to comply with FTPC tracks:
// - Switch to include/exclude FTPC tracks introduced.
//   The same switch changes the range of the eta histograms.
// - Eta symmetry plots for FTPC tracks added and separated from TPC plots.
// - PhiWgts and related histograms for FTPC tracks split in FarEast, East,
//   West, FarWest (depending on vertex.z()).
// - Psi_Diff plots for 2 different selections and the first 2 harmonics added.
// - Cut to exclude mu-events with no primary vertex introduced.
//   (This is possible for UPC events and FTPC tracks.)
// - Global DCA cut for FTPC tracks added.
// - Global DCA cuts for event plane selection separated for TPC and FTPC tracks.
// - Charge cut for FTPC tracks added.
//
// Revision 1.3  2002/01/14 23:42:55  posk
// Renamed ScalerProd histograms. Moved print commands to FlowMaker::Finish().
//
// Revision 1.2  2001/12/21 17:01:59  aihong
// minor changes
//
// Revision 1.1  2001/12/18 23:46:47  aihong
// install scalar product method
//
//
///////////////////////////////////////////////////////////////////////////////

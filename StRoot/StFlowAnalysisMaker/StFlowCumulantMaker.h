///////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCumulantMaker.h,v 1.8 2014/08/06 11:43:14 jeromel Exp $
//
// Authors:  Aihong Tang, Kent State U. Oct 2001
//           Frame adopted from Art and Raimond's StFlowAnalysisMaker.
//
///////////////////////////////////////////////////////////////////////////////
//
// Description:  Maker to analyze Flow using the new cumulant method.
//                refer to Phy. Rev. C63 (2001) 054906 (old new method)
//                and      Phy. Rev. C64 (2001) 054901 (new new method)
//                and      nucl-ex/0110016             (Practical Guide)
////
//               Anything that has "Mix" in it is for v1{3} calculation PRC 66 014905 (2002)
//
///////////////////////////////////////////////////////////////////////////////
//
// Abreviations used:
//    Cumul for Cumulant
//    Diff  for Diffeential
//    Integ for Integrated
//    Denom for Denominator
//    Mix   for mixed harmonics (for v1{3})
//
///////////////////////////////////////////////////////////////////////////////

#ifndef StFlowCumulantMaker_H
#define StFlowCumulantMaker_H
#include <Stiostream.h>
#include "StMaker.h"
#include "StFlowMaker/StFlowConstants.h"
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

class StFlowCumulantMaker : public StMaker {

public:

           StFlowCumulantMaker(const Char_t* name="FlowCumulantAnalysis");
           StFlowCumulantMaker(const Char_t* name,
			       const StFlowSelection& pFlowSelect);
           StFlowCumulantMaker(const StFlowCumulantMaker &from){};
  virtual  ~StFlowCumulantMaker();

  Int_t    Init();
  Int_t    Make();
  Int_t    Finish();
  void     SetHistoRanges(Bool_t ftpc_included = kFALSE);

  virtual  const char *GetCVS() const {static const char cvs[]=
    "Tag $Name:  $ $Id: StFlowCumulantMaker.h,v 1.8 2014/08/06 11:43:14 jeromel Exp $ built " __DATE__ " " __TIME__ ;
    return cvs;}

private:

  void     FillFromFlowEvent();
  void     FillEventHistograms();
  void     FillParticleHistograms();

#ifndef __CINT__
  UInt_t   mMult[Flow::nSels][Flow::nHars];                  //! multiplicity
  UInt_t   mMultSub[Flow::nSels*Flow::nSubs][Flow::nHars];   //! multiplicity subs
  Double_t mSqrtOfSumWgtSqr[Flow::nSels][Flow::nHars]; 

#endif /*__CINT__*/
  TString          xLabel;      //! label axis with rapidity or pseudorapidity 
  StFlowEvent*     pFlowEvent;  //! pointer to StFlowEvent
  StFlowSelection* pFlowSelect; //! selection object

  Double_t  profScale;          //! profile scale
  Double_t  r0;                 //! r0 in the cumulant paper.
  Double_t  r0Sq;               //! square of r0.
  Double_t  r0Mix;              //! r0 for v1{3} calculation   
  UInt_t    m_M;                //! m in the cumulant paper.


  // for each harmonic and each selection
  struct histFullHars {

    TProfile2D**   mHistCumul2D;
    TProfile**     mHistCumulEta;
    TProfile**     mHistCumulPt;

    TProfile2D*    mHistCumulMix2D;
    TProfile*      mHistCumulMixEta;
    TProfile*      mHistCumulMixPt;


    TH2D**         mHist_v2D;
    TH1D**         mHist_vEta;
    TH1D**         mHist_vPt;

    TH2D*         mHistMix_v2D;
    TH1D*         mHistMix_vEta;
    TH1D*         mHistMix_vPt;


    TProfile**     mCumulG0Denom; //denominator in (82), <G>
    TProfile**     mCumulG0MixDenom;

    Double_t       mIntegXz[Flow::nCumulIntegOrders*Flow::nCumulInteg_qMax];
    Double_t       mIntegYz[Flow::nCumulIntegOrders*Flow::nCumulInteg_qMax];
    Double_t       mDiffXz[Flow::nCumulDiffOrders*Flow::nCumulDiff_qMax];
    Double_t       mDiffYz[Flow::nCumulDiffOrders*Flow::nCumulDiff_qMax];
    Double_t       mCumulIntegG0[Flow::nCumulIntegOrders*Flow::nCumulInteg_qMax];    
    Double_t       mCumulG0DenomRead[Flow::nCumulDiffOrders*Flow::nCumulDiff_qMax];

    Double_t       mMixX1z[Flow::nCumulMixHar_pMax];
    Double_t       mMixY1z[Flow::nCumulMixHar_pMax];
    Double_t       mMixX2z[Flow::nCumulMixHar_qMax];
    Double_t       mMixY2z[Flow::nCumulMixHar_qMax];

    Double_t       mCumulIntegG0Mix[Flow::nCumulMixHar_pMax*Flow::nCumulMixHar_qMax];  
    Double_t       mCumulG0MixDenomRead[Flow::nCumulMixHar_pMax*Flow::nCumulMixHar_qMax];



    Double_t       mMultSum;
    Int_t          mNEvent; 
    Double_t       mMeanWgtSqrSum;

    TH1D*          mHistMultSum;    //histo for holding mMultSum.
    TH1D*          mHistNEvent;
    TH1D*          mHistMeanWgtSqrSum; // sum <w^2> over evts.

    TH1D**         mHistCumulIntegG0Sum; //summation of G value.
    TH1D**         mHistCumulIntegG0MixSum; //summation of G value.

  };

  // for each selection
  struct histFulls;	
  friend struct histFulls;

  struct histFulls {
   TProfile**  mHistCumul; //integrated cumulant from differential
   TProfile*   mHistCumulMix; //cumulant for mix 1st and 2nd hars.
 
   TH1D**      mHist_v; //integrated flow from differential
   TH1D*       mHistMix_v; //integrated flow from differential


    struct histFullHars histFullHar[Flow::nHars];
  };
  struct histFulls histFull[Flow::nSels]; //!

  TString      MakerName;
  
  Float_t mEtaMin;
  Float_t mEtaMax;
    Int_t mNEtaBins;
    Int_t nPtBinsPart;

  ClassDef(StFlowCumulantMaker,0)              // macro for rootcint
    
};
    

#endif

////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowCumulantMaker.h,v $
// Revision 1.8  2014/08/06 11:43:14  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.7  2004/12/17 15:50:09  aihong
// check in v1{3} code
//
// Revision 1.6  2004/11/16 21:22:22  aihong
// removed old cumulant method
//
// Revision 1.5  2003/09/10 19:47:14  perev
// ansi corrs
//
// Revision 1.4  2003/09/02 17:58:11  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.3  2003/03/03 16:24:37  aihong
// blow up 4-part cumulant by 1000 in order to let error bars calculated by ROOT
//
// Revision 1.2  2003/01/10 16:40:47  oldi
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
// Revision 1.1  2002/05/19 18:58:00  aihong
// speed up cumulant
//
// Revision 1.8  2002/02/02 01:10:22  posk
// Added documentation
//
// Revision 1.7  2002/01/24 20:53:51  aihong
// add histograms for combining results from parallel jobs
//
// Revision 1.6  2002/01/14 23:42:48  posk
// Renamed ScalerProd histograms. Moved print commands to FlowMaker::Finish().
//
// Revision 1.5  2001/12/21 17:01:59  aihong
// minor changes
//
// Revision 1.4  2001/12/11 22:04:10  posk
// Four sets of phiWgt histograms.
// StFlowMaker StFlowEvent::PhiWeight() changes.
// Cumulant histogram names changed.
//
// Revision 1.3  2001/11/09 21:14:56  posk
// Switched from CERNLIB to TMath. Using global dca instead of dca.
//
// Revision 1.2  2001/11/08 03:12:24  aihong
// clean up redundant histograms
//
// Revision 1.1  2001/11/02 04:47:31  aihong
// install cumulant maker
//
////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCumulantMaker.h,v 1.4 2003/09/02 17:58:11 perev Exp $
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
//
///////////////////////////////////////////////////////////////////////////////
//
// Abreviations used:
//    Cumul for Cumulant
//    Diff  for Diffeential
//    Integ for Integrated
//    Denom for Denominator
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
  void     SetOldMethod(Bool_t flag=kFALSE);
  void     SetHistoRanges(Bool_t ftpc_included = kFALSE);

  virtual  const char *GetCVS() const {static const char cvs[]=
    "Tag $Name:  $ $Id: StFlowCumulantMaker.h,v 1.4 2003/09/02 17:58:11 perev Exp $ built "__DATE__" "__TIME__ ;
    return cvs;}

private:

  void     FillFromFlowEvent();
  void     FillEventHistograms();
  void     FillParticleHistograms();

#ifndef __CINT__
  UInt_t   mMult[Flow::nSels][Flow::nHars];                  //! multiplicity
  UInt_t   mMultSub[Flow::nSels*Flow::nSubs][Flow::nHars];   //! multiplicity subs
  Double_t mWgtMult_q4[Flow::nSels][Flow::nHars];            //for getting q4 with wgt in old cumulant method
  Double_t mWgtMult_q6[Flow::nSels][Flow::nHars];            //for getting q6 with wgt in old cumulant method
#endif /*__CINT__*/
  TString          xLabel;      //! label axis with rapidity or pseudorapidity 
  StFlowEvent*     pFlowEvent;  //! pointer to StFlowEvent
  StFlowSelection* pFlowSelect; //! selection object

  Double_t  profScale;          //! profile scale
  Double_t  r0;                 //! r0 in the cumulant paper.
  Double_t  r0Sq;               //! square of r0.
  UInt_t    m_M;                //! m in the cumulant paper.
  Bool_t    mOldMethod;         //! tag for old new / new new method


  // for each harmonic and each selection
  struct histFullHars {

    TProfile2D**   mHistCumul2D;
    TProfile**     mHistCumulEta;
    TProfile**     mHistCumulPt;

    TH2D**         mHist_v2D;
    TH1D**         mHist_vEta;
    TH1D**         mHist_vPt;

    TProfile2D**   mCumulG0Denom2D; 
    TProfile**     mCumulG0DenomEta;
    TProfile**     mCumulG0DenomPt;

    TProfile**     mCumulG0Denom; //denominator in (82), <G>

    Double_t       mIntegXz[Flow::nCumulIntegOrders*Flow::nCumulInteg_qMax];
    Double_t       mIntegYz[Flow::nCumulIntegOrders*Flow::nCumulInteg_qMax];
    Double_t       mDiffXz[Flow::nCumulDiffOrders*Flow::nCumulDiff_qMax];
    Double_t       mDiffYz[Flow::nCumulDiffOrders*Flow::nCumulDiff_qMax];
    Double_t       mCumulIntegG0[Flow::nCumulIntegOrders*Flow::nCumulInteg_qMax];    
    Double_t       mCumulG0DenomRead[Flow::nCumulDiffOrders*Flow::nCumulDiff_qMax];

    Double_t       mMultSum;
    Double_t       mWgtMultSum_q4; //for getting q4 right in the old method
    Double_t       mWgtMultSum_q6; //for getting q6 right in the old method
    Int_t          mNEvent; 

    TH1D*          mHistMultSum;    //histo for holding mMultSum.
    TH1D*          mHistWgtMultSum_q4; 
    TH1D*          mHistWgtMultSum_q6;
    TH1D*          mHistNEvent;

    TH1D**         mHistCumulIntegG0Sum; //summation of G value.

  };

  // for each selection
  struct histFulls;	
  friend struct histFulls;

  struct histFulls {
   TProfile**  mHistCumul; //integrated cumulant from differential
   TH1D**      mHist_v; //integrated flow from differential

    struct histFullHars histFullHar[Flow::nHars];
  };
  struct histFulls histFull[Flow::nSels]; //!

  TString      MakerName;
  
  Float_t mEtaMin;
  Float_t mEtaMax;
    Int_t mNEtaBins;

  ClassDef(StFlowCumulantMaker, 1)              // macro for rootcint
    
};
    
inline void StFlowCumulantMaker::SetOldMethod(Bool_t flag)
{ mOldMethod=flag; }

#endif

////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowCumulantMaker.h,v $
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

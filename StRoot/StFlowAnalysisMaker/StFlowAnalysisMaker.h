///////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowAnalysisMaker.h,v 1.52 2014/08/06 11:43:14 jeromel Exp $
//
// Authors: Art Poskanzer and Raimond Snellings, LBNL, Aug 1999
//          FTPC added by Markus Oldenburg, MPI, Dec 2000
//
///////////////////////////////////////////////////////////////////////////////
//
// Description:  Maker to analyze Flow using StFlowEvent
//
///////////////////////////////////////////////////////////////////////////////

#ifndef StFlowAnalysisMaker_H
#define StFlowAnalysisMaker_H
#include <Stiostream.h>
#include "StMaker.h"
#include "StFlowMaker/StFlowConstants.h"
#include "TVector2.h"
#include "TString.h"
class StFlowMaker;
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

  /*!
    \class StFlowAnalysisMaker
    Makes histograms for the flow analysis. It reads event and particle quantities
    from StFlowEvent. It removes autocorrelations of each particle with respect
    to the event plane. For each harmonic and each selection makes a 2D histogram
    of the anisotropic flow v vs. y and p_t.
  */
  
public:

  /// Default constructor
           StFlowAnalysisMaker(const Char_t* name="FlowAnalysis");
  /// Constructor with selection object
          StFlowAnalysisMaker(const Char_t* name,
			       const StFlowSelection& pFlowSelect);
           StFlowAnalysisMaker(const StFlowAnalysisMaker &from) {};
  virtual  ~StFlowAnalysisMaker();

  Int_t    Init();
  Int_t    Make();
  Int_t    Finish();
  void     SetHistoRanges(Bool_t ftpc_included = kFALSE);
  void     SetPtRange_for_vEta(Float_t lo, Float_t hi);
  void     SetEtaRange_for_vPt(Float_t lo, Float_t hi);
  void     SetV1Ep1Ep2(Bool_t v1Ep1Ep2 = kTRUE);
  virtual  const char *GetCVS() const {static const char cvs[]=
    "Tag $Name:  $ $Id: StFlowAnalysisMaker.h,v 1.52 2014/08/06 11:43:14 jeromel Exp $ built " __DATE__ " " __TIME__ ;
    return cvs;}

private:

  Bool_t   FillFromFlowEvent();
  void     FillEventHistograms();
  void     FillParticleHistograms();
  Bool_t   mCalcReCentPars;
#ifndef __CINT__
  TVector2 mQ[Flow::nSels][Flow::nHars];                     //! flow vector
  Float_t  mPsi[Flow::nSels][Flow::nHars];                   //! event plane angle
  UInt_t   mMult[Flow::nSels][Flow::nHars];                  //! multiplicity
  Float_t  m_q[Flow::nSels][Flow::nHars];                    //! Q/::sqrt(Mult)
  TVector2 mQSub[Flow::nSels*Flow::nSubs][Flow::nHars];      //! flow vector subs
  Float_t  mPsiSub[Flow::nSels*Flow::nSubs][Flow::nHars];    //! plane angle subs
  UInt_t   mMultSub[Flow::nSels*Flow::nSubs][Flow::nHars];   //! multiplicity subs
  Float_t  mRes[Flow::nSels][Flow::nHars];      //! event plane resolution
  Float_t  mResErr[Flow::nSels][Flow::nHars];   //! event plane resolution error
  Float_t  mZDCSMD_e_PsiWgt,mZDCSMD_w_PsiWgt,mZDCSMD_f_PsiWgt;   //! ZDCSMD Psi Weight
  Float_t  mFlowWeight;				//! Weight for flow
#endif /*__CINT__*/
  TString          xLabel;      //! label axis with rapidity or pseudorapidity
  StFlowMaker*     pFlowMaker;  //! pointer to StFlowMaker 
  StFlowEvent*     pFlowEvent;  //! pointer to StFlowEvent
  StFlowSelection* pFlowSelect; //! selection object

  // for single histograms
  TH1F*     mHistTrigger;              //!
  TH1F*     mHistChargeFtpc;           //!
  TH1F*     mHistDcaGlobalTpc;         //!
  TH1F*     mHistDcaGlobalFtpc;        //!
  TH1F*     mHistDcaTpc;               //!
  TH1F*     mHistDcaFtpc;              //!
  TH1F*     mHistChi2Tpc;              //!
  TH1F*     mHistChi2Ftpc;             //!
  TH1F*     mHistFitPtsTpc;            //!
  TH1F*     mHistMaxPtsTpc;            //!
  TH1F*     mHistFitOverMaxTpc;        //!
  TH1F*     mHistFitPtsFtpc;           //!
  TH1F*     mHistMaxPtsFtpc;           //!
  TH1F*     mHistFitOverMaxFtpc;       //!
  TH1F*     mHistOrigMult;             //!
  TH1F*     mHistMultEta;              //!
  TH1F*     mHistMult;                 //!
  TH1F*     mHistMultOverOrig;         //!
  TH1F*     mHistMultPart;             //!
  TH1F*     mHistVertexZ;              //!
  TH2F*     mHistVertexXY2D;           //!
  TH1F*     mHistEtaSymTpc;            //!
  TH1F*     mHistEtaSymFtpc;           //!
  TH3F*     mHistEtaPtPhi3D;           //!
  TH2D*     mHistYieldAll2D;           //!
  TH2D*     mHistYieldPart2D;          //!
  TProfile* mHistBinEta;               //!
  TProfile* mHistBinPt;                //!
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
  TH1F*     mHistCent;                 //!
  TH2F*     mHistEtaSymVerZ2DTpc;      //!
  TH2F*     mHistEtaSymVerZ2DFtpc;     //!
  TH2F*     mHistCTBvsZDC2D;           //!
  TH2F*     mHistMeanDedxPos2D;        //!
  TH2F*     mHistMeanDedxNeg2D;        //!
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
  TH1F*	    mZDC_SMD_west_vert;	       //!
  TH1F*     mZDC_SMD_west_hori;	       //!
  TH1F*     mZDC_SMD_east_vert;        //!
  TH1F*     mZDC_SMD_east_hori;        //!
  TH1D*     mHistZDCSMDPsiWgtEast;     //!
  TH1D*     mHistZDCSMDPsiWgtWest;     //!
  TH1D*     mHistZDCSMDPsiWgtTest;     //!
  TH1D*     mHistZDCSMDPsiWgtFull;     //!
  TH1D*     mHistZDCSMDPsiCorTest;     //!
  TH1D*     mHistZDCSMDPsiCorFull;     //!
  
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
    TH1F*       mHistPhiCorr;
    TH1F*       mHistPhiLab;
    TH1F*       mHistPsiSubCorr;
    TH1F*       mHistPsiSubCorrDiff;
    TH1F*       mHistPsi;
    TProfile*   mHistReCentX;
    TProfile*   mHistReCentY;
    TProfile*   mHistQreCent;
    TH2D*       mHistQXY2D;
    TH2D*       mHistQFTPCSubXY2D;
    TH2D*       mHistQTPCSubXY2D;
    TH1F*       mHistPsi_Diff;
    TH1F*       mHistMult;
    TH1F*       mHist_q;
    TH2D*       mHistYield2D;
    TProfile2D* mHist_vObs2D;
    TProfile*   mHist_vObsEta;
    TProfile*   mHist_vObsPt;
    TH2D*       mHist_v2D;
    TH1D*       mHist_vEta;
    TH1D*       mHist_vPt;
  };

  // for two harmonics and each selection
  struct histTwoHars {
    TH1D*       mHistPhiFarEast;
    TH1D*       mHistPhiEast;
    TH1D*       mHistPhiWest;
    TH1D*       mHistPhiFarWest;
    TH1D*       mHistPhiFtpcFarEast;
    TH1D*       mHistPhiFtpcEast;
    TH1D*       mHistPhiFtpcWest;
    TH1D*       mHistPhiFtpcFarWest;
    TH1D*       mHistPhiWgtFarEast;
    TH1D*       mHistPhiWgtEast;
    TH1D*       mHistPhiWgtWest;
    TH1D*       mHistPhiWgtFarWest;
    TH1D*       mHistPhiWgtFtpcFarEast;
    TH1D*       mHistPhiWgtFtpcEast;
    TH1D*       mHistPhiWgtFtpcWest;
    TH1D*       mHistPhiWgtFtpcFarWest;
    TH1D*       mHistPhiFlatFarEast;
    TH1D*       mHistPhiFlatEast;
    TH1D*       mHistPhiFlatWest;
    TH1D*       mHistPhiFlatFarWest;
    TH1D*       mHistPhiFlatFtpcFarEast;
    TH1D*       mHistPhiFlatFtpcEast;
    TH1D*       mHistPhiFlatFtpcWest;
    TH1D*       mHistPhiFlatFtpcFarWest;
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
    struct histTwoHars histTwoHar[2];
  };
  struct histFulls histFull[Flow::nSels]; //!

  TString      MakerName;

  Float_t mEtaMin;
  Float_t mEtaMax;
    Int_t mNEtaBins;

  Float_t mPtRange_for_vEta[2];
  Float_t mEtaRange_for_vPt[2];

  Bool_t  mV1Ep1Ep2;

  ClassDef(StFlowAnalysisMaker,0)              // macro for rootcint
};
#endif

///////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowAnalysisMaker.h,v $
// Revision 1.52  2014/08/06 11:43:14  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.51  2011/03/10 18:56:25  posk
// Added histogram for laboratory azimuthal distribution of particles.
//
// Revision 1.50  2010/09/30 19:28:12  posk
// Instead of reversing the weight for negative pseudrapidity for odd harmonics,
// it is now done only for the first harmonic.
// Recentering is now done for all harmonics.
//
// Revision 1.49  2009/11/24 19:29:12  posk
// Added reCenter to remove acceptance correlations as an option instead of phiWgt.
//
// Revision 1.48  2006/02/22 19:36:27  posk
// Minor updates.
//
// Revision 1.47  2004/12/17 22:33:33  aihong
// add in full Psi weight for ZDC SMD and fix a few bugs, done by Gang
//
// Revision 1.46  2004/12/09 23:47:07  posk
// Minor changes in code formatting.
// Added hist for TPC primary dca to AnalysisMaker.
//
// Revision 1.45  2004/12/07 23:10:21  posk
// Only odd and even phiWgt hists. If the old phiWgt file contains more than
// two harmonics, only the first two are read. Now writes only the first two.
//
// Revision 1.44  2004/05/05 21:13:47  aihong
// Gang's code for ZDC-SMD added
//
// Revision 1.43  2003/11/14 20:00:43  oldi
// Implementation of v1{EP1,EP2}. This method is set to be the default for v1 now!
// Minor code clean-ups.
//
// Revision 1.42  2003/09/10 19:47:14  perev
// ansi corrs
//
// Revision 1.41  2003/09/02 17:58:10  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.40  2003/08/06 20:54:09  oldi
// Introduction of possibility to exclude pt ranges for v(eta) and eta regions
// for v(pt) histograms. Default behavior stays the same (all available tracks
// are included in v(pt) and v(eta)).
//
// Revision 1.39  2003/05/06 21:33:06  posk
// Removed some histograms.
//
// Revision 1.38  2003/05/06 18:38:06  posk
// Removed StFlowTagMaker.
//
// Revision 1.37  2003/01/10 16:40:33  oldi
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
// Revision 1.36  2002/11/26 22:11:53  posk
// First use of doxygen.
//
// Revision 1.35  2002/10/28 19:45:55  posk
// Eliminate events with Psi=0.
//
// Revision 1.34  2001/12/11 22:03:56  posk
// Four sets of phiWgt histograms.
// StFlowMaker StFlowEvent::PhiWeight() changes.
// Cumulant histogram names changed.
//
// Revision 1.33  2001/11/09 21:14:46  posk
// Switched from CERNLIB to TMath. Using global dca instead of dca.
//
// Revision 1.32  2001/08/02 17:41:50  snelling
// Added trigger histogram
//
// Revision 1.31  2001/05/22 20:11:15  posk
// Changed dEdx graphs.
//
// Revision 1.30  2001/04/25 17:45:52  perev
// HPcorrs
//
// Revision 1.29  2000/12/12 15:01:11  posk
// Put log comments at end of file.
//
// Revision 1.28  2000/12/10 02:02:02  oldi
// A new member (StTrackTopologyMap mTopology) was added to StFlowPicoTrack.
// The evaluation of either a track originates from the FTPC or not is
// unambiguous now. The evaluation itself is easily extendible for other
// detectors (e.g. SVT+TPC). Old flowpicoevent.root files are treated as if
// they contain TPC tracks only (backward compatibility).
//
// Revision 1.27  2000/12/08 17:04:09  oldi
// Phi weights for both FTPCs included.
//
// Revision 1.24  2000/09/22 22:01:40  posk
// Doubly integrated v now contains resolution error.
//
// Revision 1.23  2000/09/16 22:23:06  snelling
// Auto magically switch to rapidity when identified particles are used
//
// Revision 1.22  2000/09/15 22:52:55  posk
// Added Pt weighting for event plane calculation.
//
// Revision 1.21  2000/09/12 01:31:00  snelling
// Added pid histograms for e- e+ and dbar
//
// Revision 1.20  2000/08/31 18:50:30  posk
// Added plotCen.C to plot from a series of files with different centralities.
//
// Revision 1.19  2000/08/09 21:38:59  snelling
// Added monitor histograms
//
// Revision 1.18  2000/08/01 21:51:20  posk
// Added doubly integrated v.
//
// Revision 1.17  2000/07/03 02:07:49  perev
// StEvent: vector<TObject*>
//
// Revision 1.16  2000/06/30 14:51:19  posk
// Using MessageMgr. Added graph for Eta Symmetry vs. Vertex Z.
//
// Revision 1.15  2000/05/26 21:25:22  posk
// Use TProfile2D class and profile projection methods.
// Correction needed for >2 subevents.
//
// Revision 1.14  2000/05/16 17:30:21  snelling
// removed the dependencies cint did not like
//
// Revision 1.13  2000/05/03 16:38:35  posk
// Compatable with ROOT 2.24/02.
//
// Revision 1.12  2000/04/13 22:34:15  posk
// Resolution correction is now made.
//
// Revision 1.11  2000/03/21 00:24:45  posk
// Added GetCVS and changed some plot names.
//
// Revision 1.10  2000/03/15 23:32:04  posk
// Added StFlowSelection.
//
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
// Revision 1.7  2000/01/14 01:35:52  snelling
// changed include path ../FlowMaker/ to FlowMaker/
//
// Revision 1.3  1999/12/04 00:15:40  posk
// Works with StFlowEvent which works with the new StEvent
//
// Revision 1.2  1999/11/24 18:14:07  posk
// Now reads event quantities with StFlowEvent methods
//
// Revision 1.7  1999/11/05 00:02:03  posk
// Changed the flow vector, Q, to a TVector2.
//
// Revision 1.6  1999/10/05 16:54:11  posk
// Added getPhiWeight method for making the event plane isotropic.
//
/////////////////////////////////////////////////////////////////////////////////

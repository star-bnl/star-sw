///////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowAnalysisMaker.h,v 1.25 2000/09/29 22:53:16 posk Exp $
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
// Revision 1.25  2000/09/29 22:53:16  posk
// More histograms.
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
#include "StMaker.h"
#include "StFlowMaker/StFlowConstants.h"
#include "TVector2.h"
#include "TString.h"
class StFlowEvent;
class FlowTag_st;
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
  virtual  const char *GetCVS() const {static const char cvs[]=
    "Tag $Name:  $ $Id: StFlowAnalysisMaker.h,v 1.25 2000/09/29 22:53:16 posk Exp $ built "__DATE__" "__TIME__ ;
    return cvs;}

private:

  void     FillFromTags();
  void     FillFromFlowEvent();
  void     FillEventHistograms();
  void     FillParticleHistograms();

  TVector2 mQ[Flow::nSels][Flow::nHars];                     //! flow vector
  Float_t  mPsi[Flow::nSels][Flow::nHars];                   //! event plane angle
  UInt_t   mMult[Flow::nSels][Flow::nHars];                  //! multiplicity
  Float_t  mMeanPt[Flow::nSels][Flow::nHars];                //! mean Pt
  Float_t  m_q[Flow::nSels][Flow::nHars];                    //! Q/sqrt(Mult)
  TVector2 mQSub[Flow::nSels*Flow::nSubs][Flow::nHars];      //! flow vector subs
  Float_t  mPsiSub[Flow::nSels*Flow::nSubs][Flow::nHars];    //! plane angle subs
  Float_t  mMeanPtSub[Flow::nSels*Flow::nSubs][Flow::nHars]; //! mean Pt subs
  UInt_t   mMultSub[Flow::nSels*Flow::nSubs][Flow::nHars];   //! multiplicity subs
  Float_t  mRes[Flow::nSels][Flow::nHars];      //! event plane resolution
  Float_t  mResErr[Flow::nSels][Flow::nHars];   //! event plane resolution error
  TString          xLabel;      //! label axis with rapidity or pseudorapidity 
  StFlowEvent*     pFlowEvent;  //! pointer to StFlowEvent
  FlowTag_st*      pFlowTag;    //! pointer to StEvent
  StFlowSelection* pFlowSelect; //! selection object

  // for single histograms
  TH1F*     mHistCharge;               //!
  TH1F*     mHistDca;                  //!
  TH1F*     mHistDcaGlobal;            //!
  TH1F*     mHistChi2;                 //!
  TH1F*     mHistFitPts;               //!
  TH1F*     mHistMaxPts;               //!
  TH1F*     mHistFitOverMax;           //!
  TH1F*     mHistOrigMult;             //!
  TH1F*     mHistMultEta;              //!
  TH1F*     mHistMult;                 //!
  TH1F*     mHistMultOverOrig;         //!
  TH1F*     mHistMultPart;             //!
  TH1F*     mHistVertexZ;              //!
  TH2F*     mHistVertexXY2D;           //!
  TH1F*     mHistEtaSym;               //!
  TH3F*     mHistEtaPtPhi3D;           //!
  TH2D*     mHistYieldAll2D;           //!
  TH2D*     mHistYieldPart2D;          //!
  TProfile* mHistBinEta;               //!
  TProfile* mHistBinPt;                //!
  TProfile* mHistCosPhi;               //!
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
    TH1F*       mHistMeanPt;
    TH1F*       mHist_q;
    TH2D*       mHistYield2D;
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
    TProfile* mHistCos;
    TH1F*     mHistRes;
    TProfile* mHist_vObs;
    TH1D*     mHist_v;
    struct histFullHars histFullHar[Flow::nHars];
  };
  struct histFulls histFull[Flow::nSels]; //!

  static const Float_t qMax;

  TString      MakerName;

  ClassDef(StFlowAnalysisMaker, 1)              // macro for rootcint
};

inline Float_t StFlowAnalysisMaker::Res(Int_t eventN, Int_t harN) const 
  { return mRes[eventN][harN]; }

inline Float_t StFlowAnalysisMaker::ResErr(Int_t eventN, Int_t harN) const 
  { return mResErr[eventN][harN]; }

#endif

#ifndef StQACosmicMaker_HH
#define StQACosmicMaker_HH
/***************************************************************************
 *
 * $Id: StQACosmicMaker.h,v 1.7 1999/09/23 18:25:19 snelling Exp $
 *
 * Author: Raimond Snellings, LBNL, Jun 1999
 * Description:  Maker to QA the Cosmic data (hitfinding, tracking, 
 *               geometry etc.)
 *
 * $Log: StQACosmicMaker.h,v $
 * Revision 1.7  1999/09/23 18:25:19  snelling
 * Added QA hists for hitclus table and morphology table
 *
 * Revision 1.6  1999/09/03 16:07:37  snelling
 * Added method to write out histogrmas and TNtuple
 *
 * Revision 1.5  1999/08/26 03:36:58  snelling
 * Added Q versus pad
 *
 * Revision 1.4  1999/08/19 00:29:58  snelling
 * Added Q distribution histograms and only used points on track
 *
 * Revision 1.3  1999/08/17 18:55:55  snelling
 * Added two member funtions: setSector and setNrXbins
 *
 * Revision 1.2  1999/08/17 01:44:32  snelling
 * changed ntuple projection to normal histogram filling
 *
 * Revision 1.2  1999/08/03 17:15:53  snelling
 * added id tags
 *
 *  
 **************************************************************************/
#include "StMaker.h"
#include "St_TableSorter.h"
#include "tpc/St_tcl_Module.h"
#include "tpc/St_tph_Module.h"
#include "tpc/St_tpt_residuals_Module.h"
#include "tpc/St_xyz_newtab_Module.h"
#include "St_tcc_morphology_Table.h"
#include "TH1.h"
#include "TH2.h"
#include "TNtuple.h"
#include "TProfile.h"
#include "TString.h"

class StQACosmicMaker : public StMaker {

 public:

  StQACosmicMaker(const char *name="QACosmics");
  virtual        ~StQACosmicMaker();

  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual void   PrintInfo();

  virtual void   setSector(const Int_t sectorNumber);
  virtual void   setNrXbins(const Int_t bins) {nXBins = bins;}

  virtual void   WriteTNtupleOn() {WriteTNtuple(kTRUE);}
  virtual void   WriteTNtupleOff(){WriteTNtuple();}
  virtual void   WritePostscriptOn() {WritePostscript(kTRUE);} 
  virtual void   WritePostscriptOff(){WritePostscript();}
  virtual void   WriteHistogramsOn() {WriteHistograms(kTRUE);} 
  virtual void   WriteHistogramsOff(){WriteHistograms();}

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StQACosmicMaker.h,v 1.7 1999/09/23 18:25:19 snelling Exp $ built "__DATE__" "__TIME__ ; return cvs;}

 private:

  Int_t  SelectedSector;
  Bool_t bSectorSelectionOn;
  Bool_t bWriteTNtupleOn;
  Bool_t bWritePostscriptOn;
  Bool_t bWriteHistogramsOn;
  Int_t  nXBins;
  TString MakerName;

  Int_t  initClusHistograms();
  Int_t  fillClusHistograms();
  Int_t  initMorphHistograms();
  Int_t  fillMorphHistograms();
  Int_t  initResHistograms();
  Int_t  fillResHistograms();
  Int_t  calcResHistograms();
  Int_t  initChargeHistograms();
  Int_t  fillChargeHistograms();
  Int_t  calcChargeHistograms();
  Int_t  writeOutHistograms();
  Int_t  writeOutPostscript();

  Int_t  initTNtuple();
  Int_t  fillTNtuple();
  Int_t  writeOutTNtuple();

  Int_t  fillTablePointers();
  Int_t  cleanUpTableSorters();

  Bool_t btphit;
  Bool_t btphitclus;
  Bool_t bmorph;
  Bool_t brestpt;
  Bool_t btptrack;
  
  // bool selection
  void   WritePostscript(Bool_t flag=kFALSE){bWritePostscriptOn=flag;}
  void   WriteTNtuple(Bool_t flag=kFALSE){bWriteTNtupleOn=flag;}
  void   WriteHistograms(Bool_t flag=kFALSE){bWriteHistogramsOn=flag;}
  void   SectorSelection(Bool_t flag=kFALSE){bSectorSelectionOn=flag;}
  void   SectorSelectionOn() {SectorSelection(kTRUE);} 
  void   SectorSelectionOff(){SectorSelection();}

  //-----------------------------------------------------------------------
  // pointers to tables
  // pixel table
  St_tfc_adcxyz *phtfc; //!
  tfc_adcxyz_st *ptadcxyz; //!

  // hit table
  St_tcl_tphit *phtcl; //!
  tcl_tphit_st *pttphit; //!

  // Li Qun's table
  St_tcl_hitclus *phhcl; //!
  tcl_hitclus_st *pthcl; //!

  // Tom's morphology table
  St_tcc_morphology *phmorph; //!
  tcc_morphology_st *ptmorph; //!

  // track table 
  St_tpt_track *phtrk; //!
  tpt_track_st *pttrk; //!

  // residuals
  St_tpt_res *phres;//!
  tpt_res_st *ptres; //!

  // Table sorters
  St_TableSorter *ressorter; //!
  St_TableSorter *trksorter; //!
  St_TableSorter *morphsorter; //!

 protected:

  enum {nResHist = 4, nChargeHist = 4, nClusterHist = 4,
	nMorphHist = 4 };

  TNtuple *mTNtupleTPC; //!

  struct ClusterHist {
    TH1F *mNHits;
    TH1F *mNPadsPerCluster;
    TH1F *mNTimeBucketsPerCluster;
    TH1F *mNPadsPerHit;
    TH1F *mNTimeBucketsPerHit;
  };

  struct ClusterHist ClusterHists[nClusterHist]; //!

  struct MorphHist {
    TH1F *mNumberOfSequences;
    TH1F *mNumberOfPixels;
    TH1F *mNumberOfPads;
    TH1F *mNumberOfHits;
    TH1F *mTotalCharge;
    TH1F *mMaxCharge;
    TH1F *mAverageCharge;
    TH1F *mPadSigma1;
    TH1F *mTimeSigma1;
    TH1F *mPadTimeSigma1Sq;
    TH1F *mEcc1;
    TH1F *mLinEcc1;
    TH1F *mPadSigma2;
    TH1F *mTimeSigma2;
    TH1F *mPadTimeSigma2Sq;
    TH1F *mEcc2;
    TH1F *mLinEcc2;
  };
  
  struct MorphHist MorphHists[nMorphHist]; //!
  
  struct FitHist {
    TH1D *mXYResVersusAlpha_mean;
    TH1D *mXYResVersusAlpha_sigma;
    TH1D *mXYResVersusAlpha_mag;
    TH1D *mXYResVersusAlpha_chi;
  };

  struct ResidualHist;
  friend struct ResidualHist;

  struct ResidualHist {
    TH2F *mXYResVersusAlpha;
    struct FitHist FitHists; 
  };

  // histograms for inner/outer sector plus low/high momentum
  struct ResidualHist ResidualHists[nResHist]; //! 


  // histograms for Charge Uniformity
  struct FitQHist {
    TH1D *mQ_mean;
    TH1D *mQ_sigma;
    TH1D *mQ_mag;
    TH1D *mQ_chi;
  };

  struct ChargeHist;
  friend struct ChargeHist;

  struct ChargeHist {
    TH2F *mQdist;
    TProfile *mQprof;
    struct FitQHist FitQHists; 
  };

  // histograms for q versus x,y,z
  struct ChargeHist ChargeHists[nChargeHist]; //! 

  ClassDef(StQACosmicMaker, 1) //macro for rootcint
};

#endif

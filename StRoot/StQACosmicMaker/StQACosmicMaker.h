#ifndef StQACosmicMaker_HH
#define StQACosmicMaker_HH
/***************************************************************************
 *
 * $Id: StQACosmicMaker.h,v 1.5 1999/08/26 03:36:58 snelling Exp $
 *
 * Author: Raimond Snellings, LBNL, Jun 1999
 * Description:  Maker to QA the Cosmic data (hitfinding, tracking, 
 *               geometry etc.)
 *
 * $Log: StQACosmicMaker.h,v $
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
#include "TH1.h"
#include "TH2.h"
#include "TNtuple.h"
#include "TProfile.h"

class StQACosmicMaker : public StMaker {

 private:

  Int_t SelectedSector;
  Bool_t bSectorSelectionOn;
  Bool_t bWriteTNtupleOn;
  Bool_t bWritePostscriptOn;
  Int_t nXBins;

  virtual void   SectorSelection(Bool_t flag=kFALSE){bSectorSelectionOn=flag;}
  virtual void   SectorSelectionOn() {SectorSelection(kTRUE);} 
  virtual void   SectorSelectionOff(){SectorSelection();}
  virtual Int_t  initResHistograms();
  virtual Int_t  fillResHistograms();
  virtual Int_t  calcResHistograms();
  virtual Int_t  initChargeHistograms();
  virtual Int_t  fillChargeHistograms();
  virtual Int_t  calcChargeHistograms();
  virtual Int_t  initTNtuple();
  virtual Int_t  fillTNtuple();
  virtual void   WritePostscript(Bool_t flag=kFALSE){bWritePostscriptOn=flag;}
  virtual void   WriteTNtuple(Bool_t flag=kFALSE){bWriteTNtupleOn=flag;}

 protected:

  enum {nResHist = 4, nChargeHist = 4};

  TNtuple *mTNtupleTPC; //!

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
    struct FitQHist FitQHists; 
  };

  // histograms for q versus x,y,z
  struct ChargeHist ChargeHists[nChargeHist]; //! 

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


  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StQACosmicMaker.h,v 1.5 1999/08/26 03:36:58 snelling Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StQACosmicMaker, 1) //macro for rootcint
};

#endif







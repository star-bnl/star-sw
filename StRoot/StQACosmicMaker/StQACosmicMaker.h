#ifndef StQACosmicMaker_HH
#define StQACosmicMaker_HH
/***************************************************************************
 *
 * $Id: StQACosmicMaker.h,v 1.1.1.2 1999/09/08 17:34:25 snelling Exp $
 *
 * Author: Raimond Snellings, LBNL, Jun 1999
 * Description:  Maker to QA the Cosmic data (hitfinding, tracking, 
 *               geometry etc.)
 *
 * $Log: StQACosmicMaker.h,v $
 * Revision 1.1.1.2  1999/09/08 17:34:25  snelling
 * added documentation
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
  {static const char cvs[]="Tag $Name:  $ $Id: StQACosmicMaker.h,v 1.1.1.2 1999/09/08 17:34:25 snelling Exp $ built "__DATE__" "__TIME__ ; return cvs;}

 private:

  Int_t SelectedSector;
  Bool_t bSectorSelectionOn;
  Bool_t bWriteTNtupleOn;
  Bool_t bWriteHistogramsOn;
  Bool_t bWritePostscriptOn;
  Int_t nXBins;
  TString MakerName;

  virtual Int_t  initResHistograms();
  virtual Int_t  fillResHistograms();
  virtual Int_t  calcResHistograms();
  virtual Int_t  initChargeHistograms();
  virtual Int_t  fillChargeHistograms();
  virtual Int_t  calcChargeHistograms();
  virtual Int_t  writeOutHistograms();
  virtual Int_t  writeOutPostscript();

  virtual Int_t  initTNtuple();
  virtual Int_t  fillTNtuple();
  virtual Int_t  writeOutTNtuple();

  // bool selection
  virtual void   WritePostscript(Bool_t flag=kFALSE){bWritePostscriptOn=flag;}
  virtual void   WriteTNtuple(Bool_t flag=kFALSE){bWriteTNtupleOn=flag;}
  virtual void   WriteHistograms(Bool_t flag=kFALSE){bWriteHistogramsOn=flag;}
  virtual void   SectorSelection(Bool_t flag=kFALSE){bSectorSelectionOn=flag;}
  virtual void   SectorSelectionOn() {SectorSelection(kTRUE);} 
  virtual void   SectorSelectionOff(){SectorSelection();}


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
    TProfile *mQprof;
    struct FitQHist FitQHists; 
  };

  // histograms for q versus x,y,z
  struct ChargeHist ChargeHists[nChargeHist]; //! 

  ClassDef(StQACosmicMaker, 1) //macro for rootcint
};

#endif

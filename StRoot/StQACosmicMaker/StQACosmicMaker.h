#ifndef StQACosmicMaker_HH
#define StQACosmicMaker_HH
/***************************************************************************
 *
 * $Id: StQACosmicMaker.h,v 1.2 1999/08/17 01:44:32 snelling Exp $
 *
 * Author: Raimond Snellings, LBNL, Jun 1999
 * Description:  Maker to QA the Cosmic data (hitfinding, tracking etc.)
 *
 * $Log: StQACosmicMaker.h,v $
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

 protected:

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

  struct ResidualHist ResidualHists[4]; //! 

 public: 

  StQACosmicMaker(const char *name="QACosmics");
  virtual        ~StQACosmicMaker(); 
  
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual void   PrintInfo();
  virtual Int_t  initHistograms();
  virtual Int_t  fillHistograms();
  virtual Int_t  calcHistograms();
  virtual Int_t  initTNtuple();
  virtual Int_t  fillTNtuple();
  virtual Int_t  Finish();
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StQACosmicMaker.h,v 1.2 1999/08/17 01:44:32 snelling Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StQACosmicMaker, 1) //macro for rootcint
};

#endif





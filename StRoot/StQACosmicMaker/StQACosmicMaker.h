#include "StMaker.h"
#include "TH1.h"
#include "TH2.h"
#include "TNtuple.h"
#include "TProfile.h"


class StQACosmicMaker : public StMaker {

 private:

 protected:

  TNtuple *nttpc; //!
  TProfile *xyresvsalpha_prof; //!

  TH2F *xyresvsalpha_inner; //!
  TH1D *xyresvsalpha_inner_mean; //!
  TH1D *xyresvsalpha_inner_sigma; //!
  TH1D *xyresvsalpha_inner_mag; //!
  TH1D *xyresvsalpha_inner_chi; //!

  TH2F *xyresvsalpha_inner_lowpt; //!
  TH1D *xyresvsalpha_inner_lowpt_mean; //!
  TH1D *xyresvsalpha_inner_lowpt_sigma; //!
  TH1D *xyresvsalpha_inner_lowpt_mag; //!
  TH1D *xyresvsalpha_inner_lowpt_chi; //!

  TH2F *xyresvsalpha_outer; //!
  TH1D *xyresvsalpha_outer_mean; //!
  TH1D *xyresvsalpha_outer_sigma; //!
  TH1D *xyresvsalpha_outer_mag; //!
  TH1D *xyresvsalpha_outer_chi; //!

  TH2F *xyresvsalpha_outer_lowpt; //!
  TH1D *xyresvsalpha_outer_lowpt_mean; //!
  TH1D *xyresvsalpha_outer_lowpt_sigma; //!
  TH1D *xyresvsalpha_outer_lowpt_mag; //!
  TH1D *xyresvsalpha_outer_lowpt_chi; //!


 public: 

  StQACosmicMaker(const char *name="QACosmics");
  virtual        ~StQACosmicMaker(); 
  
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual void   PrintInfo();
  virtual void   MakeHistograms();
  virtual Int_t  Finish();
  ClassDef(StQACosmicMaker, 1)
    };



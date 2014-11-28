//Used to generate pedestals from daq file histograms

#ifndef StEEmcDAQ2Ped_HH
#define StEEmcDAQ2Ped_HH

#include "StMaker.h"
#include "TFile.h"
#include "TH2F.h"

class StEEmcDb;
#include "StEEmcUtil/EEfeeRaw/EEname2Index.h"
#include "StEEmcUtil/EEfeeRaw/EEdims.h"

class StEEmcDAQ2Ped : public StMaker {

 public:

  StEEmcDAQ2Ped(const char* name, TFile* file);
  virtual ~StEEmcDAQ2Ped();
    
  virtual Int_t Init();
  virtual Int_t InitRun(int runNo);
  virtual Int_t Finish();
  virtual Int_t Make();
  void initHistos();
  void ChooseSet(int x){mSet = x;}
  void MappingFile(TString x){mappingFile = x;}

  //Histogram
  void SetHList(TObjArray * x){mHList=x;}

 private:
  StEEmcDb* mEeDb;
  TH1F*  hPix[6][140];
  TH2F*  h2D;
  TObjArray  *mHList; /// output histo access point
  TFile* mDAQHistos;
  TH1* esmd[50];
  TH1* esmdSec[12][2];
  TH1* etow[6];
  TH1* xLowEtow; TH1* xHighEtow; TH1* xDiffEtow; TH2* xCorrEtow;
  TH1* xLowEsmd; TH1* xHighEsmd; TH1* xDiffEsmd; TH2* xCorrEsmd;
  TH1* xTestEsmd;
  int mSet;
  TString mappingFile;
  ClassDef(StEEmcDAQ2Ped,1)
    
  };


#endif

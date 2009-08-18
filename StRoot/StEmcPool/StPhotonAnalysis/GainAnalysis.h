#ifndef GAINANALYSIS_H
#define GAINANALYSIS_H

#include <TObject.h>
#include <TPostScript.h>
#include <TH1F.h>
#include <TH2F.h>
#include <TGraphErrors.h>
#include <TFile.h>
#include <TTree.h>

class MyEvent;
class MyPoint;

class GainAnalysis : public TObject{
 protected:
  TFile *mFile;
  TFile *mFileOut;

  TTree *myEventTree;
  MyEvent *ev; //!
  TPostScript *ps;

  TH2F *h_minvPt;
  TH2F *h_minvId;

 public:

  GainAnalysis(const char*,const char*);
  ~GainAnalysis();
  Int_t init(const char*);
  Int_t make(Int_t evmax=0,const char* d="default");
  Int_t finish();

  Bool_t isPointOK(MyPoint *p);

  ClassDef(GainAnalysis,1)
};

#endif

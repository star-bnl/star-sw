#ifndef STAR_StTriggerStudyMaker
#define STAR_StTriggerStudyMaker

#include "StMaker.h"

class TTree;
class TFile;

class StTriggerSimuMaker;
class StEmcCollection;
class StEmcADCtoEMaker;
class StTriggerStudyEvent;
class StEmcGeom;
class StEmcDecoder;
class StMuDstMaker;
class StBemcTables;

using namespace std;

class StTriggerStudyMaker : public StMaker {
 private:
  StMuDstMaker* muDstMaker;
  StEmcADCtoEMaker* mADCtoEMaker;
  StTriggerSimuMaker* mTriggerSimuMaker;
  TTree* mTree;
  TFile* mFile;
  StEmcGeom* mEmcGeom;
  StTriggerStudyEvent* mTSEvent;
  StBemcTables* mBemcTables;
  StEmcCollection* mEmcCollection;
  const char* mName;
  map<int,int> trigmap;
  const int nTriggers;
  int triggers[12];
  TH1F* bbctall;
  TH1F* zvertall;
  TH1F* zverttrig[12];
  TH1F* bbcttrig[12];
  TH1F* jp1et;
  TH1F* jp2et;
  TH1F* jp1et0;
  TH1F* jp0et0;
  StEmcDecoder* mDecoder;

  void fillTree();

  int runNumber;

 public: 
  StTriggerStudyMaker(const char* filename, const char *name="TrigStudy");
  virtual       ~StTriggerStudyMaker() {}
  virtual Int_t Init();
  virtual Int_t InitRun(int run);
  virtual Int_t  Make();
  virtual Int_t Finish();
  
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StTriggerStudyMaker.h,v 1.2 2014/08/06 11:43:40 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
  
  ClassDef(StTriggerStudyMaker,0)   //StAF chain virtual base class for Makers
};

#endif

#ifndef __HftMatchedTree_H
#define __HftMatchedTree_H
#include "StMaker.h"
#include "TFile.h"
#include "TArrayI.h"
#include "TTree.h"
class StPxlDb;
class StIstDb;
class EventT;
class HftMatchedTree : public StMaker {
 public:
  HftMatchedTree(const Char_t *name="HftMatTree");
  virtual ~HftMatchedTree() {}
  virtual Int_t Init();
  Int_t InitRun(Int_t runnumber);
  virtual Int_t Make();
  virtual Int_t Finish();
  void SetTree();
  void Print(Option_t *opt="") const;
  void SetMinNoHits(Int_t MinNoHits=0) {fMinNoHits = MinNoHits;}
  void SetpCut(Double_t pCut=0.0) {fpCut = pCut;}
  void SetOut(Char_t *Out="Event") {fOut = Out;}
 private:
  TFile   *fFile;
  TTree   *fTree;
  EventT  *fEvent;
  Int_t    fMinNoHits;
  Double_t fpCut;
  Char_t  *fOut;
  
  StPxlDb  *mPxlDb;
  StIstDb  *fIstDb;
  ClassDef(HftMatchedTree,1)
};
#endif

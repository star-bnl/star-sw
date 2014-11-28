#ifndef __SvtMatchedTree_H
#define __SvtMatchedTree_H
#include "StMaker.h"
#include "TFile.h"
#include "TArrayI.h"
#include "TTree.h"
class EventT;
class SvtMatchedTree : public StMaker {
 public:
  SvtMatchedTree(const Char_t *name="SvtMatTree");
  virtual ~SvtMatchedTree() {}
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
  void SetTree();
  void Print(Option_t *opt="") const;
  void SetMinNoHits(Int_t MinNoHits=0) {fMinNoHits = MinNoHits;}
  void SetpCut(Double_t pCut=0.0) {fpCut = pCut;}
  void SetOut(Char_t *Out="Event") {fOut = Out;}
  void MakeListOfRotations();
 private:
  TFile   *fFile;
  TTree   *fTree;
  EventT  *fEvent;
  Int_t    fMinNoHits;
  Double_t fpCut;
  Char_t  *fOut;
  ClassDef(SvtMatchedTree,1)
};
#endif

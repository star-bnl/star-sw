#ifndef __StGmtAligner_H
#define __StGmtAligner_H
#include "StMaker.h"
#include "TFile.h"
#include "TArrayI.h"
#include "TTree.h"

//________________
class EventT;

//________________
class StGmtAligner : public StMaker {
 public:
  StGmtAligner(const Char_t *name="GmtAligner");
  virtual ~StGmtAligner() {}
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
  void SetTree();
  void Print(Option_t *opt="") const;
  void SetMinNoHits(Int_t MinNoHits=0) {fMinNoHits = MinNoHits;}
  void SetpCut(Double_t pCut=0.0) {fpCut = pCut;}
  void SetOut(Char_t *Out="Event") {fOut = Out;}
  void MakeListOfRotations();
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StGmtClusterMaker.h,v 1.1.1.1 2013/09/02 15:01:02 fisyak Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
 private:
  TFile   *fFile;
  TTree   *fTree;
  EventT  *fEvent;
  Int_t    fMinNoHits;
  Double_t fpCut;
  Char_t  *fOut;
  ClassDef(StGmtAligner,1)
};
#endif

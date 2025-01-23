#ifndef __StGmtAlignmentMaker_H
#define __StGmtAlignmentMaker_H
#include "StMaker.h"
#include "TFile.h"
#include "TArrayI.h"
#include "TTree.h"

//________________
class EventT;

//________________
class StGmtAlignmentMaker : public StMaker {
  public:
    StGmtAlignmentMaker(const Char_t *name="GmtAligner");
    virtual ~StGmtAlignmentMaker() {}
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();
    void SetTree();
    void Print(Option_t *opt="") const;
    void SetMinNoHits(Int_t MinNoHits=0) {fMinNoHits = MinNoHits;}
    void SetpCut(Double_t pCut=0.0) {fpCut = pCut;}
    void SetOut(const Char_t *Out="Event") {fOut = Out;}
    void MakeListOfRotations();
    virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StGmtAlignmentMaker.h,v 1.1.1.2 2025/01/22 15:01:02 gnigmat Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
  private:
    TFile   *fFile;
    TTree   *fTree;
    EventT  *fEvent;
    Int_t    fMinNoHits;
    Double_t fpCut;
    const Char_t  *fOut;
    ClassDef(StGmtAlignmentMaker,1)
};
#endif

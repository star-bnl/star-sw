#ifndef StTrgTreeMaker_h
#define StTrgTreeMaker_h

#include "StMaker.h"
class TString;
class TFile;
class TTree;

class StTrgTreeMaker : public StMaker {
  public:
    StTrgTreeMaker(const char *name="trgTree", const char *outName="trgTree.root");
    virtual ~StTrgTreeMaker();
    
    virtual Int_t Init();
    virtual Int_t Make();
    virtual void  Clear(Option_t *opt="");
    virtual Int_t Finish();
    
  private:
    TString         mOutName;
    TFile*          mFile=0;
    TTree*          mTree=0;

    struct {
	Int_t bbcAdcSumE;
	Int_t bbcAdcSumW;
	Int_t bbcTacDiff;
	Int_t vpdAdcSumE;
	Int_t vpdAdcSumW;
	Float_t vpdTacDiff;
	Int_t zdcAdcSumE;
	Int_t zdcAdcSumW;
	Int_t zdcTacDiff;
	Int_t tofMult;
	Int_t epdNHitsE;
        Int_t epdNHitsW;
        Int_t epdTacDiff;
    } mTrg;
    
    ClassDef(StTrgTreeMaker, 0);
};

#endif

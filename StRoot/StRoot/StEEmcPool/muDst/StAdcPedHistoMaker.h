//  used to generate pedestals from a regualr muDst

#ifndef StAdcPedHistoMaker_HH
#define StAdcPedHistoMaker_HH

#include "StMaker.h"

class StEEmcDb;
class StMuDstMaker;
#include "StEEmcUtil/EEfeeRaw/EEname2Index.h"
#if 0
class TTree;
class TFile;
class TNtuple;


class EEmcGeomSimple;
class StMuTrack;
class StBemcTables;
class TH2F;
#endif

class StAdcPedHistoMaker : public StMaker {

 public:

  StAdcPedHistoMaker(const char* name, StMuDstMaker* uDstMaker);
  virtual ~StAdcPedHistoMaker();
    
  virtual Int_t Init();
  virtual Int_t InitRun(int runNo);
  virtual Int_t Finish();
  virtual Int_t Make();
  void initHisto();
  void  SetTrigId(int x) { trigID=x;}
  void DoPedSubtraction() { pedSub=true;}
  void SetKillMask(unsigned int mask) { killMask=mask; }

  //Histogram
  void SetHList(TObjArray * x){HList=x;}

 private:
  StEEmcDb* mEeDb;
  StMuDstMaker* mDstMaker;
  TH1F*  hPix[EEindexMax];
  bool pedSub;
  unsigned int killMask;
  TObjArray  *HList; /// output histo access point
  int trigID;

  ClassDef(StAdcPedHistoMaker,1)
};


#endif

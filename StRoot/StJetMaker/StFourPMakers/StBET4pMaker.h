// -*- mode: c++;-*-
// $Id: StBET4pMaker.h,v 1.21 2008/06/06 01:17:50 tai Exp $
#ifndef STBET4PMAKER_HH
#define STBET4PMAKER_HH

#include "StFourPMaker.h"

class StMuDstMaker;
class StBemcTables;
class StBET4pMakerImp;

class StBET4pMaker : public StFourPMaker {

public:
    
  StBET4pMaker(const char* name, StMuDstMaker* uDstMaker, bool doTowerSwapFix = true);
    
  virtual ~StBET4pMaker() {};
    
  Int_t Init();    
  Int_t Make();
    
  void Clear(Option_t* opt);

  Int_t InitRun(Int_t runId);

  FourList &getTracks();

  void setUseEndcap(bool v);
  void setUse2003Cuts(bool v);
  void setUse2005Cuts(bool v);
  void setUse2006Cuts(bool v);

  int nDylanPoints() const { return mDylanPoints; }
  double sumEmcEt() const { return mSumEmcEt; }

  bool bemcCorrupt() const { return mCorrupt; }
    
private:

  bool mCorrupt;

  StBemcTables* mTables;

  int mDylanPoints;
  double mSumEmcEt;
        
  StBET4pMakerImp* _imp;
  
  bool isBemcCorrupted();

  ClassDef(StBET4pMaker,1)
};

#endif // STBET4PMAKER_HH

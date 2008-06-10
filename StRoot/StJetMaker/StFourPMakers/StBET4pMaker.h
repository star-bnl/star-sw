// -*- mode: c++;-*-
// $Id: StBET4pMaker.h,v 1.25 2008/06/10 05:58:00 tai Exp $
#ifndef STBET4PMAKER_HH
#define STBET4PMAKER_HH

#include "StFourPMaker.h"

#include "CollectEnergyDepositsFromBEMC.h"

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

  bool bemcCorrupt() const { return isBemcCorrupted(); }
    
private:

  double sumEnergyOverBemcTowers(double minE, const StSpinJet::TowerEnergyDepositList &energyDepositList);
  int numberOfBemcTowersWithEnergyAbove(double minE, const StSpinJet::TowerEnergyDepositList &energyDepositList);

  StBemcTables* _bemcTables;

  int mDylanPoints;
  double mSumEmcEt;
        
  StSpinJet::CollectEnergyDepositsFromBEMC *_collectEnergyDepositsFromBEMC;

  StBET4pMakerImp* _imp;
  

  bool isBemcCorrupted() const;

  ClassDef(StBET4pMaker,1)
};

#endif // STBET4PMAKER_HH

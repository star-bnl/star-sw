// -*- mode: c++;-*-
// $Id: CollectEnergyDepositsFromEEMC.h,v 1.5 2008/07/09 05:25:11 tai Exp $
#ifndef COLLECTENERGYDEPOSITSFROMEEMC_H
#define COLLECTENERGYDEPOSITSFROMEEMC_H

#include "TowerEnergyDeposit.h"

class StMuDstMaker;
class EEmcGeomSimple;
class StEEmcDbMaker;

namespace StSpinJet {

class StJetEEMC {

public:

  StJetEEMC() { }
  virtual ~StJetEEMC() { }

  virtual TowerEnergyDepositList getEnergyList() = 0;

private:

};

class StJetEEMCMuDst : public StJetEEMC {

public:
  StJetEEMCMuDst(StMuDstMaker* uDstMaker);
  virtual ~StJetEEMCMuDst() { }

  void Init(StEEmcDbMaker* eedb);

  TowerEnergyDepositList getEnergyList();


private:

  StMuDstMaker* mMuDstMaker;
  EEmcGeomSimple* mEeGeom;
  StEEmcDbMaker* mEeDb;

};

class CollectEnergyDepositsFromEEMC {

public:
  CollectEnergyDepositsFromEEMC(StJetEEMC* eemc);
  virtual ~CollectEnergyDepositsFromEEMC() { }

  TowerEnergyDepositList Do();

private:

  TowerEnergyDepositList getEnergyList();

  StJetEEMC* _eemc;

};

}


#endif // COLLECTENERGYDEPOSITSFROMEEMC_H

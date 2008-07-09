// -*- mode: c++;-*-
// $Id: StJetBEMCMuDst.h,v 1.1 2008/07/09 00:04:17 tai Exp $
#ifndef STJETBEMCMUDST_H
#define STJETBEMCMUDST_H

#include "StJetBEMC.h"

class StEmcRawHit;
class StMuDstMaker;
class StBemcTables;

namespace StSpinJet {

class StJetBEMCMuDst : public StJetBEMC {

public:
  StJetBEMCMuDst(StMuDstMaker* uDstMaker, StBemcTables* bemcTables);
  virtual ~StJetBEMCMuDst() { }

  TowerEnergyDepositList getEnergyList();

private:

  TowerEnergyDeposit readTowerHit(const StEmcRawHit& hit);

  StMuDstMaker* mMuDstMaker;

  StBemcTables* _bemcTables;

};

}

#endif // STJETBEMCMUDST_H

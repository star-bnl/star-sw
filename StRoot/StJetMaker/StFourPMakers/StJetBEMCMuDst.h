// -*- mode: c++;-*-
// $Id: StJetBEMCMuDst.h,v 1.2 2008/07/10 01:20:24 tai Exp $
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
  StJetBEMCMuDst(StMuDstMaker* uDstMaker, bool doTowerSwapFix = true);
  virtual ~StJetBEMCMuDst() { }

  TowerEnergyDepositList getEnergyList();

private:

  TowerEnergyDeposit readTowerHit(const StEmcRawHit& hit);

  StMuDstMaker* mMuDstMaker;

  StBemcTables* _bemcTables;

};

}

#endif // STJETBEMCMUDST_H

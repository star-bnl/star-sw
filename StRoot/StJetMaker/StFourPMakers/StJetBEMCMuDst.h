// -*- mode: c++;-*-
// $Id: StJetBEMCMuDst.h,v 1.4 2008/07/11 23:24:46 tai Exp $
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

  TowerEnergyList getEnergyList();

private:

  TowerEnergy readTowerHit(const StEmcRawHit& hit);

  StMuDstMaker* _uDstMaker;

  StBemcTables* _bemcTables;

};

}

#endif // STJETBEMCMUDST_H

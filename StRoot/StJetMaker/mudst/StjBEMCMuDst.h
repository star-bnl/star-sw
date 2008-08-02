// -*- mode: c++;-*-
// $Id: StjBEMCMuDst.h,v 1.1 2008/08/02 04:19:19 tai Exp $
#ifndef STJETBEMCMUDST_H
#define STJETBEMCMUDST_H

#include "StjBEMC.h"

class StEmcRawHit;
class StMuDstMaker;
class StBemcTables;

namespace StSpinJet {

class StJetBEMCMuDst : public StJetBEMC {

public:

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

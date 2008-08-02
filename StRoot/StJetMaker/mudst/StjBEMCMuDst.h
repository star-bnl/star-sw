// -*- mode: c++;-*-
// $Id: StjBEMCMuDst.h,v 1.3 2008/08/02 22:43:38 tai Exp $
#ifndef STJBEMCMUDST_H
#define STJBEMCMUDST_H

#include "StjBEMC.h"

class StEmcRawHit;
class StMuDstMaker;
class StBemcTables;

namespace StSpinJet {

class StjBEMCMuDst : public StjBEMC {

public:

  StjBEMCMuDst(StMuDstMaker* uDstMaker, bool doTowerSwapFix = true);
  virtual ~StjBEMCMuDst() { }

  StjTowerEnergyList getEnergyList();

private:

  StjTowerEnergy readTowerHit(const StEmcRawHit& hit);

  StMuDstMaker* _uDstMaker;

  StBemcTables* _bemcTables;

};

}

#endif // STJBEMCMUDST_H

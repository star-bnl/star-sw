// -*- mode: c++;-*-
// $Id: StjBEMCMuDst.h,v 1.2 2008/08/02 19:23:20 tai Exp $
#ifndef STJETBEMCMUDST_H
#define STJETBEMCMUDST_H

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

#endif // STJETBEMCMUDST_H

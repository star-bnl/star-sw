// -*- mode: c++;-*-
// $Id: StjBEMCMuDst.h,v 1.4 2008/08/03 00:29:02 tai Exp $
#ifndef STJBEMCMUDST_H
#define STJBEMCMUDST_H

#include "StjBEMC.h"

class StEmcRawHit;
class StMuDstMaker;
class StBemcTables;

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

#endif // STJBEMCMUDST_H

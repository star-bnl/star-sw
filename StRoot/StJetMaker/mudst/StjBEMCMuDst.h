// -*- mode: c++;-*-
// $Id: StjBEMCMuDst.h,v 1.5 2008/08/13 22:55:57 tai Exp $
#ifndef STJBEMCMUDST_H
#define STJBEMCMUDST_H

#include "StjBEMC.h"

class StEmcCollection;

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

  StEmcCollection* findEmcCollection();

  StMuDstMaker* _uDstMaker;

  StBemcTables* _bemcTables;

};

#endif // STJBEMCMUDST_H

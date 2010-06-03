// -*- mode: c++;-*-
// $Id: StjBEMCMuDst.h,v 1.10 2010/06/03 21:50:45 pibero Exp $
#ifndef STJBEMCMUDST_H
#define STJBEMCMUDST_H

#include "StjBEMC.h"

class StEmcCollection;

class StEmcRawHit;
class StBemcTables;

class StjBEMCMuDst : public StjBEMC {

public:

  StjBEMCMuDst(bool doTowerSwapFix = true);
  virtual ~StjBEMCMuDst() { }

  StjTowerEnergyList getEnergyList();

private:

  StjTowerEnergy readTowerHit(const StEmcRawHit& hit);

  StEmcCollection* findEmcCollection();

  StBemcTables* _bemcTables;

  static StjTowerEnergyList _list;

  static int _runNumber;
  static int _eventId;

  StjTowerEnergyList getlist();
  bool isNewEvent();

};

#endif // STJBEMCMUDST_H

// -*- mode: c++;-*-
// $Id: StjBEMCMuDst.h,v 1.7 2008/08/18 02:41:22 tai Exp $
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

  static StjTowerEnergyList _list;

  static int _runNumber;
  static int _eventId;

  StjTowerEnergyList getlist();
  bool isNewEvent();

};

#endif // STJBEMCMUDST_H

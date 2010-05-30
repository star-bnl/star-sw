// -*- mode: c++;-*-
// $Id: StjBEMCMuDst.h,v 1.8 2010/05/30 07:10:06 pibero Exp $
#ifndef STJBEMCMUDST_H
#define STJBEMCMUDST_H

#include "StjBEMC.h"

class StEmcCollection;

class StEmcRawHit;
class StEmcADCtoEMaker;
class StBemcTables;

class StjBEMCMuDst : public StjBEMC {

public:

  StjBEMCMuDst(bool doTowerSwapFix = true);
  virtual ~StjBEMCMuDst() { }

  StjTowerEnergyList getEnergyList();

private:

  StjTowerEnergy readTowerHit(const StEmcRawHit& hit);

  StEmcCollection* findEmcCollection();

  StEmcADCtoEMaker* _adc2e;

  StBemcTables* _bemcTables;

  static StjTowerEnergyList _list;

  static int _runNumber;
  static int _eventId;

  StjTowerEnergyList getlist();
  bool isNewEvent();
  bool isCorrupted() const;

};

#endif // STJBEMCMUDST_H

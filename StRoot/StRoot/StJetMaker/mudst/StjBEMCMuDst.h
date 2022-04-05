// -*- mode: c++;-*-
// $Id: StjBEMCMuDst.h,v 1.11 2010/09/08 04:54:30 pibero Exp $
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
  void setVertex(float vx, float vy, float vz);

private:

  StjTowerEnergy readTowerHit(const StEmcRawHit& hit);

  StEmcCollection* findEmcCollection();

  StBemcTables* _bemcTables;

  static StjTowerEnergyList _list;

  static int _runNumber;
  static int _eventId;

  StjTowerEnergyList getlist();
  bool isNewEvent();

  bool _setVertex;

  double _vx;
  double _vy;
  double _vz;

};

#endif // STJBEMCMUDST_H

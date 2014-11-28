// -*- mode: c++;-*-
// $Id: StjEEMCMuDst.h,v 1.7 2010/09/08 04:54:30 pibero Exp $
#ifndef STJEEMCMUDST_H
#define STJEEMCMUDST_H

#include "StjEEMC.h"

class StMuDstMaker;
class StEEmcDb;

class StjEEMCMuDst : public StjEEMC {

public:
  StjEEMCMuDst();
  virtual ~StjEEMCMuDst() { }

  void Init();

  StjTowerEnergyList getEnergyList();


private:

  StEEmcDb* mEeDb;

  bool _setVertex;

  double _vx;
  double _vy;
  double _vz;
};

#endif // STJEEMCMUDST_H

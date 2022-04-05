// -*- mode: c++;-*-
// $Id: StjEEMCMuDst.h,v 1.8 2016/01/06 22:00:17 gdwebb Exp $
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
  void setVertex(float vx, float vy, float vz)
  {
    _setVertex = true;
    _vx = vx;
    _vy = vy;
    _vz = vz;
  }
  StjTowerEnergyList getEnergyList();


private:

  StEEmcDb* mEeDb;

  bool _setVertex;

  double _vx;
  double _vy;
  double _vz;
};

#endif // STJEEMCMUDST_H

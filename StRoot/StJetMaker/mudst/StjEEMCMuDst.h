// -*- mode: c++;-*-
// $Id: StjEEMCMuDst.h,v 1.6 2010/05/30 07:10:06 pibero Exp $
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

};

#endif // STJEEMCMUDST_H

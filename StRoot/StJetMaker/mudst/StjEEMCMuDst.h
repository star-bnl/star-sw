// -*- mode: c++;-*-
// $Id: StjEEMCMuDst.h,v 1.5 2009/02/04 22:14:43 kocolosk Exp $
#ifndef STJEEMCMUDST_H
#define STJEEMCMUDST_H

#include "StjEEMC.h"

class StMuDstMaker;
class StEEmcDb;

class StjEEMCMuDst : public StjEEMC {

public:
  StjEEMCMuDst(StMuDstMaker* uDstMaker);
  virtual ~StjEEMCMuDst() { }

  void Init();

  StjTowerEnergyList getEnergyList();


private:

  StMuDstMaker* _uDstMaker;
  StEEmcDb* mEeDb;

};

#endif // STJEEMCMUDST_H

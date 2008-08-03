// -*- mode: c++;-*-
// $Id: StjEEMCMuDst.h,v 1.4 2008/08/03 00:29:03 tai Exp $
#ifndef STJEEMCMUDST_H
#define STJEEMCMUDST_H

#include "StjEEMC.h"

class StMuDstMaker;
class StEEmcDbMaker;

class StjEEMCMuDst : public StjEEMC {

public:
  StjEEMCMuDst(StMuDstMaker* uDstMaker);
  virtual ~StjEEMCMuDst() { }

  void Init();

  StjTowerEnergyList getEnergyList();


private:

  StMuDstMaker* _uDstMaker;
  StEEmcDbMaker* mEeDb;

};

#endif // STJEEMCMUDST_H

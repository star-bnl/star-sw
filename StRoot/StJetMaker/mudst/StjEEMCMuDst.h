// -*- mode: c++;-*-
// $Id: StjEEMCMuDst.h,v 1.2 2008/08/02 19:23:20 tai Exp $
#ifndef STJETEEMCMUDST_H
#define STJETEEMCMUDST_H

#include "StjEEMC.h"

class StMuDstMaker;
class StEEmcDbMaker;

namespace StSpinJet {

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

}

#endif // STJETEEMCMUDST_H

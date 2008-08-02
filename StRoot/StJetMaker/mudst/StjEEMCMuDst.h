// -*- mode: c++;-*-
// $Id: StjEEMCMuDst.h,v 1.3 2008/08/02 22:43:38 tai Exp $
#ifndef STJEEMCMUDST_H
#define STJEEMCMUDST_H

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

#endif // STJEEMCMUDST_H

// -*- mode: c++;-*-
// $Id: StjEEMCMuDst.h,v 1.1 2008/08/02 04:19:21 tai Exp $
#ifndef STJETEEMCMUDST_H
#define STJETEEMCMUDST_H

#include "StjEEMC.h"

class StMuDstMaker;
class StEEmcDbMaker;

namespace StSpinJet {

class StJetEEMCMuDst : public StJetEEMC {

public:
  StJetEEMCMuDst(StMuDstMaker* uDstMaker);
  virtual ~StJetEEMCMuDst() { }

  void Init();

  TowerEnergyList getEnergyList();


private:

  StMuDstMaker* _uDstMaker;
  StEEmcDbMaker* mEeDb;

};

}

#endif // STJETEEMCMUDST_H

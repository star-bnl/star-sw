// -*- mode: c++;-*-
// $Id: StJetEEMCMuDst.h,v 1.2 2008/07/10 01:56:09 tai Exp $
#ifndef STJETEEMCMUDST_H
#define STJETEEMCMUDST_H

#include "StJetEEMC.h"

class StMuDstMaker;
class StEEmcDbMaker;

namespace StSpinJet {

class StJetEEMCMuDst : public StJetEEMC {

public:
  StJetEEMCMuDst(StMuDstMaker* uDstMaker);
  virtual ~StJetEEMCMuDst() { }

  void Init();

  TowerEnergyDepositList getEnergyList();


private:

  StMuDstMaker* mMuDstMaker;
  StEEmcDbMaker* mEeDb;

};

}

#endif // STJETEEMCMUDST_H

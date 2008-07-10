// -*- mode: c++;-*-
// $Id: StJetEEMCMuDst.h,v 1.3 2008/07/10 20:15:22 tai Exp $
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

  TowerEnergyList getEnergyList();


private:

  StMuDstMaker* mMuDstMaker;
  StEEmcDbMaker* mEeDb;

};

}

#endif // STJETEEMCMUDST_H

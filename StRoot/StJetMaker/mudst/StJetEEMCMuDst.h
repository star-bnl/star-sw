// -*- mode: c++;-*-
// $Id: StJetEEMCMuDst.h,v 1.1 2008/07/21 17:25:15 tai Exp $
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

  StMuDstMaker* _uDstMaker;
  StEEmcDbMaker* mEeDb;

};

}

#endif // STJETEEMCMUDST_H

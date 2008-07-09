// -*- mode: c++;-*-
// $Id: StJetEEMCMuDst.h,v 1.1 2008/07/09 05:36:01 tai Exp $
#ifndef STJETEEMCMUDST_H
#define STJETEEMCMUDST_H

#include "StJetEEMC.h"

class StMuDstMaker;
class EEmcGeomSimple;
class StEEmcDbMaker;

namespace StSpinJet {

class StJetEEMCMuDst : public StJetEEMC {

public:
  StJetEEMCMuDst(StMuDstMaker* uDstMaker);
  virtual ~StJetEEMCMuDst() { }

  void Init(StEEmcDbMaker* eedb);

  TowerEnergyDepositList getEnergyList();


private:

  StMuDstMaker* mMuDstMaker;
  EEmcGeomSimple* mEeGeom;
  StEEmcDbMaker* mEeDb;

};

}

#endif // STJETEEMCMUDST_H

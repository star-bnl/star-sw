// -*- mode: c++;-*-
// $Id: StjTrgSoftMuDstEmcTriggerMakerFactory.h,v 1.1 2008/08/18 06:37:26 tai Exp $
#ifndef STJTRGMUDSTSOFTWAREEMCTRIGGERMAKERFACTORY_H
#define STJTRGMUDSTSOFTWAREEMCTRIGGERMAKERFACTORY_H

#include "StjTrgSoftFactory.h"

class StEmcTriggerMaker;
class StMuDstMaker;

class StjTrgBEMCJetPatchTowerIdMap;

class StjTrgSoftMuDstEmcTriggerMakerFactory : public StjTrgSoftFactory {

public:
  StjTrgSoftMuDstEmcTriggerMakerFactory(StEmcTriggerMaker* emcTrigMaker, StMuDstMaker* uDstMaker, StjTrgBEMCJetPatchTowerIdMap* bemcJpTowerMap)
    : _emcTrigMaker(emcTrigMaker), _uDstMaker(uDstMaker), _bemcJpTowerMap(bemcJpTowerMap) { }
  virtual ~StjTrgSoftMuDstEmcTriggerMakerFactory() { }

  StjTrgSoft* create();

private:
  StEmcTriggerMaker* _emcTrigMaker;
  StMuDstMaker* _uDstMaker;
  StjTrgBEMCJetPatchTowerIdMap* _bemcJpTowerMap;

  ClassDef(StjTrgSoftMuDstEmcTriggerMakerFactory, 1)

};


#endif // STJTRGMUDSTSOFTWAREEMCTRIGGERMAKERFACTORY_H

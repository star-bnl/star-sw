// -*- mode: c++;-*-
// $Id: StjTrgMuDstSoftwareEmcTriggerMakerFactory.h,v 1.2 2008/08/18 06:20:46 tai Exp $
#ifndef STJTRGMUDSTSOFTWAREEMCTRIGGERMAKERFACTORY_H
#define STJTRGMUDSTSOFTWAREEMCTRIGGERMAKERFACTORY_H

#include "StjTrgMuDstSoftwareFactory.h"

class StEmcTriggerMaker;
class StMuDstMaker;

class StjTrgBEMCJetPatchTowerIdMap;

class StjTrgMuDstSoftwareEmcTriggerMakerFactory : public StjTrgMuDstSoftwareFactory {

public:
  StjTrgMuDstSoftwareEmcTriggerMakerFactory(StEmcTriggerMaker* emcTrigMaker, StMuDstMaker* uDstMaker, StjTrgBEMCJetPatchTowerIdMap* bemcJpTowerMap)
    : _emcTrigMaker(emcTrigMaker), _uDstMaker(uDstMaker), _bemcJpTowerMap(bemcJpTowerMap) { }
  virtual ~StjTrgMuDstSoftwareEmcTriggerMakerFactory() { }

  StjTrgMuDstSoftware* create();

private:
  StEmcTriggerMaker* _emcTrigMaker;
  StMuDstMaker* _uDstMaker;
  StjTrgBEMCJetPatchTowerIdMap* _bemcJpTowerMap;

  ClassDef(StjTrgMuDstSoftwareEmcTriggerMakerFactory, 1)

};


#endif // STJTRGMUDSTSOFTWAREEMCTRIGGERMAKERFACTORY_H

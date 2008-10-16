// -*- mode: c++;-*-
// $Id: StjTrgBEMCJetPatchTowerIdMap2006.h,v 1.1 2008/10/16 19:47:50 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRGBEMCJETPATCHTOWERIDMAP2006_H
#define STJTRGBEMCJETPATCHTOWERIDMAP2006_H

#include "StjTrgBEMCJetPatchTowerIdMap.h"

class StjTrgBEMCJetPatchTowerIdMap2006 : public StjTrgBEMCJetPatchTowerIdMap {

public:
  StjTrgBEMCJetPatchTowerIdMap2006() { }
  virtual ~StjTrgBEMCJetPatchTowerIdMap2006() { }

  int getJetPatchIdForTower(int towerId);

private:

  ClassDef(StjTrgBEMCJetPatchTowerIdMap2006, 1)

};

#endif // STJTRGBEMCJETPATCHTOWERIDMAP2006_H

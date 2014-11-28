// -*- mode: c++;-*-
// $Id: StjTrgBEMCJetPatchTowerIdMap2005.h,v 1.1 2008/08/12 04:07:02 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRGBEMCJETPATCHTOWERIDMAP2005_H
#define STJTRGBEMCJETPATCHTOWERIDMAP2005_H

#include "StjTrgBEMCJetPatchTowerIdMap.h"

class StjTrgBEMCJetPatchTowerIdMap2005 : public StjTrgBEMCJetPatchTowerIdMap {

public:
  StjTrgBEMCJetPatchTowerIdMap2005() { }
  virtual ~StjTrgBEMCJetPatchTowerIdMap2005() { }

  int getJetPatchIdForTower(int towerId);

private:

  ClassDef(StjTrgBEMCJetPatchTowerIdMap2005, 1)

};

#endif // STJTRGBEMCJETPATCHTOWERIDMAP2005_H

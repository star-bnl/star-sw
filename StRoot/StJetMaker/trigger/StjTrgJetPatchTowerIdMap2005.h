// -*- mode: c++;-*-
// $Id: StjTrgJetPatchTowerIdMap2005.h,v 1.1 2008/08/12 04:01:45 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRGJETPATCHTOWERIDMAP2005_H
#define STJTRGJETPATCHTOWERIDMAP2005_H

#include "StjTrgJetPatchTowerIdMap.h"

class StjTrgJetPatchTowerIdMap2005 : public StjTrgJetPatchTowerIdMap {

public:
  StjTrgJetPatchTowerIdMap2005() { }
  virtual ~StjTrgJetPatchTowerIdMap2005() { }

  int getJetPatchIdForTower(int towerId);

private:

  ClassDef(StjTrgJetPatchTowerIdMap2005, 1)

};

#endif // STJTRGJETPATCHTOWERIDMAP2005_H

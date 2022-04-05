// -*- mode: c++;-*-
// $Id: StjTrgBEMCJetPatchTowerIdMap.h,v 1.1 2008/08/12 04:07:01 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRGBEMCJETPATCHTOWERIDMAP_H
#define STJTRGBEMCJETPATCHTOWERIDMAP_H

#include <TObject.h>

class StjTrgBEMCJetPatchTowerIdMap : public TObject {

public:
  StjTrgBEMCJetPatchTowerIdMap() { }
  virtual ~StjTrgBEMCJetPatchTowerIdMap() { }

  virtual int getJetPatchIdForTower(int towerId) = 0;

private:

  ClassDef(StjTrgBEMCJetPatchTowerIdMap, 1)

};

#endif // STJTRGBEMCJETPATCHTOWERIDMAP_H

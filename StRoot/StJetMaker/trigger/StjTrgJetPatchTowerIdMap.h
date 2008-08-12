// -*- mode: c++;-*-
// $Id: StjTrgJetPatchTowerIdMap.h,v 1.1 2008/08/12 04:01:44 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRGJETPATCHTOWERIDMAP_H
#define STJTRGJETPATCHTOWERIDMAP_H

#include <TObject.h>

class StjTrgJetPatchTowerIdMap : public TObject {

public:
  StjTrgJetPatchTowerIdMap() { }
  virtual ~StjTrgJetPatchTowerIdMap() { }

  virtual int getJetPatchIdForTower(int towerId) = 0;

private:

  ClassDef(StjTrgJetPatchTowerIdMap, 1)

};

#endif // STJTRGJETPATCHTOWERIDMAP_H

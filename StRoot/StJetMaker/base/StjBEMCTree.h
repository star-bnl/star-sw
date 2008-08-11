// -*- mode: c++;-*-
// $Id: StjBEMCTree.h,v 1.6 2008/08/11 03:50:42 tai Exp $
#ifndef STJBEMCTREE_H
#define STJBEMCTREE_H

#include "StjBEMC.h"

class StjTowerEnergyListReader;

class StjBEMCTree : public StjBEMC {

public:
  StjBEMCTree(StjTowerEnergyListReader* reader)
    : _reader(reader) { }
  virtual ~StjBEMCTree() { }

  StjTowerEnergyList getEnergyList();

private:

  StjTowerEnergyListReader* _reader;

  ClassDef(StjBEMCTree, 1)

};

#endif // STJBEMCTREE_H

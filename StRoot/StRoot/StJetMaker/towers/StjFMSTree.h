// -*- mode: c++;-*-
// $Id: StjFMSTree.h,v 1.1 2017/05/22 19:36:06 zchang Exp $
#ifndef STJFMSTREE_H
#define STJFMSTREE_H

#include "StjFMS.h"

class StjTowerEnergyListReader;

class StjFMSTree : public StjFMS {

public:
  StjFMSTree(StjTowerEnergyListReader* reader)
    : _reader(reader) { }
  virtual ~StjFMSTree() { }

  StjTowerEnergyList getEnergyList();

private:

  StjTowerEnergyListReader* _reader;

  ClassDef(StjFMSTree, 1)

};

#endif // STJFMSTREE_H

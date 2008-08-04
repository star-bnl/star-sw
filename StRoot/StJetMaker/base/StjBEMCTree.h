// -*- mode: c++;-*-
// $Id: StjBEMCTree.h,v 1.5 2008/08/04 00:55:27 tai Exp $
#ifndef STJBEMCTREE_H
#define STJBEMCTREE_H

#include "StjBEMC.h"
#include <Rtypes.h>

class TTree;

class StjTowerEnergyListReader;

class StjBEMCTree : public StjBEMC {

public:
  StjBEMCTree(TTree *tree,
		const Int_t& indexMajor, const Int_t& indexMinor,
		const char* indexMajorName = "runNumber",
		const char* indexMinorName = "eventId"
		);
  virtual ~StjBEMCTree() { }

  StjTowerEnergyList getEnergyList();

private:

  TTree* _tree;

  const Int_t& _indexMajor;
  const Int_t& _indexMinor;

  StjTowerEnergyListReader* _reader;

  ClassDef(StjBEMCTree, 1)

};

#endif // STJBEMCTREE_H

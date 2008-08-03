// -*- mode: c++;-*-
// $Id: StjBEMCTree.h,v 1.4 2008/08/03 00:26:26 tai Exp $
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

};

#endif // STJBEMCTREE_H

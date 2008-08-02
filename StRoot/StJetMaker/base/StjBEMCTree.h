// -*- mode: c++;-*-
// $Id: StjBEMCTree.h,v 1.3 2008/08/02 22:43:15 tai Exp $
#ifndef STJBEMCTREE_H
#define STJBEMCTREE_H

#include "StjBEMC.h"
#include <Rtypes.h>

class TTree;

class StjTowerEnergyListReader;

namespace StSpinJet {

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

}

#endif // STJBEMCTREE_H

// -*- mode: c++;-*-
// $Id: StjBEMCTree.h,v 1.2 2008/08/02 19:22:41 tai Exp $
#ifndef STJETBEMCTREE_H
#define STJETBEMCTREE_H

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

#endif // STJETBEMCTREE_H

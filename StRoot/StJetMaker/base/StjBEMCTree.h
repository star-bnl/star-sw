// -*- mode: c++;-*-
// $Id: StjBEMCTree.h,v 1.1 2008/08/02 04:15:13 tai Exp $
#ifndef STJETBEMCTREE_H
#define STJETBEMCTREE_H

#include "StjBEMC.h"
#include <Rtypes.h>

class TTree;

class StJetTowerEnergyListReader;

namespace StSpinJet {

class StJetBEMCTree : public StJetBEMC {

public:
  StJetBEMCTree(TTree *tree,
		const Int_t& indexMajor, const Int_t& indexMinor,
		const char* indexMajorName = "runNumber",
		const char* indexMinorName = "eventId"
		);
  virtual ~StJetBEMCTree() { }

  TowerEnergyList getEnergyList();

private:

  TTree* _tree;

  const Int_t& _indexMajor;
  const Int_t& _indexMinor;

  StJetTowerEnergyListReader* _reader;

};

}

#endif // STJETBEMCTREE_H

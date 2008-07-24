// -*- mode: c++;-*-
// $Id: StJetBEMCTree.h,v 1.2 2008/07/24 20:57:08 tai Exp $
#ifndef STJETBEMCTREE_H
#define STJETBEMCTREE_H

#include "StJetBEMC.h"
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

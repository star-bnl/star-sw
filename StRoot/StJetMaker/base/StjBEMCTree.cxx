// $Id: StjBEMCTree.cxx,v 1.1 2008/08/02 04:15:10 tai Exp $
#include "StjBEMCTree.h"

#include "StjTowerEnergyListReader.h"

#include <TTree.h>

namespace StSpinJet {

StJetBEMCTree::StJetBEMCTree(TTree *tree,
			       const Int_t& indexMajor, const Int_t& indexMinor,
			       const char* indexMajorName, const char* indexMinorName
			       )
 : _tree(tree)
 , _indexMajor(indexMajor), _indexMinor(indexMinor)
 {
  _tree->BuildIndex(indexMajorName, indexMinorName);
  _reader = new StJetTowerEnergyListReader(_tree);
 }


TowerEnergyList StJetBEMCTree::getEnergyList()
{
  Long64_t entry = _tree->GetEntryNumberWithIndex(_indexMajor, _indexMinor);
  return _reader->GetEntry(entry);
}

}

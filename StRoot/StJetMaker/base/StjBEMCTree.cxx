// $Id: StjBEMCTree.cxx,v 1.4 2008/08/04 00:55:26 tai Exp $
#include "StjBEMCTree.h"

#include "StjTowerEnergyListReader.h"

#include <TTree.h>

ClassImp(StjBEMCTree)

StjBEMCTree::StjBEMCTree(TTree *tree,
			       const Int_t& indexMajor, const Int_t& indexMinor,
			       const char* indexMajorName, const char* indexMinorName
			       )
 : _tree(tree)
 , _indexMajor(indexMajor), _indexMinor(indexMinor)
 {
  _tree->BuildIndex(indexMajorName, indexMinorName);
  _reader = new StjTowerEnergyListReader(_tree);
 }


StjTowerEnergyList StjBEMCTree::getEnergyList()
{
  Long64_t entry = _tree->GetEntryNumberWithIndex(_indexMajor, _indexMinor);
  return _reader->GetEntry(entry);
}

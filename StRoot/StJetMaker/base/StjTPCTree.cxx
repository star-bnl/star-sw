// $Id: StjTPCTree.cxx,v 1.2 2008/08/02 19:22:49 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTPCTree.h"

#include "StjTrackListReader.h"

#include <TTree.h>

namespace StSpinJet {

StjTPCTree::StjTPCTree(TTree *tree,
	       const Int_t& indexMajor, const Int_t& indexMinor,
	       const char* indexMajorName, const char* indexMinorName
	       )
 : _tree(tree)
 , _indexMajor(indexMajor), _indexMinor(indexMinor)
{
  _tree->BuildIndex(indexMajorName, indexMinorName);
  _reader = new StjTrackListReader(_tree);
}

StjTrackList StjTPCTree::getTrackList()
{
  Long64_t entry = _tree->GetEntryNumberWithIndex(_indexMajor, _indexMinor);
  return _reader->GetEntry(entry);
}

}

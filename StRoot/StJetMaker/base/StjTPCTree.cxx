// $Id: StjTPCTree.cxx,v 1.1 2008/08/02 04:15:42 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTPCTree.h"

#include "StjTrackListReader.h"

#include <TTree.h>

namespace StSpinJet {

StJetTPCTree::StJetTPCTree(TTree *tree,
	       const Int_t& indexMajor, const Int_t& indexMinor,
	       const char* indexMajorName, const char* indexMinorName
	       )
 : _tree(tree)
 , _indexMajor(indexMajor), _indexMinor(indexMinor)
{
  _tree->BuildIndex(indexMajorName, indexMinorName);
  _reader = new StJetTrackListReader(_tree);
}

TrackList StJetTPCTree::getTrackList()
{
  Long64_t entry = _tree->GetEntryNumberWithIndex(_indexMajor, _indexMinor);
  return _reader->GetEntry(entry);
}

}

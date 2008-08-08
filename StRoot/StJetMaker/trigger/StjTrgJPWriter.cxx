// $Id: StjTrgJPWriter.cxx,v 1.2 2008/08/08 21:16:42 tai Exp $
#include "StjTrgJPWriter.h"

#include "StjTrgMuDst.h"

#include <TTree.h>

#include <vector>

using namespace std;

void StjTrgJPWriter::createBranch_trgSpecific(TTree* tree)
{
  tree->Branch("nJetPatches", &_nJetPatches  , "nJetPatches/I"  );
  tree->Branch("jetPatchId" ,  _jetPatchId   , "jetPatchId[nJetPatches]/I");
}

void StjTrgJPWriter::fillBranch_trgSpecific()
{
  vector<int> jps = _trg->jetPatches();

  _nJetPatches = jps.size();

  for(int i = 0; i < _nJetPatches; ++i) {
    _jetPatchId[i] = jps[i];
  }
}

// $Id: StJetTrgJPWriter.cxx,v 1.7 2008/07/24 02:14:48 tai Exp $
#include "StJetTrgJPWriter.h"

#include "StJetTrg.h"

#include <TTree.h>

#include <vector>

using namespace std;

void StJetTrgJPWriter::createBranch_trgSpecific(TTree* tree)
{
  tree->Branch("nJetPatches", &_nJetPatches  , "nJetPatches/I"  );
  tree->Branch("jetPatchId" ,  _jetPatchId   , "jetPatchId[nJetPatches]/I");
}

void StJetTrgJPWriter::fillBranch_trgSpecific()
{
  vector<int> jps = _trg->jetPatches();

  _nJetPatches = jps.size();

  for(int i = 0; i < _nJetPatches; ++i) {
    _jetPatchId[i] = jps[i];
  }
}

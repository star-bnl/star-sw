// $Id: StjTrgHTWriter.cxx,v 1.1 2008/08/02 04:06:26 tai Exp $
#include "StjTrgHTWriter.h"

#include "StjTrg.h"

#include <TTree.h>

#include <vector>

using namespace std;

void StJetTrgHTWriter::createBranch_trgSpecific(TTree* tree)
{
  tree->Branch("nTowers"    , &_nTowers      , "nTowers/I"      );
  tree->Branch("towerId"    ,  _towerId      , "towerId[nTowers]/I");
}

void StJetTrgHTWriter::fillBranch_trgSpecific()
{
  vector<int> towers = _trg->towers();

  _nTowers = towers.size();

  for(int i = 0; i < _nTowers; ++i) {
    _towerId[i] = towers[i];
  }
}

// $Id: StjTrgHTWriter.cxx,v 1.3 2008/08/10 23:04:57 tai Exp $
#include "StjTrgHTWriter.h"

#include "StjTrg.h"

#include <TTree.h>

#include <vector>

using namespace std;

void StjTrgHTWriter::createBranch_trgSpecific(TTree* tree)
{
  tree->Branch("nTowers"    , &_nTowers      , "nTowers/I"      );
  tree->Branch("towerId"    ,  _towerId      , "towerId[nTowers]/I");
}

void StjTrgHTWriter::fillBranch_trgSpecific()
{
  vector<int> towers = _trg->towers();

  _nTowers = towers.size();

  for(int i = 0; i < _nTowers; ++i) {
    _towerId[i] = towers[i];
  }
}

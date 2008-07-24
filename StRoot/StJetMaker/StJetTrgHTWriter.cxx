// $Id: StJetTrgHTWriter.cxx,v 1.7 2008/07/24 02:14:48 tai Exp $
#include "StJetTrgHTWriter.h"

#include "StJetTrg.h"

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

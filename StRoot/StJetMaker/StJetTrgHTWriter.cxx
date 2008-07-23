// $Id: StJetTrgHTWriter.cxx,v 1.6 2008/07/23 23:21:03 tai Exp $
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
  vector<int> towers = _trg->towers(_trgId);

  _nTowers = towers.size();

  for(int i = 0; i < _nTowers; ++i) {
    _towerId[i] = towers[i];
  }
}

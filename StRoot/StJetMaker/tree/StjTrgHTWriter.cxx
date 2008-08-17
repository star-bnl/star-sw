// $Id: StjTrgHTWriter.cxx,v 1.2 2008/08/17 11:29:18 tai Exp $
#include "StjTrgHTWriter.h"

#include "StjTrg.h"

#include <TTree.h>

#include <vector>

using namespace std;

void StjTrgHTWriter::createBranch_trgSpecific(TTree* tree)
{
  tree->Branch("nTowers"     , &_nTowers      , "nTowers/I"      );
  tree->Branch("towerId"     ,  _towerId      , "towerId[nTowers]/I");
  tree->Branch("towerDsmAdc" ,  _towerDsmAdc  , "towerDsmAdc[nTowers]/I");
  tree->Branch("towerAdc"    ,  _towerAdc     , "towerAdc[nTowers]/i");
  tree->Branch("towerEnergy" ,  _towerEnergy  , "towerEnergy[nTowers]/D");
}

void StjTrgHTWriter::fillBranch_trgSpecific()
{
  vector<int> towers            = _trg->towers();
  vector<int> towerDsmAdc       = _trg->towerDsmAdc();
  vector<unsigned int> towerAdc = _trg->towerAdc();
  vector<double> towerEnergy    = _trg->towerEnergy();

  _nTowers = towers.size();

  for(int i = 0; i < _nTowers; ++i) {
    _towerId[i] = towers[i];
    _towerDsmAdc[i] = towerDsmAdc[i];
    _towerAdc[i] = towerAdc[i];
    _towerEnergy[i] = towerEnergy[i];
  }
}

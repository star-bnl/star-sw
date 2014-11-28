// $Id: StjTrgHTWriter.cxx,v 1.4 2008/09/21 19:11:40 tai Exp $
#include "StjTrgHTWriter.h"

#include "StjTrg.h"

#include <TTree.h>

#include <vector>

ClassImp(StjTrgHTWriter)

using namespace std;

void StjTrgHTWriter::createBranch_trgSpecific(TTree* tree)
{
  tree->Branch("nTowers"     , &_nTowers      , "nTowers/I"      );
  tree->Branch("towerId"     ,  _towerId      , "towerId[nTowers]/I");
  tree->Branch("towerEt"     ,  _towerEt      , "towerEt[nTowers]/D");
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
  vector<double> towerEt        = _trg->towerEt();

  _nTowers = towers.size();

  for(int i = 0; i < _nTowers; ++i) {
    _towerId[i]     = towers[i];
    _towerDsmAdc[i] = towerDsmAdc[i];
    _towerAdc[i]    = towerAdc[i];
    _towerEnergy[i] = towerEnergy[i];
    _towerEt[i]     = towerEt[i];
  }
}

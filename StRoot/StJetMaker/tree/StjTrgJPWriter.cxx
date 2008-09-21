// $Id: StjTrgJPWriter.cxx,v 1.4 2008/09/21 19:11:40 tai Exp $
#include "StjTrgJPWriter.h"

#include "StjTrg.h"

#include <TTree.h>

#include <vector>

ClassImp(StjTrgJPWriter)

using namespace std;

void StjTrgJPWriter::createBranch_trgSpecific(TTree* tree)
{
  tree->Branch("nJetPatches"    , &_nJetPatches     , "nJetPatches/I"  );
  tree->Branch("jetPatchId"     ,  _jetPatchId      , "jetPatchId[nJetPatches]/I");
  tree->Branch("jetPatchEt"     ,  _jetPatchEt      , "jetPatchEt[nJetPatches]/D");
  tree->Branch("jetPatchDsmAdc" ,  _jetPatchDsmAdc  , "jetPatchDsmAdc[nJetPatches]/I");
  tree->Branch("jetPatchAdc"    ,  _jetPatchAdc     , "jetPatchAdc[nJetPatches]/i");
  tree->Branch("jetPatchEnergy" ,  _jetPatchEnergy  , "jetPatchEnergy[nJetPatches]/D");
}

void StjTrgJPWriter::fillBranch_trgSpecific()
{
  vector<int> jps = _trg->jetPatches();
  vector<int> jetPatchDsmAdc       = _trg->jetPatchDsmAdc();
  vector<unsigned int> jetPatchAdc = _trg->jetPatchAdc();
  vector<double> jetPatchEnergy    = _trg->jetPatchEnergy();
  vector<double> jetPatchEt        = _trg->jetPatchEt();

  _nJetPatches = jps.size();

  for(int i = 0; i < _nJetPatches; ++i) {
    _jetPatchId[i]     = jps[i];
    _jetPatchDsmAdc[i] = jetPatchDsmAdc[i];
    _jetPatchAdc[i]    = jetPatchAdc[i];
    _jetPatchEnergy[i] = jetPatchEnergy[i];
    _jetPatchEt[i]     = jetPatchEt[i];
  }
}

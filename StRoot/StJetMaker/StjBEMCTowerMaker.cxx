// $Id: StjBEMCTowerMaker.cxx,v 1.1 2008/08/02 04:04:09 tai Exp $
#include "StjBEMCTowerMaker.h"


#include "StjTowerEnergyCut2003BemcTower.h"
#include "StjTowerEnergyCutBemcWestOnly.h"
#include "StjTowerEnergyCutEnergy.h"
#include "StjTowerEnergyCutBemcStatus.h"
#include "StjTowerEnergyCutAdc.h"

#include "StjTowerEnergyListWriter.h"

#include "StjBEMCMuDst.h"
#include "StjTowerEnergyListCut.h"
#include "StjBEMCTxt.h"

#include <TDirectory.h>

#include <iostream>

using namespace std;
using namespace StSpinJet;
using namespace StJetTowerEnergyCut;

ClassImp(StJetBEMCTowerMaker)
  

StJetBEMCTowerMaker::StJetBEMCTowerMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker)
  : StMaker(name)
  , _file(file)
  , _uDstMaker(uDstMaker)
{ }

Int_t StJetBEMCTowerMaker::Init()
{
  _bemc = new StJetBEMCMuDst(_uDstMaker, true);
  // _bemc = new StJetBEMCNull();
  // _bemc = new StJetBEMCTxt("./testStJetMaker/bemcenergy.txt");

  _bemcCut = new StJetBEMCEnergyCut();
  _bemcCut->addCut(new TowerEnergyCutBemcWestOnly());
  _bemcCut->addCut(new TowerEnergyCutEnergy());
  _bemcCut->addCut(new TowerEnergyCutBemcStatus());
  _bemcCut->addCut(new TowerEnergyCutAdc());

  _writer = new StJetTowerEnergyListWriter("bemcTowers", _file);

  return kStOk;
}

Int_t StJetBEMCTowerMaker::Make()
{
  TowerEnergyList energyList = _bemc->getEnergyList();

  energyList = (*_bemcCut)(energyList);

  _writer->Fill(energyList);

  return kStOk;

}

Int_t StJetBEMCTowerMaker::Finish()
{
  _writer->Finish();

  return kStOk;
}

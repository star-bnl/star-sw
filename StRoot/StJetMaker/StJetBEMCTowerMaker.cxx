// $Id: StJetBEMCTowerMaker.cxx,v 1.9 2008/07/24 20:57:02 tai Exp $
#include "StJetBEMCTowerMaker.h"


#include "TowerEnergyCut2003BemcTower.h"
#include "TowerEnergyCutBemcWestOnly.h"
#include "TowerEnergyCutEnergy.h"
#include "TowerEnergyCutBemcStatus.h"
#include "TowerEnergyCutAdc.h"

#include "StJetTowerEnergyListWriter.h"

#include "StJetBEMCMuDst.h"
#include "StJetBEMCEnergyCut.h"
#include "StJetBEMCTxt.h"

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

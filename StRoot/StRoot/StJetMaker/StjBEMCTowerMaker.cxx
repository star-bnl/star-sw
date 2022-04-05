// $Id: StjBEMCTowerMaker.cxx,v 1.4 2010/05/30 07:10:00 pibero Exp $
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

ClassImp(StjBEMCTowerMaker)
  

StjBEMCTowerMaker::StjBEMCTowerMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker)
  : StMaker(name)
  , _file(file)
  , _uDstMaker(uDstMaker)
{ }

Int_t StjBEMCTowerMaker::Init()
{
  _bemc = new StjBEMCMuDst;
  // _bemc = new StjBEMCNull();
  // _bemc = new StjBEMCTxt("./testStJetMaker/bemcenergy.txt");

  _bemcCut = new StjTowerEnergyListCut();
  _bemcCut->addCut(new StjTowerEnergyCutBemcWestOnly());
  _bemcCut->addCut(new StjTowerEnergyCutEnergy());
  _bemcCut->addCut(new StjTowerEnergyCutBemcStatus());
  _bemcCut->addCut(new StjTowerEnergyCutAdc());

  _writer = new StjTowerEnergyListWriter("bemcTowers", _file);

  return kStOk;
}

Int_t StjBEMCTowerMaker::Make()
{
  StjTowerEnergyList energyList = _bemc->getEnergyList();

  energyList = (*_bemcCut)(energyList);

  _writer->Fill(energyList);

  return kStOk;

}

Int_t StjBEMCTowerMaker::Finish()
{
  _writer->Finish();

  return kStOk;
}

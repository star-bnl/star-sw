// $Id: StJetBEMCTowerMaker.cxx,v 1.1 2008/07/21 02:00:23 tai Exp $
#include "StJetBEMCTowerMaker.h"

#include "StJetTPCMuDst.h"
#include "StJetBEMCMuDst.h"
#include "StJetEEMCMuDst.h"
#include "StJetBEMCEnergyCut.h"

#include "TowerEnergyCut2003BemcTower.h"
#include "TowerEnergyCutBemcWestOnly.h"
#include "TowerEnergyCutEnergy.h"
#include "TowerEnergyCutBemcStatus.h"
#include "TowerEnergyCutAdc.h"

#include "StJetBEMCTxt.h"

#include <TFile.h>
#include <TTree.h>

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

  _file->cd();
  _tree = new TTree("bemcTowers", "bemcTowers");
  _tree->SetAutoSave(kMaxLong64);
  _tree->SetMaxTreeSize(kMaxLong64);

  _tree->Branch("eventId"    , &_eventId      , "eventId/I"      );
  _tree->Branch("nTowers"    , &_nTowers      , "nTowers/I"      );
  _tree->Branch("energy"     ,  _energy       , "energy[nTowers]/D"     );     
  _tree->Branch("towerId"    ,  _towerId      , "towerId[nTowers]/I"    );    
  _tree->Branch("towerEta"   ,  _towerEta     , "towerEta[nTowers]/D"   );     
  _tree->Branch("towerPhi"   ,  _towerPhi     , "towerPhi[nTowers]/D"   );     
  _tree->Branch("adc"        ,  _adc          , "adc[nTowers]/i"        );	            
  _tree->Branch("pedestal"   ,  _pedestal     , "pedestal[nTowers]/D"   );   
  _tree->Branch("rms"        ,  _rms          ,	"rms[nTowers]/D"        );	            
  _tree->Branch("towerR"     ,  _towerR       , "towerR[nTowers]/D"     );     
  _tree->Branch("vertexX"    ,  _vertexX      , "vertexX[nTowers]/D"    );    
  _tree->Branch("vertexY"    ,  _vertexY      , "vertexY[nTowers]/D"    );    
  _tree->Branch("vertexZ"    ,  _vertexZ      , "vertexZ[nTowers]/D"    );    
  _tree->Branch("status"     ,  _status       , "status[nTowers]/I"     );      
  _tree->Branch("detectorId" , &_detectorId   , "detectorId/I" ); 
  _tree->Branch("runNumber"  , &_runNumber    , "runNumber/I"    );

  return kStOk;
}

Int_t StJetBEMCTowerMaker::Make()
{
  TowerEnergyList energyList = _bemc->getEnergyList();

  energyList = (*_bemcCut)(energyList);

  if(energyList.empty()) return kStOk;

  _runNumber = energyList[0].runNumber;
  _eventId = energyList[0].eventId;
  _detectorId = energyList[0].detectorId;

  _nTowers = energyList.size();
  for(int i = 0; i < _nTowers; ++i) {
    const TowerEnergy& tower = energyList[i];
    _towerId[i]      =	tower.towerId;
    _towerR[i]       =	tower.towerR;
    _towerEta[i]     =	tower.towerEta;
    _towerPhi[i]     =	tower.towerPhi;
    _vertexX[i]      =	tower.vertexX;
    _vertexY[i]      =	tower.vertexY;
    _vertexZ[i]      =	tower.vertexZ;
    _energy[i]       =	tower.energy;
    _adc[i]          =	tower.adc;
    _pedestal[i]     =	tower.pedestal;
    _rms[i]          =	tower.rms;
    _status[i]       =  tower.status;
  }

  _tree->Fill();

  return kStOk;

}

Int_t StJetBEMCTowerMaker::Finish()
{
  _tree->BuildIndex("runNumber", "eventId");

  return kStOk;
}

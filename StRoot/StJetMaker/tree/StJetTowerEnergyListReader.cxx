// $Id: StJetTowerEnergyListReader.cxx,v 1.1 2008/07/24 20:57:13 tai Exp $
#include "StJetTowerEnergyListReader.h"

#include <TTree.h>

using namespace StSpinJet;

StJetTowerEnergyListReader::StJetTowerEnergyListReader(TTree *tree)
 : _tree(tree)
 {
  _tree->SetBranchAddress("eventId"    , &_eventId      );
  _tree->SetBranchAddress("nTowers"    , &_nTowers      );
  _tree->SetBranchAddress("energy"     ,  _energy       );
  _tree->SetBranchAddress("towerId"    ,  _towerId      );
  _tree->SetBranchAddress("towerEta"   ,  _towerEta     );
  _tree->SetBranchAddress("towerPhi"   ,  _towerPhi     );
  _tree->SetBranchAddress("adc"        ,  _adc          );
  _tree->SetBranchAddress("pedestal"   ,  _pedestal     );
  _tree->SetBranchAddress("rms"        ,  _rms          );
  _tree->SetBranchAddress("towerR"     ,  _towerR       );
  _tree->SetBranchAddress("vertexX"    ,  _vertexX      );
  _tree->SetBranchAddress("vertexY"    ,  _vertexY      );
  _tree->SetBranchAddress("vertexZ"    ,  _vertexZ      );
  _tree->SetBranchAddress("status"     ,  _status       );
  _tree->SetBranchAddress("detectorId" , &_detectorId   );
  _tree->SetBranchAddress("runNumber"  , &_runNumber    );
 }


TowerEnergyList StJetTowerEnergyListReader::GetEntry(Long64_t entry)
{
  TowerEnergyList ret;

  if(entry < 0) return ret;

  if(_tree->GetEntry(entry) <= 0) return ret;

  for(int i = 0; i < _nTowers; ++i) {

    TowerEnergy energy;

    energy.runNumber  =  _runNumber;             
    energy.eventId    =  _eventId;             
    energy.detectorId =  _detectorId; // 9: BEMC, 13: EEMC          
    energy.towerId    =  _towerId[i];              
    energy.towerR     =  _towerR[i];              
    energy.towerEta   =  _towerEta[i];              
    energy.towerPhi   =  _towerPhi[i];              
    energy.vertexX    =  _vertexX[i];              
    energy.vertexY    =  _vertexY[i];              
    energy.vertexZ    =  _vertexZ[i];              
    energy.energy     =  _energy[i];              
    energy.adc        =  _adc[i];       
    energy.pedestal   =  _pedestal[i];                    
    energy.rms        =  _rms[i];       
    energy.status    =  _status[i];     // 1 is good for BEMC. 0 is good for EEMC      
   
    ret.push_back(energy);
  }

  return ret;
}

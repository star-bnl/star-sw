// $Id: StjTowerEnergyListReader.cxx,v 1.5 2008/08/11 03:50:59 tai Exp $
#include "StjTowerEnergyListReader.h"

#include <TTree.h>

ClassImp(StjTowerEnergyListReader)

void StjTowerEnergyListReader::SetBranchAddress(TTree *tree)
 {
  tree->SetBranchAddress("eventId"    , &_eventId      );
  tree->SetBranchAddress("nTowers"    , &_nTowers      );
  tree->SetBranchAddress("energy"     ,  _energy       );
  tree->SetBranchAddress("towerId"    ,  _towerId      );
  tree->SetBranchAddress("towerEta"   ,  _towerEta     );
  tree->SetBranchAddress("towerPhi"   ,  _towerPhi     );
  tree->SetBranchAddress("adc"        ,  _adc          );
  tree->SetBranchAddress("pedestal"   ,  _pedestal     );
  tree->SetBranchAddress("rms"        ,  _rms          );
  tree->SetBranchAddress("towerR"     ,  _towerR       );
  tree->SetBranchAddress("vertexX"    , &_vertexX      );
  tree->SetBranchAddress("vertexY"    , &_vertexY      );
  tree->SetBranchAddress("vertexZ"    , &_vertexZ      );
  tree->SetBranchAddress("status"     ,  _status       );
  tree->SetBranchAddress("detectorId" , &_detectorId   );
  tree->SetBranchAddress("runNumber"  , &_runNumber    );
 }

void StjTowerEnergyListReader::clearEntry()
{
  _list.clear();
}

void StjTowerEnergyListReader::readEntry()
{
  clearEntry();

  for(int i = 0; i < _nTowers; ++i) {

    StjTowerEnergy energy;

    energy.runNumber  =  _runNumber;             
    energy.eventId    =  _eventId;             
    energy.detectorId =  _detectorId; // 9: BEMC, 13: EEMC          
    energy.towerId    =  _towerId[i];              
    energy.towerR     =  _towerR[i];              
    energy.towerEta   =  _towerEta[i];              
    energy.towerPhi   =  _towerPhi[i];              
    energy.vertexX    =  _vertexX;              
    energy.vertexY    =  _vertexY;              
    energy.vertexZ    =  _vertexZ;              
    energy.energy     =  _energy[i];              
    energy.adc        =  _adc[i];       
    energy.pedestal   =  _pedestal[i];                    
    energy.rms        =  _rms[i];       
    energy.status    =  _status[i];     // 1 is good for BEMC. 0 is good for EEMC      
   
    _list.push_back(energy);
  }

}

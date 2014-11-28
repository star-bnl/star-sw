// $Id: StjTowerEnergyListWriter.cxx,v 1.4 2008/08/04 06:10:46 tai Exp $
#include "StjTowerEnergyListWriter.h"

#include <TDirectory.h>
#include <TTree.h>

#include <iostream>

ClassImp(StjTowerEnergyListWriter)

using namespace std;

StjTowerEnergyListWriter::StjTowerEnergyListWriter(const char* treeName, TDirectory* file)
  : _file(file)
{
  _file->cd();
  _tree = new TTree(treeName, treeName);
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
  _tree->Branch("vertexX"    , &_vertexX      , "vertexX/D"    );    
  _tree->Branch("vertexY"    , &_vertexY      , "vertexY/D"    );    
  _tree->Branch("vertexZ"    , &_vertexZ      , "vertexZ/D"    );    
  _tree->Branch("status"     ,  _status       , "status[nTowers]/I"     );      
  _tree->Branch("detectorId" , &_detectorId   , "detectorId/I" ); 
  _tree->Branch("runNumber"  , &_runNumber    , "runNumber/I"    );
}

void StjTowerEnergyListWriter::Fill(const StjTowerEnergyList& energyList)
{
  if(energyList.empty()) return;

  _runNumber  = energyList[0].runNumber;
  _eventId    = energyList[0].eventId;
  _detectorId = energyList[0].detectorId;

  _vertexX    = energyList[0].vertexX;
  _vertexY    = energyList[0].vertexY;
  _vertexZ    = energyList[0].vertexZ;

  _nTowers = energyList.size();
  for(int i = 0; i < _nTowers; ++i) {
    const StjTowerEnergy& tower = energyList[i];
    _towerId[i]      =	tower.towerId;
    _towerR[i]       =	tower.towerR;
    _towerEta[i]     =	tower.towerEta;
    _towerPhi[i]     =	tower.towerPhi;
    _energy[i]       =	tower.energy;
    _adc[i]          =	tower.adc;
    _pedestal[i]     =	tower.pedestal;
    _rms[i]          =	tower.rms;
    _status[i]       =  tower.status;
  }

  _tree->Fill();
}

void StjTowerEnergyListWriter::Finish()
{
  _tree->BuildIndex("runNumber", "eventId");
}

#include "StjFCSMuDst.h"

#include "StMaker.h"
#include "StChain.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"

#include "StMuDSTMaker/COMMON/StMuFcsCollection.h"
#include "StMuDSTMaker/COMMON/StMuFcsHit.h"

#include "StFcsDbMaker/StFcsDbMaker.h"
#include "StFcsDbMaker/StFcsDb.h"
#include "StEventTypes.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFcsCollection.h"
#include "StEvent/StFcsHit.h"
#include "StEvent/StFcsPoint.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"

#include <iostream>
#include <fstream>
#include "StMessMgr.h"
#include "StEventTypes.h"
using namespace std;

StjFCSMuDst::StjFCSMuDst()
{
    _setVertex = false;
    useECal = false;
    useHCal = false;
    
    mFcsDb = nullptr;
    StMaker* chain = StMaker::GetChain();
    if (chain) {
        if(chain->GetDataSet("fcsDb")){
            mFcsDb = static_cast<StFcsDb*>(chain->GetDataSet("fcsDb"));
            mFcsDb->setDbAccess(0);
        }else{
            LOG_ERROR << "No fcdDb found in chain. Please initialize StFcsDbMaker within your chain." << endm;
        }
    }
    if (!chain) {
        LOG_ERROR << "StjFCSMuDst Initializing failed due to missing chain." << endm;
    }
}

StjTowerEnergyList StjFCSMuDst::getEnergyList(){
    StjTowerEnergyList fcsEnergyList;
    
    mMuFcsColl = StMuDst::muFcsCollection();
        
    if(mMuFcsColl->numberOfHits()!=0){

        StjTowerEnergy energyDeposit;
        for(int det = 0; det <= kFcsHcalSouthDetId; det++){
                   
            int nhits = mMuFcsColl->numberOfHits(det);
            int det_hit_index = mMuFcsColl->indexOfFirstHit(det);
                
            for(int hit_i = 0 ; hit_i < nhits; hit_i++){
                
                int hit_index = hit_i + det_hit_index;
                StMuFcsHit* hit = mMuFcsColl->getHit(hit_index);
                
                if(det <= 1 && useECal){
                    fcsEnergyList.push_back(hitenergyDeposit(*hit));
                }
                if(det >= 2 && useHCal){
                    fcsEnergyList.push_back(hitenergyDeposit(*hit));
                }
                
            }// Hit loop
            
        }//Det Loop
    }
    
    return fcsEnergyList;
}

StjTowerEnergy StjFCSMuDst::hitenergyDeposit(const StMuFcsHit& hit){
    StjTowerEnergy energyDeposit;
    
    StThreeVectorD xyz = mFcsDb->getStarXYZ(hit.detectorId(),hit.id());
    
    float x = xyz.x();
    float y = xyz.y();
    float z = xyz.z();
        
    energyDeposit.runNumber = StMuDst::event()->runId();
    energyDeposit.eventId = StMuDst::event()->eventId();
    
    energyDeposit.towerId = hit.id();
    energyDeposit.detectorId = hit.detectorId();
    energyDeposit.towerR = TMath::Sqrt(x*x + y*y);
    energyDeposit.towerEta = xyz.pseudoRapidity();
    energyDeposit.towerPhi = xyz.phi();
    energyDeposit.vertexX = 0; //To be use with FwdTracks vertext reconstruction
    energyDeposit.vertexY = 0;
    energyDeposit.vertexZ = 0;
    energyDeposit.energy   = hit.energy();
    energyDeposit.adc      = hit.adcSum();
    energyDeposit.pedestal = 0;
    energyDeposit.rms      = 0;
    energyDeposit.status   = 1;

    return energyDeposit;
}



// $Id: StjFMSMuDst.cxx,v 1.1 2017/05/22 19:35:00 zchang Exp $
#include "StjFMSMuDst.h"

#include "StMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"

//#include <cassert>
#include "StMuDSTMaker/COMMON/StMuFmsCollection.h"
#include "StMuDSTMaker/COMMON/StMuFmsHit.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"

#include "StFmsDbMaker/StFmsDbMaker.h"
#include "StEventTypes.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFmsCollection.h"
#include "StEvent/StFmsHit.h"
#include "StEvent/StFmsPoint.h"
#include "StEvent/StFmsPointPair.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"

//per Pibero
#include <iostream>
#include <fstream>
using namespace std;

StjFMSMuDst::StjFMSMuDst() : mFmsDbMaker((StFmsDbMaker*)StMaker::GetChain()->GetDataSet("fmsDb"))
{
  _setVertex = false;
  //  cout<<" HERE  "<<endl;
  //fmsdb  = gStFmsDbMaker;
}


void StjFMSMuDst::Init()
{
  /*  mFmsDbMaker=static_cast<StFmsDbMaker*>(GetMaker("fmsDb"));
  if(!mFmsDbMaker){
    LOG_ERROR  << "StFmsJetMaker::InitRun Failed to get StFmsDbMaker" << endm;
    return kStFatal;
  }
  */
  //  return StMaker::Init();
}


StjTowerEnergyList StjFMSMuDst::getEnergyList()
{
  StjTowerEnergyList fmsEnergyList;

  mFmsColl = findFmsCollection();  
  if(mFmsColl){
  StjTowerEnergy energyDeposit;
  int npoint=mFmsColl->numberOfPoints();
  StSPtrVecFmsPoint& points = mFmsColl->points();
  //loop over FMS points                                                                                                    
  for(int i=0; i<npoint; i++) {
    float x=points[i]->XYZ().x();
    float y=points[i]->XYZ().y();
    //float z=points[i]->XYZ().z();                                                                                         
    StLorentzVectorF v1=points[i]->fourMomentum();

    //loop over FPS layers to and project                                                                                   
    energyDeposit.towerR = TMath::Sqrt(x*x + y*y);
    energyDeposit.towerEta = v1.pseudoRapidity();
    energyDeposit.towerPhi = v1.phi();
    energyDeposit.vertexX = 0;
    energyDeposit.vertexY = 0;
    energyDeposit.vertexZ = 0;
    energyDeposit.energy   = points[i]->energy();
    energyDeposit.adc      = points[i]->energy();
    energyDeposit.pedestal = 0;
    energyDeposit.rms      = 0;
    energyDeposit.status   = 1;
    energyDeposit.towerId  = points[i]->id();
    energyDeposit.detectorId  = 100 + points[i]->detectorId() ; //set FMS detector Id to 100 + detectorId(), i.e. 108, 109, 110, and 111, by zchang

    if(points[i]->energy()>1) fmsEnergyList.push_back(energyDeposit);
  }
  }
  return fmsEnergyList;

}


StFmsCollection* StjFMSMuDst::findFmsCollection()
{
  StEvent* event = dynamic_cast<StEvent*>(StMaker::GetChain()->GetInputDS("StEvent"));
  return (event) ? event->fmsCollection() : StMuDst::fmsCollection();
}

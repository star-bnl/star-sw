// $Id: StJetEEMCMuDst.cxx,v 1.7 2008/07/13 05:36:46 tai Exp $
#include "StJetEEMCMuDst.h"

#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"

#include "StEEmcDbMaker/StEEmcDbMaker.h"
#include "StEEmcDbMaker/EEmcDbItem.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"


namespace StSpinJet {


StJetEEMCMuDst::StJetEEMCMuDst(StMuDstMaker* uDstMaker)
 : _uDstMaker(uDstMaker)
 , mEeDb(0)
{

}

void StJetEEMCMuDst::Init()
{
  if(_uDstMaker) mEeDb = (StEEmcDbMaker*)_uDstMaker->GetMaker("eemcDb");
  if(mEeDb) mEeDb->setThreshold(3);
}

TowerEnergyList StJetEEMCMuDst::getEnergyList()
{
  StMuEmcCollection* muEmc = _uDstMaker->muDst()->muEmcCollection();

  TowerEnergyList ret;

  for (int id = 0; id < muEmc->getNEndcapTowerADC(); ++id) {

    int rawadc, sec, sub, etabin;
    muEmc->getEndcapTowerADC(id, rawadc, sec, sub, etabin);
	
    const EEmcDbItem *dbItem = mEeDb->getT(sec,sub-1+'A',etabin);
	
    if(dbItem->fail) continue; //drop broken channels
    if(dbItem->stat) continue; // drop not working channels and jumpy pedestal channels
    if(dbItem->gain<=0.) continue; // drop it, unless you work with ADC spectra
    if(rawadc<dbItem->thr) continue; // drop raw ADC < ped+N*sigPed, N==3 in init
	    
    double adc = rawadc - (dbItem->ped);
    double energy = adc/(dbItem->gain);
	    
    TowerEnergy energyDeposit;
    energyDeposit.runNumber = _uDstMaker->muDst()->event()->runId();
    energyDeposit.eventId = _uDstMaker->muDst()->event()->eventId();
    energyDeposit.detectorId = 13;
    energyDeposit.towerId = (sec*5 + sub)*12 + etabin;


    EEmcGeomSimple geom;
    TVector3 towerLocation = geom.getTowerCenter(sec-1,sub-1,etabin-1);

    energyDeposit.towerR = towerLocation.Perp();
    energyDeposit.towerEta = towerLocation.Eta();
    energyDeposit.towerPhi = towerLocation.Phi();

    StThreeVectorF vertex = _uDstMaker->muDst()->event()->primaryVertexPosition();

    energyDeposit.vertexX = vertex.x();
    energyDeposit.vertexY = vertex.y();
    energyDeposit.vertexZ = vertex.z(); 

    energyDeposit.energy   = energy;
    energyDeposit.adc      = rawadc;
    energyDeposit.pedestal = dbItem->ped;
    energyDeposit.rms      = 0.0;
    energyDeposit.status   = dbItem->stat;

    if(energyDeposit.energy < 0.01) continue; // drop if less than 10MeV for now

    ret.push_back(energyDeposit);

  }

  return ret;
}

}


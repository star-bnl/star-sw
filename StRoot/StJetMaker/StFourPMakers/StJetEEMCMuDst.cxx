// $Id: StJetEEMCMuDst.cxx,v 1.5 2008/07/10 20:48:59 tai Exp $
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
 : mMuDstMaker(uDstMaker)
 , mEeDb(0)
{

}

void StJetEEMCMuDst::Init()
{
  if(mMuDstMaker) mEeDb = (StEEmcDbMaker*)mMuDstMaker->GetMaker("eemcDb");
  if(mEeDb) mEeDb->setThreshold(3);
}

TowerEnergyList StJetEEMCMuDst::getEnergyList()
{
  StMuEmcCollection* muEmc = mMuDstMaker->muDst()->muEmcCollection();

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
    energyDeposit.detectorId = 13;
    energyDeposit.towerId = (sec*5 + sub)*12 + etabin;


    EEmcGeomSimple geom;
    TVector3 towerLocation = geom.getTowerCenter(sec-1,sub-1,etabin-1);

    energyDeposit.towerX = towerLocation.x();
    energyDeposit.towerY = towerLocation.y();
    energyDeposit.towerZ = towerLocation.z();

    StThreeVectorF vertex = mMuDstMaker->muDst()->event()->primaryVertexPosition();

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


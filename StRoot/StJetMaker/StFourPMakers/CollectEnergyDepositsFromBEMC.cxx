// $Id: CollectEnergyDepositsFromBEMC.cxx,v 1.4 2008/07/08 11:21:56 tai Exp $
#include "CollectEnergyDepositsFromBEMC.h"

#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StEvent/StEmcRawHit.h>
#include <StEvent/StEmcCollection.h>
#include <StEvent/StEmcModule.h>
#include <StEvent/StEmcDetector.h>
#include <StEmcUtil/geometry/StEmcGeom.h>
#include <StEmcRawMaker/defines.h>
#include <StEmcRawMaker/StBemcTables.h>
#include "StMuDSTMaker/COMMON/StMuEvent.h"

#include <iostream>

using namespace std;

namespace StSpinJet {

CollectEnergyDepositsFromBEMC::CollectEnergyDepositsFromBEMC(StMuDstMaker* uDstMaker, StBemcTables* bemcTables)
  : mMuDstMaker(uDstMaker)
  , _bemcTables(bemcTables)
  , mUse2003Cuts(false)
  , mUse2005Cuts(false)
{

}

TowerEnergyDepositList CollectEnergyDepositsFromBEMC::Do()
{
  BemcTowerIdHitMap allBemcTowerHits = getTowerHitsFromBEMC();

  BemcTowerIdHitMap selectedBemcTowerHits = selectBemcTowerHits(allBemcTowerHits);

  return readBemcTowerEnergy(selectedBemcTowerHits);
}

CollectEnergyDepositsFromBEMC::BemcTowerIdHitMap CollectEnergyDepositsFromBEMC::getTowerHitsFromBEMC()
{
  BemcTowerIdHitMap ret;

  StEmcCollection* emc = mMuDstMaker->muDst()->emcCollection();
  StEmcDetector* detector = emc->detector(kBarrelEmcTowerId);

  static const int nBemcModules = 120;
  for(int m = 1; m <= nBemcModules; ++m) { //loop on modules...
    StEmcModule* module = detector->module(m);
  	
    StSPtrVecEmcRawHit& rawHits = module->hits();
    for(UInt_t k = 0; k < rawHits.size(); ++k) { //loop on hits in modules
      StEmcRawHit* theRawHit = rawHits[k];
  	    
      StEmcGeom* geom = StEmcGeom::instance("bemc"); 
      int bemcTowerID;
      geom->getId(theRawHit->module(), theRawHit->eta(), abs(theRawHit->sub()),bemcTowerID); // to get the software id

      ret[bemcTowerID] = theRawHit;
    }
  }
  return ret;
}

CollectEnergyDepositsFromBEMC::BemcTowerIdHitMap CollectEnergyDepositsFromBEMC::selectBemcTowerHits(const BemcTowerIdHitMap &bemcTowerHits)
{
  BemcTowerIdHitMap ret;

  for(map<BemcTowerID, const StEmcRawHit*>::const_iterator it = bemcTowerHits.begin(); it != bemcTowerHits.end(); ++it) {

    if (!shouldKeepThisBemcHit((*it).second, (*it).first))
      continue;

    ret.insert(*it);
  }
  return ret;
}

bool CollectEnergyDepositsFromBEMC::shouldKeepThisBemcHit(const StEmcRawHit* theRawHit, int bemcTowerID)
{

  if (mUse2003Cuts)
    if (!accept2003Tower(bemcTowerID)) return false;

  if (mUse2005Cuts)
    if (bemcTowerID > 2400) return false;

  //now check the status: (//BTOW defined in StEmcRawMaker/defines.h
  int status;
  _bemcTables->getStatus(BTOW, bemcTowerID, status);
  
  //check for ADC that is 2-sigma above RMS:
  float pedestal, rms;
  int CAP(0); //this arument matters only for SMD
  _bemcTables->getPedestal(BTOW, bemcTowerID, CAP, pedestal, rms);
  
  if (status != 1) return false;

  if (theRawHit->adc() - pedestal <= 0) return false;

  if ((theRawHit->adc() - pedestal) <= 2.*rms) return false;

  return true;

}

StSpinJet::TowerEnergyDepositList CollectEnergyDepositsFromBEMC::readBemcTowerEnergy(const BemcTowerIdHitMap &bemcTowerHits)
{
  TowerEnergyDepositList ret;

  for(BemcTowerIdHitMap::const_iterator it = bemcTowerHits.begin(); it != bemcTowerHits.end(); ++it) {

    const StEmcRawHit* hit = (*it).second;
    int bemcTowerID = (*it).first;

    if(hit->energy() <= 0) continue;

    TowerEnergyDeposit energyDeposit;
    energyDeposit.detectorId = kBarrelEmcTowerId;
    energyDeposit.towerId = (*it).first;

    TVector3 towerLocation = getBemcTowerLocation(energyDeposit.towerId);

    energyDeposit.towerX = towerLocation.x();
    energyDeposit.towerY = towerLocation.y();
    energyDeposit.towerZ = towerLocation.z();

    StThreeVectorF vertex = mMuDstMaker->muDst()->event()->primaryVertexPosition();

    energyDeposit.vertexX = vertex.x();
    energyDeposit.vertexY = vertex.y();
    energyDeposit.vertexZ = vertex.z(); 

    energyDeposit.energy = hit->energy();
    energyDeposit.adc    = hit->adc();

    float pedestal, rms;
    int CAP(0);
    _bemcTables->getPedestal(BTOW, bemcTowerID, CAP, pedestal, rms);
    energyDeposit.pedestal = pedestal;
    energyDeposit.rms = rms;

    int status;
    _bemcTables->getStatus(BTOW, bemcTowerID, status);
    energyDeposit.status = status;

    ret.push_back(energyDeposit);
  }

  return ret;
}

bool CollectEnergyDepositsFromBEMC::accept2003Tower(int id)
{
    if( id==555
	|| id==615
	|| id==656
	|| id==772
	|| id==1046
	|| id==1048
	|| id==1408
	|| id==1555
	|| id==1750
	|| id==1773
	|| id==2073
	|| id==2093
	|| id==2096
	|| (id>=1866 && id<=1894)
	|| id==511
	|| id==1614
	|| id==1615
	|| id==1616
	|| id==1636
	|| id==1899
	|| id==2127
	|| id==953
	|| id==1418
	|| id==1419
	|| id==1878
	|| id==1879
	|| id==1881
	|| (id>=1042 && id<=1045)
	|| (id>=1385 && id<=1387)
	|| (id>=1705 && id<=1708)
	|| (id>=1725 && id<=1728)
	|| (id>=1745 && id<=1748)
	|| (id>=1765 && id<=1768)
	|| (id>=1785 && id<=1788))
	{
	    cout <<"rejecting tower:\t"<<id<<endl;
	    return false;
	}
    else {
	return true;
    }
}

TVector3 CollectEnergyDepositsFromBEMC::getBemcTowerLocation(int bemcTowerId)
{
  StEmcGeom* geom = StEmcGeom::instance("bemc");

  float towerX, towerY, towerZ;

  geom->getXYZ(bemcTowerId, towerX, towerY, towerZ);

  return TVector3(towerX, towerY, towerZ);
}


}

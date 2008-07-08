// $Id: CollectEnergyDepositsFromBEMC.cxx,v 1.8 2008/07/08 23:45:43 tai Exp $
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

StJetBEMCMuDst::StJetBEMCMuDst(StMuDstMaker* uDstMaker, StBemcTables* bemcTables)
  : mMuDstMaker(uDstMaker)
  , _bemcTables(bemcTables)
{

}

TowerEnergyDepositList StJetBEMCMuDst::getEnergyList()
{
  TowerEnergyDepositList ret;

  StEmcDetector* detector = mMuDstMaker->muDst()->emcCollection()->detector(kBarrelEmcTowerId);

  static const int nBemcModules = 120;

  for(int m = 1; m <= nBemcModules; ++m) {

    StSPtrVecEmcRawHit& rawHits = detector->module(m)->hits();

    for(UInt_t k = 0; k < rawHits.size(); ++k) {
  	    
      ret.push_back(readTowerHit(*rawHits[k]));
    }
  }

  return ret;
}

TowerEnergyDeposit StJetBEMCMuDst::readTowerHit(const StEmcRawHit& hit)
{
  TowerEnergyDeposit ret;

  ret.detectorId = kBarrelEmcTowerId;

  int towerId;
  StEmcGeom::instance("bemc")->getId(hit.module(), hit.eta(), abs(hit.sub()), towerId);

  ret.towerId = towerId;

  float towerX, towerY, towerZ;
  StEmcGeom::instance("bemc")->getXYZ(towerId, towerX, towerY, towerZ);

  ret.towerX = towerX;
  ret.towerY = towerY;
  ret.towerZ = towerZ;

  StThreeVectorF vertex = mMuDstMaker->muDst()->event()->primaryVertexPosition();

  ret.vertexX = vertex.x();
  ret.vertexY = vertex.y();
  ret.vertexZ = vertex.z(); 

  ret.energy = hit.energy();
  ret.adc    = hit.adc();

  float pedestal, rms;
  int CAP(0);
  _bemcTables->getPedestal(BTOW, towerId, CAP, pedestal, rms);
  ret.pedestal = pedestal;
  ret.rms = rms;

  int status;
  _bemcTables->getStatus(BTOW, towerId, status);
  ret.status = status;

  return ret;
}


CollectEnergyDepositsFromBEMC::CollectEnergyDepositsFromBEMC(StMuDstMaker* uDstMaker, StBemcTables* bemcTables)
  : _bemc(new StJetBEMCMuDst(uDstMaker, bemcTables))
  , mUse2003Cuts(false)
  , mUse2005Cuts(false)
{

}


TowerEnergyDepositList CollectEnergyDepositsFromBEMC::Do()
{
  TowerEnergyDepositList energyList = _bemc->getEnergyList();

  energyList = selectBemcTowerHits(energyList);

  return energyList;
}

StSpinJet::TowerEnergyDepositList CollectEnergyDepositsFromBEMC::selectBemcTowerHits(const TowerEnergyDepositList &energyList)
{
  TowerEnergyDepositList ret;

  for(TowerEnergyDepositList::const_iterator it = energyList.begin(); it != energyList.end(); ++it) {

    if(!shouldKeepThisBemcHit(*it)) continue;

    ret.push_back(*it);

  }

  return ret;
}


bool CollectEnergyDepositsFromBEMC::shouldKeepThisBemcHit(const TowerEnergyDeposit& energyDeposit)
{

  if(mUse2003Cuts)
    if(!accept2003Tower(energyDeposit.towerId)) return false;

  if (mUse2005Cuts)
    if(energyDeposit.towerId > 2400) return false;

  if(energyDeposit.energy <= 0) return false;

  if(energyDeposit.status != 1) return false;

  if(energyDeposit.adc - energyDeposit.pedestal <= 0) return false;

  if((energyDeposit.adc - energyDeposit.pedestal) <= 2.*energyDeposit.rms) return false;

  return true;

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



}

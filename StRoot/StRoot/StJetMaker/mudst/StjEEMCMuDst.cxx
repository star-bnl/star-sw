// $Id: StjEEMCMuDst.cxx,v 1.10 2012/03/10 10:49:16 pibero Exp $
#include "StjEEMCMuDst.h"

#include "StMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"

#include "StEEmcUtil/database/StEEmcDb.h"
#include "StEEmcUtil/database/EEmcDbItem.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"


StjEEMCMuDst::StjEEMCMuDst() : mEeDb((StEEmcDb*)StMaker::GetChain()->GetDataSet("StEEmcDb"))
{
  _setVertex = false;
}

void StjEEMCMuDst::Init()
{
  if (mEeDb) mEeDb->setThreshold(3);
}

StjTowerEnergyList StjEEMCMuDst::getEnergyList()
{
  StMuEmcCollection* muEmc = StMuDst::muEmcCollection();

  StjTowerEnergyList ret;

  for (int id = 0; id < muEmc->getNEndcapTowerADC(); ++id) {

    int rawadc, sec, sub, etabin;
    muEmc->getEndcapTowerADC(id, rawadc, sec, sub, etabin);

    // Sanity check
    if (rawadc < 0 || rawadc >= 4095) continue;

    assert(1 <= sec && sec <= 12);
    assert(1 <= sub && sub <= 5);
    assert(1 <= etabin && etabin <= 12);
	
    // See http://www.star.bnl.gov/public/eemc/how_to/dbUsage.html
    // See $STAR/StRoot/StEEmcUtil/database/cstructs/eemcConstDB.hh
    //
    // Use idividual bits of 'stat' to exclude individual
    // channels from a particular analysis, but let other 
    // analysis make a different choice.
    //
    // status bits (short int) 
    //
    // #define EEMCSTAT_ONLPED   0x0001 // only pedestal is visible
    // #define EEMCSTAT_STKBT    0x0002 // sticky lower bits
    // #define EEMCSTAT_HOTHT    0x0004 // masked for HT trigger
    // #define EEMCSTAT_HOTJP    0x0008 // masked for JP trigger
    // #define EEMCSTAT_HIGPED   0x0010 // ped is very high but channel seems to work
    // #define EEMCSTAT_HOTSTR   0x0020 // hot esmd strip
    // #define EEMCSTAT_JUMPED   0x0040 // jumpy  ped over several chan over days
    // #define EEMCSTAT_WIDPED   0x0080 // wide ped over:2.5 ch  towers, 1.5 ch MAPMT's
    //
    //The remaing  bits of 'stat' are free.
    //
    // The 'fail' 16-bits are meant as general abort of a given channel.
    //
    // failure bits (short int)
    // #define EEMCFAIL_GARBG  0x0001  // faulty channel
    // #define EEMCFAIL_HVOFF  0x0002  // HV was off or varied
    // #define EEMCFAIL_NOFIB  0x0004  // signal fiber is broken
    // #define EEMCFAIL_CPYCT  0x0008  // stuck in copyCat mode 
	
    const EEmcDbItem *dbItem = mEeDb->getT(sec,sub-1+'A',etabin);

    if(dbItem->fail) continue; //drop broken channels
    if(dbItem->stat) continue; // drop not working channels and jumpy pedestal channels
    if(dbItem->gain<=0.) continue; // drop it, unless you work with ADC spectra
    if(rawadc<dbItem->thr) continue; // drop raw ADC < ped+N*sigPed, N==3 in init
	    
    double adc = rawadc - (dbItem->ped);
    double energy = adc/(dbItem->gain);
	    
    StjTowerEnergy energyDeposit;
    energyDeposit.runNumber = StMuDst::event()->runId();
    energyDeposit.eventId = StMuDst::event()->eventId();
    energyDeposit.detectorId = kEndcapEmcTowerId;
    energyDeposit.towerId = (sec-1)*60+(sub-1)*12+(etabin-1);

    const EEmcGeomSimple& geom = EEmcGeomSimple::Instance();
    TVector3 towerLocation = geom.getTowerCenter(sec-1,sub-1,etabin-1);

    energyDeposit.towerR = towerLocation.Perp();
    energyDeposit.towerEta = towerLocation.Eta();
    energyDeposit.towerPhi = towerLocation.Phi();

    if (_setVertex) {
      energyDeposit.vertexX = _vx;
      energyDeposit.vertexY = _vy;
      energyDeposit.vertexZ = _vz;
    }
    else {
      StThreeVectorF vertex = StMuDst::event()->primaryVertexPosition();

      energyDeposit.vertexX = vertex.x();
      energyDeposit.vertexY = vertex.y();
      energyDeposit.vertexZ = vertex.z(); 
    }

    energyDeposit.energy   = energy;
    energyDeposit.adc      = rawadc;
    energyDeposit.pedestal = dbItem->ped;
    energyDeposit.rms      = dbItem->sigPed;
    energyDeposit.status   = dbItem->stat == 0;

    if(energyDeposit.energy < 0.01) continue; // drop if less than 10MeV for now

    ret.push_back(energyDeposit);

  }

  return ret;
}

/***************************************************************************
 *
 * $Id: TowerSanityMaker.cxx,v 1.5 2003/09/24 20:54:08 thenry Exp $
 * 
 * Author: Thomas Henry August 2003
 ***************************************************************************
 *
 * Description:  Maker which creates a database of average tower values per
 * run, accessible by runnumber or eventid and towerid
 *
 ***************************************************************************
 *
 * Revision 1.0  2003/02/20 thenry
 *
 **************************************************************************/
#include <string>
#include <iostream>
#include <math.h>
#include <sys/times.h>

#ifdef __ROOT__
#include "StChain.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StIOMaker/StIOMaker.h"
#include "StEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StSpinMaker/StMuTrackFourVec.h"

#include "StEmcClusterCollection.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StEmcPoint.h"
#include "StMuDSTMaker/COMMON/StMuEmcUtil.h"
#include "StEmcTpcFourPMaker.h"
#include "SystemOfUnits.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcADCtoEMaker/StBemcData.h"

#include "TowerSanityMaker.h"

ClassImp(TowerSanityMaker)
  
TowerSanityMaker::TowerSanityMaker(const char* name,
				   StEmcADCtoEMaker* adcToEMaker,
				   StMuDstMaker* uDstMkr,
				   const char* dataBaseName, 
				   dataBaseUse dbu) : 
  SafetyArray(name), adcToE(adcToEMaker), muDstMkr(uDstMkr),
  dbName(dataBaseName), dbUse(dbu), ofile(NULL) 
{
  runThreshold = 0.007;
  maxTowerThreshold = 12.0;
  avgTowerThreshold = 0.03;
}

Int_t TowerSanityMaker::Make()
{
  if(dbUse == toRead)
    return kStOK;
  StMuDst* uDst = muDstMkr->muDst();
  StMuEvent* uEvent = uDst->event();
  unsigned int runNumber = uEvent->runNumber();
  int pointIndex = 0;
  int& hitId = pointIndex;
  StBemcData* data = adcToE->getBemcData();
  int numHits = data->NTowerHits;
  cout << "Number Hits: " << numHits;
  int maxHits = 4800;

  tripower ttrip;
  for(hitId = 1; hitId <= maxHits; hitId++)
    {
      if(data->TowerStatus[hitId-1] != 1) continue;
      double energy = data->TowerEnergy[hitId-1];
      ttrip.set(hitId-1, energy);
      TowerSanity.add(ttrip, static_cast<long>(runNumber));
    }
  
  return kStOK;
}

Int_t TowerSanityMaker::Init()
{
  if((dbUse == toRead) || (dbUse == toMerge))
    {
      ifstream is(dbName);
      TowerSanity.load(is);
    }
  return kStOK;
}

Int_t TowerSanityMaker::Finish()
{
  if((dbUse == toWrite) || (dbUse == toMerge))
    {
      ofstream os(dbName, ofstream::out | ofstream::trunc);
      TowerSanity.save(os);
    }
  /*
  for(edbType::iterator it = TowerSanity.EMCDB.begin(); 
      it != TowerSanity.EMCDB.end(); ++it)
    {
      long runNumber = (*it).first;
      cout << "runAverageEnergyPerTower: " << 
	runAverageEnergyPerTower(runNumber) << endl;
      cout << "runStandardDeviation: " << 
	runStandardDeviation(runNumber) << endl;
      tdbType &TDB = (*it).second.TDB;
      for(tdbType::iterator it = TDB.begin(); it != TDB.end(); ++it)
	{
	  tripower &tr = (*it).second;
	  cout << "Tower Id: " << tr.towerId << endl;
	  cout << "Number Events: " << tr.events << endl;
	  cout << "Tower Max: " << tr.max << endl;
	  cout << "Tower Average: " << tr.average() << endl;
	  cout << "Tower StdDev: " << tr.stddev() << endl;
	}
	}*/
  return kStOK;
}

bool TowerSanityMaker::isGood(unsigned int runNumber, long index)
{
  StEmcGeom* geom = StEmcGeom::getEmcGeom(detname[0].Data());
  Int_t module, eta, s;
  geom->getBin(static_cast<const Int_t>(index), module, eta, s);
  if(badModules.count(module) > 0) 
    {
      cout << "Bad module #: " << module << endl;
      return false;
    }
  if(runAverageEnergyPerTower(runNumber) > runThreshold)
    {
      cout << "runAverageEnergyPerTower(runNumber)=" <<
	runAverageEnergyPerTower(runNumber) 
	   << '(' << runNumber << ')' << endl;
      return false;
    }
  if(maxTowerEnergy(runNumber, index) > maxTowerThreshold)
    {
      cout << "maxTowerEnergy(runNumber, index)=" <<
	maxTowerEnergy(runNumber, index) << '(' << runNumber << 
	", " << index << ')' << endl;
      return false;
    }
  if(averageTowerEnergy(runNumber, index) > avgTowerThreshold)
    {
      cout << "averageTowerEnergy(runNumber, index)=" <<
	averageTowerEnergy(runNumber, index) << '(' << runNumber << 
	", " << index << ')' << endl;
      return false;
    }
  return true;
}
#endif //__ROOT__








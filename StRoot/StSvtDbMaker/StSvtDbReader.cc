/***************************************************************************
 *
 * $Id: StSvtDbReader.cc,v 1.6 2004/07/31 00:50:26 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT DB access Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtDbReader.cc,v $
 * Revision 1.6  2004/07/31 00:50:26  munhoz
 * adding anode drift veloc correction factor
 *
 * Revision 1.5  2004/07/26 00:06:08  munhoz
 * read drift curve
 *
 * Revision 1.4  2004/01/30 07:22:07  munhoz
 * adding rms and daq parameters reading
 *
 * Revision 1.3  2003/04/25 19:46:19  caines
 * Switch argument calls to correct order
 *
 * Revision 1.2  2001/11/07 16:49:48  caines
 *  Add _st s added by stic for our comfort and convenience
 *
 * Revision 1.1  2001/10/29 18:53:13  munhoz
 * starting SVT Data base
 *
 *
 **************************************************************************/

#include "StSvtDbReader.hh"
#include "StMessMgr.h"

#include "StSvtClassLibrary/StSvtConfig.hh"
#include "StSvtClassLibrary/StSvtHybridDriftVelocity.hh"
#include "StSvtClassLibrary/StSvtHybridPed.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClassLibrary/StSvtGeometry.hh"
#include "StSvtClassLibrary/StSvtWaferGeometry.hh"

#include "StDbLib/StDbManager.hh"                      // Database Libraries
#include "StDbLib/StDbConfigNode.hh"                   //
#include "StDbLib/StDbTable.h"                         //

#include "svtPedestals.h"
#include "svtDriftVelocity.h"
#include "svtConfiguration.h"
#include "svtWafersPosition.h"
#include "svtDimensions.h"

#ifdef __ROOT__
ClassImp(StSvtDbReader)
#endif

//_____________________________________________________________________________
StSvtDbReader::StSvtDbReader(Text_t *timestamp)
{
  // set time stamp
  mTimeStamp = timestamp;
  mUnixTimeStamp = 0;

  // set DB manager
  setDbManager();
}

//_____________________________________________________________________________
StSvtDbReader::StSvtDbReader(Int_t timestamp)
{
  // set time stamp
  mTimeStamp = 0;
  mUnixTimeStamp = timestamp;

  // set DB manager
  setDbManager();
}

//_____________________________________________________________________________
StSvtDbReader::~StSvtDbReader()
{
  if (mConfigCalib)
    delete mConfigCalib;
  if (mConfigGeom)
    delete mConfigGeom;
  if (mConfigCond)
    delete mConfigCond;
}

//_____________________________________________________________________________
void StSvtDbReader::setDbManager()
{
  //->get the singleton manager
  //
  mDbMgr = StDbManager::Instance();

  mDbMgr -> setVerbose(true);             // set Verbose mode for debug

  //-> connect to the db & get an empty container
  //
  mConfigCalib = mDbMgr -> initConfig(dbCalibrations,dbSvt);
  mConfigGeom  = mDbMgr -> initConfig(dbGeometry,dbSvt);
  //  mConfigCond  = mDbMgr -> initConfig(dbConditions,dbSvt);

  // set the request time. (need to be only once for an ensemble of tables)
  //
  if (mTimeStamp)
    mDbMgr->setRequestTime(mTimeStamp);
  else
    mDbMgr->setRequestTime(mUnixTimeStamp);
}

//_____________________________________________________________________________
StSvtConfig* StSvtDbReader::getConfiguration()
{
  cout << "StSvtDbReader::getConfiguration" << endl;

  //-> add a table to the container with descriptor given by Database
  //
  StDbTable* svtConfigTable = mConfigGeom -> addDbTable("svtConfiguration");
  if (svtConfigTable == 0 )
   {
     gMessMgr->Warning() << " No Table : svtConfiguration " << endm;
     return(0);
   };

  //
  // fetch the data & use it.
  //
  mDbMgr->fetchDbTable(svtConfigTable);

  svtConfiguration_st *config  = (svtConfiguration_st*) svtConfigTable->GetTable() ;
  
  cout << config->numberOfBarrels << endl;
  cout << config->numberOfLadders << endl;
  cout << config->numberOfWafers << endl;
  cout << config->numberOfHybrids << endl;
  
  cout << config->numberOfLaddersPerBarrel[0] << endl;
  cout << config->numberOfLaddersPerBarrel[1] << endl;
  cout << config->numberOfLaddersPerBarrel[2] << endl;
  cout << config->numberOfWafersPerLadder[0] << endl;
  cout << config->numberOfWafersPerLadder[1] << endl;
  cout << config->numberOfWafersPerLadder[2] << endl;
  cout << config->numberOfHybridsPerWafer << endl;
  
  mSvtConfig = new StSvtConfig();
  
  mSvtConfig->setNumberOfBarrels(config->numberOfBarrels);
  
  for (int i=0; i<config->numberOfBarrels; i++) {
    mSvtConfig->setNumberOfLadders(i+1,config->numberOfLaddersPerBarrel[i]);
    mSvtConfig->setNumberOfWafers(i+1, config->numberOfWafersPerLadder[i]);
  }
  mSvtConfig->setNumberOfHybrids(config->numberOfHybridsPerWafer);
  mSvtConfig->setTotalNumberOfHybrids(config->numberOfHybrids);

  //temporary. Must read electronics db and fill these quantities
  mSvtConfig->setNumberOfAnodes(240);
  mSvtConfig->setNumberOfTimeBins(128);

  mSvtConfig->setConfiguration();

  return mSvtConfig;
}

//_____________________________________________________________________________
StSvtHybridCollection* StSvtDbReader::getDriftVelocity()
{
  int index, numberOfHybrids;
  StSvtHybridDriftVelocity* hybridDriftVeloc;

  cout << "StSvtDbReader::getDriftVelocity" << endl;

  //-> add a table to the container with descriptor given by Database
  //
  StDbTable* svtDriftVelocTable = mConfigCalib -> addDbTable("svtDriftVelocity");
  if (svtDriftVelocTable == 0 )
   {
     cout << " No Table : svtDriftVelocity  " << endl;
     return(0);
   };

  if (!mSvtConfig) {
    gMessMgr->Warning() << " No Configuration information! Impossible to retrieve drift velocity! " << endm;
    return 0;
  }
  
  //
  // fetch the data & use it.
  //
  numberOfHybrids = mSvtConfig->getTotalNumberOfHybrids();
  int* indexHybrid =  new int[numberOfHybrids];

  // Loop over barrels, ladders, wafers and hybrids
  int i=0;
  for (int barrel = 1;barrel <= mSvtConfig->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtConfig->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtConfig->getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= mSvtConfig->getNumberOfHybrids();hybrid++) {
	  indexHybrid[i++] = mSvtConfig->getHybridIndex(barrel,ladder,wafer,hybrid);
	}
      }
    }
  }

  svtDriftVelocTable->setElementID(indexHybrid,numberOfHybrids); // set element ID to hybridIndex
  mDbMgr->fetchDbTable(svtDriftVelocTable);

  svtDriftVelocity_st *driftVeloc  = (svtDriftVelocity_st*)svtDriftVelocTable -> GetTable() ;

  // Create all drift velocity objects
  StSvtHybridCollection* svtDriftVeloc = new StSvtHybridCollection(mSvtConfig);

  for (int barrel = 1;barrel <= mSvtConfig->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtConfig->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtConfig->getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= mSvtConfig->getNumberOfHybrids();hybrid++) {

	  index = mSvtConfig->getHybridIndex(barrel,ladder,wafer,hybrid);

	  if (index < 0) continue;

	  hybridDriftVeloc = new StSvtHybridDriftVelocity(barrel,ladder,wafer,hybrid);
	  
	  // loop over anodes
	  for (int anode=1;anode<=mSvtConfig->getNumberOfAnodes();anode++) {
	    hybridDriftVeloc->setV1(driftVeloc[index].injectionDriftVelocityFocus[anode-1],anode);
	    hybridDriftVeloc->setV2(driftVeloc[index].injectionDriftVelocity[anode-1],anode);
	    hybridDriftVeloc->setV3(driftVeloc[index].averageDriftVelocity[anode-1],anode);
	  }

	  svtDriftVeloc->put_at(hybridDriftVeloc,index);

	} // end of loop over hybrids
      } // end of loop over wafers
    } // end of loop over ladders
  } // end of loop over barrels

  return svtDriftVeloc;
}

//_____________________________________________________________________________
StSvtHybridCollection* StSvtDbReader::getPedestals()
{
  int index, numberOfHybrids;
  StSvtHybridPed* hybridPed;

  cout << "StSvtDbReader::getPedestals" << endl;

  //-> add a table to the container with descriptor given by Database
  //
  StDbTable* svtPedTable = mConfigCalib -> addDbTable("svtPedestals");
  if (svtPedTable == 0 )
   {
     cout << " No Table : svtPedestals  " << endl;
     return(0);
   };

  if (!mSvtConfig) {
    gMessMgr->Warning() << " No Configuration information! Impossible to retrieve pedestals! " << endm;
    return 0;
  }
  
  //
  // fetch the data & use it.
  //
  numberOfHybrids = mSvtConfig->getTotalNumberOfHybrids();
  int* indexHybrid =  new int[numberOfHybrids];

  // Loop over barrels, ladders, wafers and hybrids
  int i=0;
  for (int barrel = 1;barrel <= mSvtConfig->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtConfig->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtConfig->getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= mSvtConfig->getNumberOfHybrids();hybrid++) {
	  if (mSvtConfig->getHybridIndex(barrel,ladder,wafer,hybrid) >= 0)
	    indexHybrid[i++] = mSvtConfig->getHybridIndex(barrel,ladder,wafer,hybrid);
	}
      }
    }
  }

  svtPedTable->setElementID(indexHybrid,numberOfHybrids); // set element ID to hybridIndex
  mDbMgr->setVerbose(true);
  mDbMgr->fetchDbTable(svtPedTable);
  mDbMgr->setVerbose(false);

  svtPedestals_st *pedestals  = (svtPedestals_st*)svtPedTable->GetTable() ;

  // Create all pedestal objects
  StSvtHybridCollection* svtPed = new StSvtHybridCollection(mSvtConfig);

  for (int barrel = 1;barrel <= mSvtConfig->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtConfig->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtConfig->getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= mSvtConfig->getNumberOfHybrids();hybrid++) {

	  index = mSvtConfig->getHybridIndex(barrel,ladder,wafer,hybrid);

	  if (index < 0) continue;

	  hybridPed = new StSvtHybridPed(barrel,ladder,wafer,hybrid);
	  hybridPed->reset();

	  // loop over anodes
	  //for (int anode = 1; anode <= mSvtConfig->getNumberOfAnodes(); anode++)
	  //  for (int time = 0; time < mSvtConfig->getNumberOfTimeBins(); time++) {
	  for (int anode = 1; anode <= 240; anode++)
	    for (int time = 0; time < 128; time++) {
	      hybridPed->addToPixel(anode,time,pedestals[index].pedestal[anode-1][time]);
	      if ((anode == 180) && (time == 64)) {
	      //if (index == 6) {
		//cout << "index = " << index << ", sigma = " << pedestals[index].rms << endl;
		cout << anode << "  " << time << "  " << pedestals[index].pedestal[anode-1][time] << endl;
	      }
	    }

	  hybridPed->setRMS(pedestals[index].rms);

	  svtPed->put_at(hybridPed,index);

	} // end of loop over hybrids
      } // end of loop over wafers
    } // end of loop over ladders
  } // end of loop over barrels

  return svtPed;
}

//_____________________________________________________________________________
StSvtGeometry* StSvtDbReader::getGeometry()
{
  //-> add a table to the container with descriptor given by Database
  //
  StDbTable* svtGeomTable ;

  svtGeomTable = mConfigGeom -> addDbTable("svtWafersPosition");
  if (svtGeomTable == 0 )
   {
     cout << " No Table : svtWafersPosition" << endl;
     return(0);
   };

  if (!mSvtConfig) {
    gMessMgr->Warning() << " No Geometry information!" << endm;
    return 0;
  }
  
  //
  // fetch the data & use it.
  //
  int numberOfWafers = (int)mSvtConfig->getTotalNumberOfHybrids()/2;
  int* indexWafer =  new int[numberOfWafers];

  // Loop over barrels, ladders, wafers and hybrids
  int i=0;
  for (int barrel = 1;barrel <= mSvtConfig->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtConfig->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtConfig->getNumberOfWafers(barrel);wafer++) {
	  if (mSvtConfig->getHybridIndex(barrel,ladder,wafer,1) >= 0)
	    indexWafer[i++] = (int)mSvtConfig->getHybridIndex(barrel,ladder,wafer,1)/2;
      }
    }
  }

  svtGeomTable->setElementID(indexWafer,numberOfWafers); // set element ID to hybridIndex
  mDbMgr->setVerbose(true);
  mDbMgr->fetchDbTable(svtGeomTable);
  mDbMgr->setVerbose(false);

  svtWafersPosition_st *position  = (svtWafersPosition_st*)svtGeomTable->GetTable() ;

  svtGeomTable = mConfigGeom -> addDbTable("svtDimensions");
  if (svtGeomTable == 0 )
   {
     cout << " No Table : svtDimensions " << endl;
     return(0);
   };

  //
  // fetch the data & use it.
  //
  //svtGeomTable->setElementID(0,1); // set element ID to hybridIndex
  mDbMgr->setVerbose(true);
  mDbMgr->fetchDbTable(svtGeomTable);
  mDbMgr->setVerbose(false);

  svtDimensions_st *dimension  = (svtDimensions_st*)svtGeomTable->GetTable() ;

  // Create all geometry objects
  StSvtGeometry* svtGeom = new StSvtGeometry(mSvtConfig);
  StSvtWaferGeometry* waferGeom;
  int index;

  svtGeom->setBarrelRadius(dimension->barrelRadius);
  svtGeom->setWaferLength(dimension->waferLength);
  svtGeom->setWaferThickness(dimension->waferThickness);
  svtGeom->setWaferWidth(dimension->waferWidth);
  //svtGeom->setAnodePitch(dimension->anodePitch);
  svtGeom->setAnodePitch(0.025);
  svtGeom->setFocusRegionLength(dimension->focusRegionLength);
  svtGeom->setDistanceInjector(dimension->distanceInjector);
  svtGeom->setLaserPosition(dimension->laserPosition);

  for (int barrel = 1;barrel <= svtGeom->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= svtGeom->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= svtGeom->getNumberOfWafers(barrel);wafer++) {

	  index = svtGeom->getWaferIndex(barrel,ladder,wafer);

	  if (index < 0) continue;

	  waferGeom = new StSvtWaferGeometry(barrel,ladder,wafer);

	  waferGeom->setID(position[index].ID);
	  waferGeom->setDriftDirection(position[index].driftDirection[0],position[index].driftDirection[1],position[index].driftDirection[2]);
	  waferGeom->setNormalDirection(position[index].normalDirection[0],position[index].normalDirection[1],position[index].normalDirection[2]);
	  waferGeom->setTransverseDirection(position[index].transverseDirection[0],position[index].transverseDirection[1],position[index].transverseDirection[2]);
	  waferGeom->setCenterPosition(position[index].centerPosition[0],position[index].centerPosition[1],position[index].centerPosition[2]);

	  svtGeom->put_at(waferGeom,index);

	  //cout << position[index].driftDirection[0] << "  " << waferGeom->d(0) << endl;

      } // end of loop over wafers
    } // end of loop over ladders
  } // end of loop over barrels

  return svtGeom;
}

//_____________________________________________________________________________
StSvtHybridCollection* StSvtDbReader::getRms()
{
  return NULL;
}

//_____________________________________________________________________________
StSvtHybridCollection* StSvtDbReader::getDriftCurve()
{
  return NULL;
}

//_____________________________________________________________________________
StSvtHybridCollection* StSvtDbReader::getAnodeDriftCorr()
{
  return NULL;
}  

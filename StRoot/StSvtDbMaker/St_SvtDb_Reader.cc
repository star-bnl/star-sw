/***************************************************************************
 *
 * $Id: St_SvtDb_Reader.cc,v 1.1 2001/10/29 18:53:14 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT DB access Maker
 *
 ***************************************************************************
 *
 * $Log: St_SvtDb_Reader.cc,v $
 * Revision 1.1  2001/10/29 18:53:14  munhoz
 * starting SVT Data base
 *
 *
 **************************************************************************/

#include "St_SvtDb_Reader.hh"

#include "StMessMgr.h"

#include "StSvtClassLibrary/StSvtConfig.hh"
#include "StSvtClassLibrary/StSvtHybridPed.hh"
#include "StSvtClassLibrary/StSvtHybridDriftVelocity.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClassLibrary/StSvtGeometry.hh"
#include "StSvtClassLibrary/StSvtWaferGeometry.hh"

#include "tables/St_svtConfiguration_Table.h"
#include "tables/St_svtDriftVelocity_Table.h"
#include "tables/St_svtPedestals_Table.h"
#include "tables/St_svtWafersPosition_Table.h"
#include "tables/St_svtDimensions_Table.h"

#ifdef __ROOT__
ClassImp(St_SvtDb_Reader)
#endif

//_____________________________________________________________________________
St_SvtDb_Reader::St_SvtDb_Reader()
{}

//_____________________________________________________________________________
St_SvtDb_Reader::~St_SvtDb_Reader()
{}

//_____________________________________________________________________________
void St_SvtDb_Reader::setDataBase(St_DataSet* input, dbSvtType type)
{
  if (input)
    svtDb[type] = input;    
  else
    gMessMgr->Message("Error setting St_SvtDb_Reader: Need to specify input DataSet","E");  
}

//_____________________________________________________________________________
StSvtConfig* St_SvtDb_Reader::getConfiguration()
{
  cout << "St_SvtDb_Reader::getConfiguration" << endl;

  St_svtConfiguration *configuration;
  const int dbIndex = kGeometry;
  if (svtDb[dbIndex]){
    configuration = (St_svtConfiguration*)svtDb[dbIndex]->Find("svtConfiguration");
    if (!(configuration && configuration->HasData()) ){
     gMessMgr->Message("Error Finding SVT Configuration","E");
     return 0;
    }
  }
  else {
    gMessMgr->Message("Error Finding SVT Geometry Db","E");
    return 0;
  }

  svtConfiguration_st* config = configuration->GetTable();

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

  return mSvtConfig;
}

//_____________________________________________________________________________
StSvtHybridCollection* St_SvtDb_Reader::getDriftVelocity()
{
  cout << "St_SvtDb_Reader::getDriftVelocity" << endl;

  St_svtDriftVelocity *driftVelocity;
  const int dbIndex = kCalibration;
  if (svtDb[dbIndex]){
    driftVelocity = (St_svtDriftVelocity*)svtDb[dbIndex]->Find("svtDriftVelocity");
    if (!(driftVelocity && driftVelocity->HasData()) ){
     gMessMgr->Message("Error Finding SVT Drift Velocity","E");
     return 0;
    }
  }
  else {
    gMessMgr->Message("Error Finding SVT Calibration Db","E");
    return 0;
  }

  svtDriftVelocity_st *driftVeloc = driftVelocity->GetTable();

  // Create all drift velocity objects
  StSvtHybridCollection* svtDriftVeloc = new StSvtHybridCollection(mSvtConfig);
  StSvtHybridDriftVelocity* hybridDriftVeloc;
  int index;

  for (int barrel = 1;barrel <= mSvtConfig->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtConfig->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtConfig->getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= mSvtConfig->getNumberOfHybrids();hybrid++) {

	  index = mSvtConfig->getHybridIndex(barrel,ladder,wafer,hybrid);

	  if (index < 0) continue;

	  hybridDriftVeloc = new StSvtHybridDriftVelocity(barrel,ladder,wafer,hybrid);
	  
	  // loop over anodes
	  for (int anode=1;anode<=mSvtConfig->getNumberOfAnodes();anode++) {
	    hybridDriftVeloc->setV1(anode,driftVeloc[index].injectionDriftVelocityFocus[anode-1]);
	    hybridDriftVeloc->setV2(anode,driftVeloc[index].injectionDriftVelocity[anode-1]);
	    hybridDriftVeloc->setV3(anode,driftVeloc[index].averageDriftVelocity[anode-1]);
	  }

	  svtDriftVeloc->put_at(hybridDriftVeloc,index);

	} // end of loop over hybrids
      } // end of loop over wafers
    } // end of loop over ladders
  } // end of loop over barrels

  return svtDriftVeloc;
}

//_____________________________________________________________________________
StSvtHybridCollection* St_SvtDb_Reader::getPedestals()
{
  cout << "St_SvtDb_Reader::getPedestals" << endl;

  St_svtPedestals *pedestals;
  const int dbIndex = kCalibration;
  if (svtDb[dbIndex]){
    pedestals = (St_svtPedestals*)svtDb[dbIndex]->Find("svtPedestals");
    if (!(pedestals && pedestals->HasData()) ){
     gMessMgr->Message("Error Finding SVT Pedestals","E");
     return 0;
    }
  }
  else {
    gMessMgr->Message("Error Finding SVT Calibration Db","E");
    return 0;
  }

  svtPedestals_st *pedestal = pedestals->GetTable();

  // Create all pedestal objects
  StSvtHybridCollection* svtPed = new StSvtHybridCollection(mSvtConfig);
  StSvtHybridPed* hybridPed;
  int index;

  for (int barrel = 1;barrel <= mSvtConfig->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtConfig->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtConfig->getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= mSvtConfig->getNumberOfHybrids();hybrid++) {

	  index = mSvtConfig->getHybridIndex(barrel,ladder,wafer,hybrid);

	  if (index < 0) continue;

	  hybridPed = new StSvtHybridPed(barrel,ladder,wafer,hybrid);
	  hybridPed->reset();

	  // loop over anodes
	  for (int anode = 1; anode <= mSvtConfig->getNumberOfAnodes(); anode++)
	    for (int time = 0; time < mSvtConfig->getNumberOfTimeBins(); time++) {
	      hybridPed->addToPixel(anode,time,pedestal[index].pedestal[anode-1][time]);
	      cout << anode << "  " << time << "  " << pedestal[index].pedestal[anode-1][time] << endl;
	    }

	  svtPed->put_at(hybridPed,index);

	} // end of loop over hybrids
      } // end of loop over wafers
    } // end of loop over ladders
  } // end of loop over barrels

  return svtPed;
}

//_____________________________________________________________________________
StSvtGeometry* St_SvtDb_Reader::getGeometry()
{
  // get svt dimensions table
  St_svtDimensions *dimensions;
  const int dbIndex = kGeometry;
  if (svtDb[dbIndex]){
    dimensions = (St_svtDimensions*)svtDb[dbIndex]->Find("svtDimensions");
    if (!(dimensions && dimensions->HasData()) ){
     gMessMgr->Message("Error Finding SVT Dimensions","E");
     return 0;
    }
  }
  else {
    gMessMgr->Message("Error Finding SVT Geometry Db","E");
    return 0;
  }

  svtDimensions_st *dimension = dimensions->GetTable();

  // Create all pedestal objects
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

  char path[100];
  St_svtWafersPosition *positions;
  svtWafersPosition_st *position;

  for (int barrel = 1;barrel <= svtGeom->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= svtGeom->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= svtGeom->getNumberOfWafers(barrel);wafer++) {

	  index = svtGeom->getWaferIndex(barrel,ladder,wafer);

	  if (index < 0) continue;

	  switch (barrel) {
	  case 1:
	    sprintf(path,"InnerBarrel/Ladder_0%d/Wafer_0%d/svtWafersPosition",ladder,wafer);
	    break;
	  case 2:
	    if (ladder < 10)
	      sprintf(path,"MiddleBarrel/Ladder_0%d/Wafer_0%d/svtWafersPosition",ladder,wafer);
	    else
	      sprintf(path,"MiddleBarrel/Ladder_%d/Wafer_0%d/svtWafersPosition",ladder,wafer);
	    break;
	  case 3:
	    if (ladder < 10)
	      sprintf(path,"OuterBarrel/Ladder_0%d/Wafer_0%d/svtWafersPosition",ladder,wafer);
	    else
	      sprintf(path,"OuterBarrel/Ladder_%d/Wafer_0%d/svtWafersPosition",ladder,wafer);
	    break;
	  }

	  // get wafers position table
	  if (svtDb[dbIndex]){
	    positions = (St_svtWafersPosition*)svtDb[dbIndex]->Find(path);
	    if (!(positions && positions->HasData()) ){
	      gMessMgr->Message("Error Finding SVT Positions","E");
	      return 0;
	    }
	  }
	  else {
	    gMessMgr->Message("Error Finding SVT Geometry Db","E");
	    return 0;
	  }
	  
	  position = positions->GetTable();

	  // fill StSvtGeometry object
	  waferGeom = new StSvtWaferGeometry(barrel,ladder,wafer);

	  waferGeom->setDriftDirection(position->driftDirection[0],position->driftDirection[1],position->driftDirection[2]);
	  waferGeom->setNormalDirection(position->normalDirection[0],position->normalDirection[1],position->normalDirection[2]);
	  waferGeom->setTransverseDirection(position->transverseDirection[0],position->transverseDirection[1],position->transverseDirection[2]);
	  waferGeom->setCenterPosition(position->centerPosition[0],position->centerPosition[1],position->centerPosition[2]);

	  cout << "index = "  << index << ", x = " << position->centerPosition[0] << endl;

	  svtGeom->put_at(waferGeom,index);

      } // end of loop over wafers
    } // end of loop over ladders
  } // end of loop over barrels

  return svtGeom;
}

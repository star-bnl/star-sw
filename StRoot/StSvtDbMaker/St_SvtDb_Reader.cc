/***************************************************************************
 *
 * $Id: St_SvtDb_Reader.cc,v 1.11 2004/07/29 01:36:00 caines Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT DB access Maker
 *
 ***************************************************************************
 *
 * $Log: St_SvtDb_Reader.cc,v $
 * Revision 1.11  2004/07/29 01:36:00  caines
 * Changes for the drift curve usage
 *
 * Revision 1.10  2004/07/26 00:06:08  munhoz
 * read drift curve
 *
 * Revision 1.9  2004/03/30 21:16:18  caines
 * Get daq parameters
 *
 * Revision 1.8  2004/01/30 07:22:07  munhoz
 * adding rms and daq parameters reading
 *
 * Revision 1.7  2004/01/29 17:24:39  perev
 * Replace assert to delete
 *
 * Revision 1.6  2004/01/27 02:37:54  perev
 * LeakOff
 *
 * Revision 1.5  2003/04/14 15:51:53  munhoz
 * reading t0 from DB
 *
 * Revision 1.4  2002/05/06 00:42:51  munhoz
 * adding bad anode list reading
 *
 * Revision 1.3  2002/02/15 22:45:43  munhoz
 * introducing drift velocity reading capability
 *
 * Revision 1.2  2002/02/05 23:30:52  caines
 * fixing configuration bug
 *
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
#include "StSvtClassLibrary/StSvtHybridDriftCurve.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClassLibrary/StSvtHybridBadAnodes.hh"
#include "StSvtClassLibrary/StSvtGeometry.hh"
#include "StSvtClassLibrary/StSvtWaferGeometry.hh"
#include "StSvtClassLibrary/StSvtT0.hh"
#include "StSvtClassLibrary/StSvtDaq.hh"

#include "tables/St_svtConfiguration_Table.h"
#include "tables/St_svtDriftVelAvg_Table.h"
#include "tables/St_svtDriftCurve_Table.h"
#include "tables/St_svtBadAnodes_Table.h"
#include "tables/St_svtPedestals_Table.h"
#include "tables/St_svtRms_Table.h"
#include "tables/St_svtWafersPosition_Table.h"
#include "tables/St_svtDimensions_Table.h"
#include "tables/St_svtElectronics_Table.h"
#include "tables/St_svtDaq_Table.h"

#ifdef __ROOT__
ClassImp(St_SvtDb_Reader)
#endif

svtElectronics_st *electronic = NULL;

//_____________________________________________________________________________
St_SvtDb_Reader::St_SvtDb_Reader()
{
  memset(svtDb,0,sizeof(svtDb));
  mSvtConfig = NULL;      
  mSvtDriftVeloc = NULL;
  mSvtDriftCurve = NULL;
  mSvtPed = NULL; 
  mSvtRms = NULL; 
  mSvtGeom = NULL;        
  mSvtBadAnodes = NULL; 
  mSvtT0 = NULL;                
  mSvtDaq = NULL;
}

//_____________________________________________________________________________
St_SvtDb_Reader::~St_SvtDb_Reader()
{
  if (mSvtDriftVeloc)
    delete mSvtDriftVeloc;
  if (mSvtConfig)
    delete mSvtConfig;
  if (mSvtPed)
    delete mSvtPed; 
  if (mSvtRms)
    delete mSvtRms; 
  if (mSvtGeom)
    delete mSvtGeom;        
  if (mSvtBadAnodes)
    delete mSvtBadAnodes; 
  if (mSvtT0)
    delete mSvtT0;                
  if (mSvtDaq)
    delete mSvtDaq;
}

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
  gMessMgr->Info() << "St_SvtDb_Reader::getConfiguration" << endm;

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

  gMessMgr->Info() << "numberOfBarrels = " << config->numberOfBarrels << endm;
  gMessMgr->Info() << "numberOfLadders = "  << config->numberOfLadders << endm;
  gMessMgr->Info() << "numberOfWafers = "  << config->numberOfWafers << endm;
  gMessMgr->Info() << "numberOfHybrids = "  << config->numberOfHybrids << endm;
  
  gMessMgr->Info() << "numberOfLaddersPerBarrel[0] = "  << config->numberOfLaddersPerBarrel[0] << endm;
  gMessMgr->Info() << "numberOfLaddersPerBarrel[1] = "  << config->numberOfLaddersPerBarrel[1] << endm;
  gMessMgr->Info() << "numberOfLaddersPerBarrel[2] = "  << config->numberOfLaddersPerBarrel[2] << endm;
  gMessMgr->Info() << "numberOfWafersPerLadder[0] = "  << config->numberOfWafersPerLadder[0] << endm;
  gMessMgr->Info() << "numberOfWafersPerLadder[1] = "  << config->numberOfWafersPerLadder[1] << endm;
  gMessMgr->Info() << "numberOfWafersPerLadder[2] = "  << config->numberOfWafersPerLadder[2] << endm;
  gMessMgr->Info() << "numberOfHybridsPerWafer = "  << config->numberOfHybridsPerWafer << endm;

  if (!mSvtConfig)
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
StSvtHybridCollection* St_SvtDb_Reader::getDriftVelocity()
{
  //  gMessMgr->Info() << "St_SvtDb_Reader::getDriftVelocity" << endm;

  if(!mSvtDriftVeloc)
    mSvtDriftVeloc = new StSvtHybridCollection(mSvtConfig);

  getDriftVelocityAverage(mSvtDriftVeloc);

  return mSvtDriftVeloc;
}

//_____________________________________________________________________________
void St_SvtDb_Reader::getDriftVelocityAverage(StSvtHybridCollection* svtDriftVeloc)
{
  gMessMgr->Info() << "St_SvtDb_Reader::getDriftVelocityAverage" << endm;

  St_svtDriftVelAvg *driftVelocity;
  const int dbIndex = kCalibration;

  svtDriftVelAvg_st *driftVeloc;
  StSvtHybridDriftVelocity* hybridDriftVeloc;

  char path[100];
  int index;

  for (int barrel = 1;barrel <= mSvtConfig->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtConfig->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtConfig->getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= mSvtConfig->getNumberOfHybrids();hybrid++) {

	  index = mSvtConfig->getHybridIndex(barrel,ladder,wafer,hybrid);

	  if (index < 0) continue;

	  switch (barrel) {
	  case 1:
	    sprintf(path,"InnerBarrel/Ladder_0%d/Wafer_0%d/Hybrid_0%d/svtDriftVelAvg",ladder,wafer,hybrid);
	    break;
	  case 2:
	    if (ladder < 10)
	      sprintf(path,"MiddleBarrel/Ladder_0%d/Wafer_0%d/Hybrid_0%d/svtDriftVelAvg",ladder,wafer,hybrid);
	    else
	      sprintf(path,"MiddleBarrel/Ladder_%d/Wafer_0%d/Hybrid_0%d/svtDriftVelAvg",ladder,wafer,hybrid);
	    break;
	  case 3:
	    if (ladder < 10)
	      sprintf(path,"OuterBarrel/Ladder_0%d/Wafer_0%d/Hybrid_0%d/svtDriftVelAvg",ladder,wafer,hybrid);
	    else
	      sprintf(path,"OuterBarrel/Ladder_%d/Wafer_0%d/Hybrid_0%d/svtDriftVelAvg",ladder,wafer,hybrid);
	    break;
	  }

	  // get wafers position table
	  if (svtDb[dbIndex]){
	    driftVelocity = (St_svtDriftVelAvg*)svtDb[dbIndex]->Find(path);
	    if (!(driftVelocity && driftVelocity->HasData()) ){
	      gMessMgr->Message("Error Finding SVT drift velocity average","E");
	      return;
	    }
	  }
	  else {
	    gMessMgr->Message("Error Finding SVT Calibration Db","E");
	    return;
	  }

	  driftVeloc = driftVelocity->GetTable();

	  hybridDriftVeloc = (StSvtHybridDriftVelocity*)svtDriftVeloc->at(index);
	  if (!hybridDriftVeloc)
	    hybridDriftVeloc = new StSvtHybridDriftVelocity(barrel,ladder,wafer,hybrid);
	  
	  // loop over anodes
	  for (int anode=1;anode<=mSvtConfig->getNumberOfAnodes();anode++) {
	    hybridDriftVeloc->setV3(driftVeloc->averageDriftVelocity,anode);
	  }

	  if ((index==0) || (index==431)) 
	    gMessMgr->Info() << "index = " << index << ", averageDriftVelocity = " << driftVeloc->averageDriftVelocity << endm;

	  mSvtDriftVeloc->put_at(hybridDriftVeloc,index);

	} // end of loop over hybrids
      } // end of loop over wafers
    } // end of loop over ladders
  } // end of loop over barrels

}

//_____________________________________________________________________________
StSvtHybridCollection* St_SvtDb_Reader::getDriftCurve()
{
  gMessMgr->Info() << "St_SvtDb_Reader::getDriftVelocityCurve" << endm;

  if(!mSvtDriftCurve)
    mSvtDriftCurve = new StSvtHybridCollection(mSvtConfig);

  St_svtDriftCurve *driftVelocityCurve;
  const int dbIndex = kCalibration;

  svtDriftCurve_st *driftCurve;
  StSvtHybridDriftCurve* hybridDriftCurve;

  char path[100];
  int index;

  for (int barrel = 1;barrel <= mSvtConfig->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtConfig->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtConfig->getNumberOfWafers(barrel);wafer++) {
        for (int hybrid = 1;hybrid <= mSvtConfig->getNumberOfHybrids();hybrid++) {

          index = mSvtConfig->getHybridIndex(barrel,ladder,wafer,hybrid);
										    
          if (index < 0) continue;

            switch (barrel) {
            case 1:
	      sprintf(path,"InnerBarrel/Ladder_0%d/Wafer_0%d/Hybrid_0%d/svtDriftCurve",ladder,wafer,hybrid);
	      break;
	    case 2:
              if (ladder < 10)
                sprintf(path,"MiddleBarrel/Ladder_0%d/Wafer_0%d/Hybrid_0%d/svtDriftCurve",ladder,wafer,hybrid);
              else
                sprintf(path,"MiddleBarrel/Ladder_%d/Wafer_0%d/Hybrid_0%d/svtDriftCurve",ladder,wafer,hybrid);
              break;
	    case 3:
	      if (ladder < 10)
	        sprintf(path,"OuterBarrel/Ladder_0%d/Wafer_0%d/Hybrid_0%d/svtDriftCurve",ladder,wafer,hybrid);
              else
                sprintf(path,"OuterBarrel/Ladder_%d/Wafer_0%d/Hybrid_0%d/svtDriftCurve",ladder,wafer,hybrid);
              break;
          }

          // get wafers position table
	  if (svtDb[dbIndex]){
	    driftVelocityCurve = (St_svtDriftCurve*)svtDb[dbIndex]->Find(path);
	    if (!(driftVelocityCurve && driftVelocityCurve->HasData()) ){
	      gMessMgr->Message("Error Finding SVT drift velocity curve","E");
	      return NULL;
	    }
	  }
	  else {
            gMessMgr->Message("Error Finding SVT Calibration Db","E");
            return NULL;
          }

          driftCurve = driftVelocityCurve->GetTable();

          hybridDriftCurve = (StSvtHybridDriftCurve*)mSvtDriftCurve->at(index);
          if (!hybridDriftCurve)
            hybridDriftCurve = new StSvtHybridDriftCurve(barrel,ladder,wafer,hybrid);

          // loop over data
          for (int i=1; i<=3; i++)
	    for (int j=1; j<=10; j++) {
	      hybridDriftCurve->setParameter(i,j,driftCurve->driftCurve[i-1][j-1]);
	      // cout << "adc = " << i << ", parameter = " << j << ", value = " << driftCurve->driftCurve[i-1][j-1] << endl;
	    }
	  
          mSvtDriftCurve->put_at(hybridDriftCurve,index);

        } // end of loop over hybrids
      } // end of loop over wafers
    } // end of loop over ladders
  } // end of loop over barrels

  return mSvtDriftCurve;
}
    
//_____________________________________________________________________________
StSvtHybridCollection* St_SvtDb_Reader::getPedestals()
{
  gMessMgr->Info() << "St_SvtDb_Reader::getPedestals" << endm;

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
  if (!mSvtPed)
    mSvtPed = new StSvtHybridCollection(mSvtConfig);
  StSvtHybridPed* hybridPed;
  int index;

  for (int barrel = 1;barrel <= mSvtConfig->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtConfig->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtConfig->getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= mSvtConfig->getNumberOfHybrids();hybrid++) {

	  index = mSvtConfig->getHybridIndex(barrel,ladder,wafer,hybrid);

	  if (index < 0) continue;

	  hybridPed = (StSvtHybridPed*)mSvtPed->at(index);
	  if (!hybridPed)
	    hybridPed = new StSvtHybridPed(barrel,ladder,wafer,hybrid);
	  hybridPed->reset();

	  // loop over anodes
	  for (int anode = 1; anode <= mSvtConfig->getNumberOfAnodes(); anode++)
	    for (int time = 0; time < mSvtConfig->getNumberOfTimeBins(); time++) {
	      hybridPed->addToPixel(anode,time,pedestal[index].pedestal[anode-1][time]);
	      //gMessMgr->Info() << anode << "  " << time << "  " << pedestal[index].pedestal[anode-1][time] << endm;
	    }

	  mSvtPed->put_at(hybridPed,index);

	} // end of loop over hybrids
      } // end of loop over wafers
    } // end of loop over ladders
  } // end of loop over barrels

  return mSvtPed;
}


//_____________________________________________________________________________
StSvtHybridCollection* St_SvtDb_Reader::getRms()
{
  gMessMgr->Info() << "St_SvtDb_Reader::getRms" << endm;

  St_svtRms *st_rms;
  const int dbIndex = kCalibration;
  if (svtDb[dbIndex]){
    st_rms = (St_svtRms*)svtDb[dbIndex]->Find("svtRms");
    if (!(st_rms && st_rms->HasData()) ){
     gMessMgr->Message("Error Finding SVT RMS","E");
     return 0;
    }
  }
  else {
    gMessMgr->Message("Error Finding SVT Calibration Db","E");
    return 0;
  }

  svtRms_st *rms = st_rms->GetTable();

  // Create all pedestal objects
  if (!mSvtRms)
    mSvtRms = new StSvtHybridCollection(mSvtConfig);
  StSvtHybridPixels* hybridRms;
  int index;

  for (int barrel = 1;barrel <= mSvtConfig->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtConfig->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtConfig->getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= mSvtConfig->getNumberOfHybrids();hybrid++) {

	  index = mSvtConfig->getHybridIndex(barrel,ladder,wafer,hybrid);

	  if (index < 0) continue;

	  hybridRms = (StSvtHybridPixels*)mSvtRms->at(index);
	  if (!hybridRms)
	    hybridRms = new StSvtHybridPixels(barrel,ladder,wafer,hybrid);
	  hybridRms->reset();

	  // loop over anodes
	  for (int anode = 1; anode <= mSvtConfig->getNumberOfAnodes(); anode++)
	    for (int time = 0; time < mSvtConfig->getNumberOfTimeBins(); time++) {
	      hybridRms->addToPixel(anode,time,rms[index].rms[anode-1][time]);
	      //gMessMgr->Info() << anode << "  " << time << "  " << rms[index].rms[anode-1][time] << endm;
	    }

	  mSvtRms->put_at(hybridRms,index);

	} // end of loop over hybrids
      } // end of loop over wafers
    } // end of loop over ladders
  } // end of loop over barrels

  return mSvtRms;
}

//_____________________________________________________________________________
StSvtGeometry* St_SvtDb_Reader::getGeometry()
{
  gMessMgr->Info() << "St_SvtDb_Reader::getGeometry" << endm;

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
  if (!mSvtGeom)  
    mSvtGeom = new StSvtGeometry(mSvtConfig);
  StSvtWaferGeometry* waferGeom;
  int index;

  mSvtGeom->setBarrelRadius(dimension->barrelRadius);
  mSvtGeom->setWaferLength(dimension->waferLength);
  mSvtGeom->setWaferThickness(dimension->waferThickness);
  mSvtGeom->setWaferWidth(dimension->waferWidth);
  //mSvtGeom->setAnodePitch(dimension->anodePitch);
  mSvtGeom->setAnodePitch(0.025);
  mSvtGeom->setFocusRegionLength(dimension->focusRegionLength);
  mSvtGeom->setDistanceInjector(dimension->distanceInjector);
  mSvtGeom->setLaserPosition(dimension->laserPosition);

  char path[100];
  St_svtWafersPosition *positions;
  svtWafersPosition_st *position;

  for (int barrel = 1;barrel <= mSvtGeom->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtGeom->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtGeom->getNumberOfWafers(barrel);wafer++) {

	  index = mSvtGeom->getWaferIndex(barrel,ladder,wafer);

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
	  waferGeom = (StSvtWaferGeometry*)mSvtGeom->at(index);
	  if (!waferGeom)
	    waferGeom = new StSvtWaferGeometry(barrel,ladder,wafer);

	  waferGeom->setDriftDirection(position->driftDirection[0],position->driftDirection[1],position->driftDirection[2]);
	  waferGeom->setNormalDirection(position->normalDirection[0],position->normalDirection[1],position->normalDirection[2]);
	  waferGeom->setTransverseDirection(position->transverseDirection[0],position->transverseDirection[1],position->transverseDirection[2]);
	  waferGeom->setCenterPosition(position->centerPosition[0],position->centerPosition[1],position->centerPosition[2]);

	  if ((index<4) || (index>208)) 
	    gMessMgr->Info() << "index = "  << index << ", x = " << position->centerPosition[0] << endm;

	  mSvtGeom->put_at(waferGeom,index);

      } // end of loop over wafers
    } // end of loop over ladders
  } // end of loop over barrels

  return mSvtGeom;
}

//_____________________________________________________________________________
StSvtHybridCollection* St_SvtDb_Reader::getBadAnodes()
{
  gMessMgr->Info() << "St_SvtDb_Reader::getBadAnodes" << endm;

  // get svt dimensions table
  St_svtBadAnodes *badAnodes;
  svtBadAnodes_st *badAnode;
  const int dbIndex = kCalibration;

  // Create all pedestal objects
  if (!mSvtBadAnodes)
    mSvtBadAnodes = new StSvtHybridCollection(mSvtConfig);
  StSvtHybridBadAnodes* hybridBadAnodes;

  char path[100];
  int index;

  for (int barrel = 1;barrel <= mSvtConfig->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtConfig->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtConfig->getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= mSvtConfig->getNumberOfHybrids();hybrid++) {

	  index = mSvtConfig->getHybridIndex(barrel,ladder,wafer,hybrid);

	  if (index < 0) continue;

	  switch (barrel) {
	  case 1:
	    sprintf(path,"InnerBarrel/Ladder_0%d/Wafer_0%d/Hybrid_0%d/svtBadAnodes",ladder,wafer,hybrid);
	    break;
	  case 2:
	    if (ladder < 10)
	      sprintf(path,"MiddleBarrel/Ladder_0%d/Wafer_0%d/Hybrid_0%d/svtBadAnodes",ladder,wafer,hybrid);
	    else
	      sprintf(path,"MiddleBarrel/Ladder_%d/Wafer_0%d/Hybrid_0%d/svtBadAnodes",ladder,wafer,hybrid);
	    break;
	  case 3:
	    if (ladder < 10)
	      sprintf(path,"OuterBarrel/Ladder_0%d/Wafer_0%d/Hybrid_0%d/svtBadAnodes",ladder,wafer,hybrid);
	    else
	      sprintf(path,"OuterBarrel/Ladder_%d/Wafer_0%d/Hybrid_0%d/svtBadAnodes",ladder,wafer,hybrid);
	    break;
	  }

	  // get wafers position table
	  if (svtDb[dbIndex]){
	    badAnodes = (St_svtBadAnodes*)svtDb[dbIndex]->Find(path);
	    if (!(badAnodes && badAnodes->HasData()) ){
	      gMessMgr->Message("Error Finding SVT bad anodes","E");
	      return 0;
	    }
	  }
	  else {
	    gMessMgr->Message("Error Finding SVT Calibration Db","E");
	    return 0;
	  }

	  badAnode = badAnodes->GetTable();

	  hybridBadAnodes = (StSvtHybridBadAnodes*)mSvtBadAnodes->at(index);
	  if (!hybridBadAnodes)
	    hybridBadAnodes = new StSvtHybridBadAnodes(barrel,ladder,wafer,hybrid);
	  
	  // loop over anodes
	  for (int anode=1;anode<=mSvtConfig->getNumberOfAnodes();anode++) {
	    if (badAnode->isBadAnode[anode-1]) {
	      hybridBadAnodes->setBadAnode(anode);
	      //gMessMgr->Info() << "hybrid = "<< index << ", anode = " << anode << endm;
	    }
	  }

	  mSvtBadAnodes->put_at(hybridBadAnodes,index);

	} // end of loop over hybrids
      } // end of loop over wafers
    } // end of loop over ladders
  } // end of loop over barrels

  return mSvtBadAnodes;
}

//_____________________________________________________________________________
int St_SvtDb_Reader::getElectronics()
{
  // get svt electronics table
  St_svtElectronics *electronics;
  const int dbIndex = kCalibration;
  if (svtDb[dbIndex]){
    electronics = (St_svtElectronics*)svtDb[dbIndex]->Find("svtElectronics");
    if (!(electronics && electronics->HasData()) ){
     gMessMgr->Message("Error Finding SVT Electronics","E");
     return kFALSE;
    }
  }
  else {
    gMessMgr->Message("Error Finding SVT Geometry Db","E");
    return kFALSE;
  }

  electronic = electronics->GetTable();
  return kTRUE;
}

//_____________________________________________________________________________
StSvtT0* St_SvtDb_Reader::getT0()
{
  if (!mSvtT0)
    mSvtT0 = new StSvtT0();

  if (getElectronics()) {
    for (int i=0;i<24;i++)
      mSvtT0->setT0(electronic->tZero[i],i+1);
    mSvtT0->setFsca(electronic->samplingFrequency);

    gMessMgr->Info() << "t0 = " << mSvtT0->getT0(1) << ", fsca =  " << mSvtT0->getFsca() << endm;

  }

  return mSvtT0;
}


//_____________________________________________________________________________
StSvtDaq* St_SvtDb_Reader::getDaqParameters()
{
  gMessMgr->Info() << "St_SvtDb_Reader::getDaqParameters" << endm;

  St_svtDaq *daq;
  const int dbIndex = kCalibration;
  if (svtDb[dbIndex]){
    daq = (St_svtDaq*)svtDb[dbIndex]->Find("svtDaq");
    if (!(daq && daq->HasData()) ){
     gMessMgr->Message("Error Finding SVT Daq","E");
     return 0;
    }
  }
  else {
    gMessMgr->Message("Error Finding SVT Geometry Db","E");
    return 0;
  }

  svtDaq_st* daqParam = daq->GetTable();

  gMessMgr->Info() << "clearedTimeBins = " << daqParam->clearedTimeBins<< endm;
  gMessMgr->Info() << "pixelsBefore = " << daqParam->pixelsBefore << endm;
  gMessMgr->Info() << "pixelsAfter = " << daqParam->pixelsAfter << endm;
  gMessMgr->Info() << "pedOffset = " << daqParam->pedOffset << endm;
  gMessMgr->Info() << "seqLo = " << daqParam->seqLo << endm;
  gMessMgr->Info() << "seqHi = " << daqParam->seqHi << endm;
  gMessMgr->Info() << "threshLo = " << daqParam->threshLo << endm;
  gMessMgr->Info() << "threshHi = " << daqParam->threshHi << endm;

  if(!mSvtDaq)
    mSvtDaq = new StSvtDaq();

  mSvtDaq->setClearedTimeBins(daqParam->clearedTimeBins);
  mSvtDaq->setSavedBlackAnodes(daqParam->savedBlackAnodes[0],0);
  mSvtDaq->setSavedBlackAnodes(daqParam->savedBlackAnodes[1],1);
  mSvtDaq->setSavedBlackAnodes(daqParam->savedBlackAnodes[2],2);
  mSvtDaq->setSavedBlackAnodes(daqParam->savedBlackAnodes[2],3);
  mSvtDaq->setPixelsBefore(daqParam->pixelsBefore);
  mSvtDaq->setPixelsAfter(daqParam->pixelsAfter);
  mSvtDaq->setPedOffset(daqParam->pedOffset);
  mSvtDaq->setSeqLo(daqParam->seqLo);
  mSvtDaq->setSeqHi(daqParam->seqHi);
  mSvtDaq->setThreshLo(daqParam->threshLo);
  mSvtDaq->setThreshHi(daqParam->threshHi);

  return mSvtDaq;
}

/***************************************************************************
 *
 * $Id: StDbCalibFactory.cc,v 1.3 1999/09/30 02:06:01 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  StDbTable ctor & list holder for Calibration tables
 *
 ***************************************************************************
 *
 * $Log: StDbCalibFactory.cc,v $
 * Revision 1.3  1999/09/30 02:06:01  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#include "StDbCalibFactory.hh"
#include "StDbCalibSchema.h"


StDbCalibFactory* StDbCalibFactory::mInstance=0; 

/////////////////////////////////////////////////////////

void
StDbCalibFactory::initIDList(){

  if(isloaded) return;

  mIDList.push_back(new StDbTableID("tpcDriftVelocity",tpcDriftVelocityID));
  mIDList.push_back(new StDbTableID("tpcDedxPidAmpDb",tpcDedxPidAmpDbID));
  mIDList.push_back(new StDbTableID("tpcGainFactors",tpcGainFactorsID));
  mIDList.push_back(new StDbTableID("tpcTimeOffsets",tpcTimeOffsetsID));
  isloaded=true;

}









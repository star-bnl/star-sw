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









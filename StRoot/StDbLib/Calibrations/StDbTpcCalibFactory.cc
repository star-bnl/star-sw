#include "StDbTpcCalibFactory.hh"
#include "StDbTpcCalibTables.hh"


StDbTpcCalibFactory* StDbTpcCalibFactory::mInstance=0; 

StDbTpcCalibFactory::StDbTpcCalibFactory(){
    mdbDomain = Tpc;
    mdbType = Calibrations;
  }


void
StDbTpcCalibFactory::initTableList(){

  mTableList.push_back(new StDb_tpcDriftVelocity("tpcDriftVelocity"));
  mTableList.push_back(new StDb_dedxPidAmpDb("dedxPidAmpDb"));
  isloaded=true;
}







#include "StDbTpcGeomFactory.hh"
#include "StDbTpcGeomTables.hh"

StDbTpcGeomFactory* StDbTpcGeomFactory::mInstance=0;

void
StDbTpcGeomFactory::initTableList(){

  mTableList.push_back(new StDb_tpcWirePlanes("tpcWirePlanes"));
  mTableList.push_back(new StDb_tpcElectronics("tpcElectronics"));
  mTableList.push_back(new StDb_tpcDimensions("tpcDimensions"));
  mTableList.push_back(new StDb_tpcPadPlanes("tpcPadPlanes"));
  isloaded=true;

}




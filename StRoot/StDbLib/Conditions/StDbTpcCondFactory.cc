#include "StDbTpcCondFactory.hh"
#include "StDbTpcCondTables.hh"

StDbTpcCondFactory* StDbTpcCondFactory::mInstance=0; 

void
StDbTpcCondFactory::initTableList(){

  mTableList.push_back(new StDb_tpcGas("tpcGas"));
  mTableList.push_back(new StDb_tpcVoltages("tpcVoltages"));
  isloaded=true;

}





#include "StDbCondFactory.hh"
#include "StDbCondSchema.h"


StDbCondFactory* StDbCondFactory::mInstance=0; 

/////////////////////////////////////////////////////////

void
StDbCondFactory::initIDList(){

  if(isloaded) return;

  mIDList.push_back(new StDbTableID("tpcGas",tpcGasID));
  mIDList.push_back(new StDbTableID("tpcVoltages",tpcVoltagesID));
  isloaded=true;

}









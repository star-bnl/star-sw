#include "StDbRunParamsFactory.hh"
#include "StDbRunParamsSchema.h"


StDbRunParamsFactory* StDbRunParamsFactory::mInstance=0; 

/////////////////////////////////////////////////////////

void
StDbRunParamsFactory::initIDList(){

  if(isloaded) return;

  mIDList.push_back(new StDbTableID("tfs_fspar_st",tfs_fspar_stID));
  mIDList.push_back(new StDbTableID("tfs_fsctrl_st",tfs_fsctrl_stID));
  isloaded=true;

}









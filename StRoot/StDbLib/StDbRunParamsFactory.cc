/***************************************************************************
 *
 * $Id: StDbRunParamsFactory.cc,v 1.2 1999/09/30 02:06:07 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  StDbTable ctor & list holder for RunParams tables
 *
 ***************************************************************************
 *
 * $Log: StDbRunParamsFactory.cc,v $
 * Revision 1.2  1999/09/30 02:06:07  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
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









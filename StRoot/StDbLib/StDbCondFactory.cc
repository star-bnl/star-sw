/***************************************************************************
 *
 * $Id: StDbCondFactory.cc,v 1.3 1999/12/07 21:25:25 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  StDbTable ctor & list holder for Conditions tables
 *
 ***************************************************************************
 *
 * $Log: StDbCondFactory.cc,v $
 * Revision 1.3  1999/12/07 21:25:25  porter
 * some fixes for linux warnings
 *
 * Revision 1.2  1999/09/30 02:06:02  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#include "StDbCondFactory.hh"
#include "StDbCondSchema.h"


StDbCondFactory* StDbCondFactory::mInstance=0; 

/////////////////////////////////////////////////////////

void
StDbCondFactory::initIDList(){

  if(isloaded) return;

  mIDList.push_back(new StDbTableID("tpcGas",tpcGasID));
  mIDList.push_back(new StDbTableID("tpcVoltages",tpcVoltagesID));
  mIDList.push_back(new StDbTableID("SectorBroker",SectorBrokerID));
  isloaded=true;

}









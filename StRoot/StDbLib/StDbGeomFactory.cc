/***************************************************************************
 *
 * $Id: StDbGeomFactory.cc,v 1.3 1999/09/30 02:06:06 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  StDbTable ctor & list holder for Geometry tables
 *
 ***************************************************************************
 *
 * $Log: StDbGeomFactory.cc,v $
 * Revision 1.3  1999/09/30 02:06:06  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#include "StDbGeomFactory.hh"
#include "StDbGeomSchema.h"


StDbGeomFactory* StDbGeomFactory::mInstance=0; 

/////////////////////////////////////////////////////////

void
StDbGeomFactory::initIDList(){

  if(isloaded) return;

  mIDList.push_back(new StDbTableID("tpcWirePlanes",tpcWirePlanesID));
  mIDList.push_back(new StDbTableID("tpcElectronics",tpcElectronicsID));
  mIDList.push_back(new StDbTableID("tpcDimensions",tpcDimensionsID));
  mIDList.push_back(new StDbTableID("tpcPadPlanes",tpcPadPlanesID));
  mIDList.push_back(new StDbTableID("tpcSectorPosition",tpcSectorPositionID));

  isloaded=true;

}









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









/***************************************************************************
 * Author: christelle roy
 * Description: SSD Geometry
 ***************************************************************************/


#include "StSsdGeometry.hh"
#include "StMessMgr.h"
#include "tables/St_ssdWafersPosition_Table.h"
#include "tables/St_ssdDimensions_Table.h"

ClassImp(StSsdGeometry)

StSsdGeometry::StSsdGeometry() : StSsdWaferCollection()
{}

StSsdGeometry::StSsdGeometry(const char* config) : StSsdWaferCollection(config)
{}

StSsdGeometry::StSsdGeometry(StSsdConfig* config) : StSsdWaferCollection(config)
{}

StSsdGeometry::~StSsdGeometry()
{}

StSsdGeometry::StSsdGeometry(const StSsdGeometry& geom)
{}

StSsdGeometry& StSsdGeometry::operator = (const StSsdGeometry& geom)
{
  return *this;
}


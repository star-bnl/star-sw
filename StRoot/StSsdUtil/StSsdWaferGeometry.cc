/***************************************************************************
 *
 *  StSsdWaferGeometry.cc,v 1
 *
 * Author: cr
 ***************************************************************************
 *
 * Description: SSD Wafer Geometry
 *
 **************************************************************************/

#include "StSsdWaferGeometry.hh"
#include "StMessMgr.h"

ClassImp(StSsdWaferGeometry)

StSsdWaferGeometry::StSsdWaferGeometry() : StSsdHybridObject()
{}

StSsdWaferGeometry::StSsdWaferGeometry(int barrel, int ladder, int wafer) : 
  StSsdHybridObject(barrel, ladder, wafer, 0)
{}

StSsdWaferGeometry::~StSsdWaferGeometry()
{}

StSsdWaferGeometry::StSsdWaferGeometry(const StSsdWaferGeometry& geom)
{}

StSsdWaferGeometry& StSsdWaferGeometry::operator = (const StSsdWaferGeometry& geom)
{
  return *this;
}

/***************************************************************************
 *
 * $Id: StSvtWaferGeometry.cc,v 1.1 2001/08/16 21:02:04 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Wafer Geometry
 *
 ***************************************************************************
 *
 * $Log: StSvtWaferGeometry.cc,v $
 * Revision 1.1  2001/08/16 21:02:04  munhoz
 * changing StObjArray to StStrArray. StSvtConfig reestructured. New classes for geometry DB
 *
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// This class represents the SVT Geometry object.                   //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include "StSvtWaferGeometry.hh"
#include "StMessMgr.h"

ClassImp(StSvtWaferGeometry)

StSvtWaferGeometry::StSvtWaferGeometry() : StSvtHybridObject()
{}

StSvtWaferGeometry::StSvtWaferGeometry(int barrel, int ladder, int wafer) : 
  StSvtHybridObject(barrel, ladder, wafer, 0)
{}

StSvtWaferGeometry::~StSvtWaferGeometry()
{}

StSvtWaferGeometry::StSvtWaferGeometry(const StSvtWaferGeometry& geom)
{}

StSvtWaferGeometry& StSvtWaferGeometry::operator = (const StSvtWaferGeometry& geom)
{
  return *this;
}

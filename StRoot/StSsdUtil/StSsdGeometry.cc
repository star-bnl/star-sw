/***************************************************************************
 *
 * $Id: StSsdGeometry.cc,v 1.1 2004/03/12 04:24:20 jeromel Exp $
 *
 * Author: cr
 ***************************************************************************
 *
 * Description: SSD Geometry
 ***************************************************************************/


#include "StSsdGeometry.hh"
#include "StMessMgr.h"

#include "tables/St_svg_geom_Table.h"
#include "tables/St_svg_shape_Table.h"
#include "tables/St_srs_srspar_Table.h"

ClassImp(StSsdGeometry)

StSsdGeometry::StSsdGeometry() : StSsdWaferCollection()
{}

StSsdGeometry::StSsdGeometry(const char* config) : StSsdWaferCollection(config)
{}

StSsdGeometry::StSsdGeometry(StSsdConfig* config) : StSsdWaferCollection(config)
{}

StSsdGeometry::StSsdGeometry(srs_srspar_st* param, svg_geom_st *geom, svg_shape_st *shape) : StSsdWaferCollection()
{

}

StSsdGeometry::~StSsdGeometry()
{}

StSsdGeometry::StSsdGeometry(const StSsdGeometry& geom)
{}

StSsdGeometry& StSsdGeometry::operator = (const StSsdGeometry& geom)
{
  return *this;
}

int StSsdGeometry::getBarrelID(int layer, int ladder)
{
  int barrel = -1;

  switch (layer) {
  case 1:
    barrel = 1;
    break;
    
  default:
    gMessMgr->Error() << "There is NO barrel number !!!";
    gMessMgr->Print();
    break;  
  }

  return barrel;
}

int StSsdGeometry::getWaferIndex(int barrel, int ladder, int wafer)
{
  return StSsdWaferCollection::getWaferIndex( barrel, ladder, wafer);
}

int StSsdGeometry::getWaferIndex(int HardWarePos)
{
  int layer = (int)HardWarePos/1000;
  int wafer = (int)(HardWarePos - layer*1000)/100;
  int ladder = (int)(HardWarePos - layer*1000 - wafer*100);
  //  int barrel = getBarrelID(layer,ladder);
  int barrel = 1;

  if (barrel > 0)
    return getWaferIndex(barrel,ladder,wafer);
  else
    return -1;
}

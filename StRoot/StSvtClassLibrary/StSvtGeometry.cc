/***************************************************************************
 *
 * $Id: StSvtGeometry.cc,v 1.2 2002/01/31 21:57:35 caines Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Geometry
 *
 ***************************************************************************
 *
 * $Log: StSvtGeometry.cc,v $
 * Revision 1.2  2002/01/31 21:57:35  caines
 * Get ladder and barrels correct for the middle layer
 *
 * Revision 1.1  2001/08/16 21:02:03  munhoz
 * changing StObjArray to StStrArray. StSvtConfig reestructured. New classes for geometry DB
 *
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// This class represents the SVT Geometry object.                   //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include "StSvtGeometry.hh"
#include "StMessMgr.h"

#include "tables/St_svg_geom_Table.h"
#include "tables/St_svg_shape_Table.h"
#include "tables/St_srs_srspar_Table.h"

ClassImp(StSvtGeometry)

StSvtGeometry::StSvtGeometry() : StSvtWaferCollection()
{}

StSvtGeometry::StSvtGeometry(const char* config) : StSvtWaferCollection(config)
{}

StSvtGeometry::StSvtGeometry(StSvtConfig* config) : StSvtWaferCollection(config)
{}

StSvtGeometry::StSvtGeometry(srs_srspar_st* param, svg_geom_st *geom, svg_shape_st *shape) : StSvtWaferCollection()
{

}

StSvtGeometry::~StSvtGeometry()
{}

StSvtGeometry::StSvtGeometry(const StSvtGeometry& geom)
{}

StSvtGeometry& StSvtGeometry::operator = (const StSvtGeometry& geom)
{
  return *this;
}

int StSvtGeometry::getBarrelID(int layer, int ladder)
{
  int barrel = -1;

  switch (layer) {
  case 1:
    if (ladder%2 == 0)
      barrel = 1;
    break;
  case 2:
    if (ladder%2 != 0)
      barrel = 1;
    break;
  case 3:
    //if (!ladder%2)
    if (ladder%2 == 0)
      barrel = 2;
    break;
  case 4:
    //if (ladder%2)
    if (ladder%2 != 0)
      barrel = 2;
    break;
  case 5:
    if (ladder%2 == 0)
      barrel = 3;
    break;
  case 6:
    if (ladder%2 != 0)
      barrel = 3;
    break;
  }

  return barrel;
}

int StSvtGeometry::getWaferIndex(int barrel, int ladder, int wafer)
{
  return StSvtWaferCollection::getWaferIndex( barrel, ladder, wafer);
}

int StSvtGeometry::getWaferIndex(int HardWarePos)
{
  int layer = (int)HardWarePos/1000;
  int wafer = (int)(HardWarePos - layer*1000)/100;
  int ladder = (int)(HardWarePos - layer*1000 - wafer*100);
  int barrel = getBarrelID(layer,ladder);

  if (barrel > 0)
    return getWaferIndex(barrel,ladder,wafer);
  else
    return -1;
}

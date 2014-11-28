/***************************************************************************
 *
 * $Id: StSvtGeometry.cc,v 1.3 2005/07/23 03:37:33 perev Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Geometry
 *
 ***************************************************************************
 *
 * $Log: StSvtGeometry.cc,v $
 * Revision 1.3  2005/07/23 03:37:33  perev
 * IdTruth + Cleanup
 *
 * Revision 1.2  2002/01/31 21:57:35  caines
 *  Get ladder and barrels correct for the middle layer
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
static const int tbBarrel[6][2]={
{1,-1},{-1,1},{2,-1},{-1,2},{3,-1},{-1,3}};  

  if (layer>0 && layer<=6) barrel = tbBarrel[layer-1][ladder&1];

  return barrel;
}

int StSvtGeometry::getWaferIndex(int barrel, int ladder, int wafer)
{
  return StSvtWaferCollection::getWaferIndex( barrel, ladder, wafer);
}

int StSvtGeometry::getWaferIndex(int HardWarePos)
{
  int layer  = (int)(HardWarePos/1000);
  int wafer  = (int)(HardWarePos/100 )%10;
  int ladder = (int)(HardWarePos%100 );
  int barrel = getBarrelID(layer,ladder);

  if (barrel > 0)
    return getWaferIndex(barrel,ladder,wafer);
  else
    return -1;
}

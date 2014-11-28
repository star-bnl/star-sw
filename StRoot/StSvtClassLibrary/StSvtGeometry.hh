/***************************************************************************
 *
 * $Id: StSvtGeometry.hh,v 1.1 2001/08/16 21:02:03 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Geometry object. It makes the link with the Data Base
 *
 ***************************************************************************
 *
 * $Log: StSvtGeometry.hh,v $
 * Revision 1.1  2001/08/16 21:02:03  munhoz
 * changing StObjArray to StStrArray. StSvtConfig reestructured. New classes for geometry DB
 *
 *
 **************************************************************************/

#ifndef STSVTGEOMETRY_HH
#define STSVTGEOMETRY_HH

#include "StSvtWaferCollection.hh"

class StSvtConfig;
class svg_geom_st;
class svg_shape_st;
class srs_srspar_st;

class StSvtGeometry: public StSvtWaferCollection
{
public:
  StSvtGeometry();
  StSvtGeometry(const char* config);
  StSvtGeometry(StSvtConfig* config);
  StSvtGeometry(srs_srspar_st* param, svg_geom_st *geom, svg_shape_st *shape);
  virtual ~StSvtGeometry();

  StSvtGeometry(const StSvtGeometry&);
  StSvtGeometry& operator = (const StSvtGeometry&);

  void setBarrelRadius(double x[6]);
  void setWaferLength(double x);
  void setWaferThickness(double x);
  void setWaferWidth(double x);
  void setAnodePitch(double x);
  void setFocusRegionLength(double x);
  void setDistanceInjector(double x[4]);
  void setLaserPosition(double x[5]);

  double getBarrelRadius(int layer){return barrelRadius[layer-1];}
  double getWaferLength(){return waferLength;}
  double getWaferThickness(){return waferThickness;}
  double getWaferWidth(){return waferWidth;}
  double getAnodePitch(){return anodePitch;}
  double getFocusRegionLength(){return focusRegionLength;}
  double getDistanceInjector(int line){return distanceInjector[line-1];}
  double getLaserPosition(int i){return laserPosition[i];}

  int getWaferIndex(int barrel, int ladder, int wafer);
  int getWaferIndex(int HardWarePos);
  int getBarrelID(int layer, int ladder);

private:

  double barrelRadius[6];     //cm 

  double waferLength;        
  double waferThickness;     
  double waferWidth;         
  double anodePitch;         
  double focusRegionLength;    
  double distanceInjector[4]; //Index 0 is the closest to anodes
  double laserPosition[5];     

  ClassDef(StSvtGeometry,1)
};

inline void StSvtGeometry::setBarrelRadius(double x[6])
{barrelRadius[0] = x[0];
 barrelRadius[1] = x[1];
 barrelRadius[2] = x[2];
 barrelRadius[3] = x[3];
 barrelRadius[4] = x[4];
 barrelRadius[5] = x[5];
};

inline void StSvtGeometry::setWaferLength(double x)
{waferLength = x;};

inline void StSvtGeometry::setWaferThickness(double x)
{waferThickness = x;};

inline void StSvtGeometry::setWaferWidth(double x)
{waferWidth = x;};

inline void StSvtGeometry::setAnodePitch(double x)
{anodePitch = x;};

inline void StSvtGeometry::setFocusRegionLength(double x)
{focusRegionLength = x;};

inline void StSvtGeometry::setDistanceInjector(double x[4])
{distanceInjector[0] = x[0];
 distanceInjector[1] = x[1];
 distanceInjector[2] = x[2];
 distanceInjector[3] = x[3];
};

inline void StSvtGeometry::setLaserPosition(double x[5])
{laserPosition[0] = x[0];
 laserPosition[1] = x[1];
 laserPosition[2] = x[2];
 laserPosition[3] = x[3];
 laserPosition[4] = x[4];
};

#endif

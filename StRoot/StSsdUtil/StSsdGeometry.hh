/*!
 * \class StSsdGeometry
 * \author cr
 *
 * SSD Geometry object. It makes the link with the Data Base
 */

#ifndef STSSDGEOMETRY_HH
#define STSSDGEOMETRY_HH

#include "StSsdWaferCollection.hh"

class StSsdConfig;
class svg_geom_st;
class svg_shape_st;
class srs_srspar_st;

class StSsdGeometry: public StSsdWaferCollection
{
public:
  StSsdGeometry();
  StSsdGeometry(const char* config);
  StSsdGeometry(StSsdConfig* config);
  StSsdGeometry(srs_srspar_st* param, svg_geom_st *geom, svg_shape_st *shape);
  virtual ~StSsdGeometry();

  StSsdGeometry(const StSsdGeometry&);
  StSsdGeometry& operator = (const StSsdGeometry&);

  //  void setBarrelRadius(double x[1]);
  void setBarrelRadius(double x);
  void setWaferLength(double x);
  void setWaferThickness(double x);
  void setWaferWidth(double x); 
  void setStripPitch(double x);
  void setFocusRegionLength(double x);
  void setDistanceInjector(double x[4]);
  void setLaserPosition(double x[5]);

  //  double getBarrelRadius(int layer){return barrelRadius[layer-1];}
  double getBarrelRadius(int layer){return barrelRadius;}

  double getWaferLength(){return waferLength;}
  double getWaferThickness(){return waferThickness;}
  double getWaferWidth(){return waferWidth;}
  double getStripPitch(){return stripPitch;}
  double getFocusRegionLength(){return focusRegionLength;}
  double getDistanceInjector(int line){return distanceInjector[line-1];}
  double getLaserPosition(int i){return laserPosition[i];}

  int getWaferIndex(int barrel, int ladder, int wafer);
  int getWaferIndex(int HardWarePos);
  int getBarrelID(int layer, int ladder);

private:

  double barrelRadius;     //cm 
  double waferLength;        
  double waferThickness;     
  double waferWidth;         
  double stripPitch;         
  double focusRegionLength;    
  double distanceInjector[4]; //Index 0 is the closest to anodes
  double laserPosition[5];     

  ClassDef(StSsdGeometry,1)
};

// inline void StSsdGeometry::setBarrelRadius(double x[1])
// {barrelRadius[0] = x[0];
// };
inline void StSsdGeometry::setBarrelRadius(double x)
{barrelRadius = x;
};

inline void StSsdGeometry::setWaferLength(double x)
{waferLength = x;};

inline void StSsdGeometry::setWaferThickness(double x)
{waferThickness = x;};

inline void StSsdGeometry::setWaferWidth(double x)
{waferWidth = x;};

inline void StSsdGeometry::setStripPitch(double x)
{stripPitch = x;};

inline void StSsdGeometry::setFocusRegionLength(double x)
{focusRegionLength = x;};

inline void StSsdGeometry::setDistanceInjector(double x[4])
{
  distanceInjector[0] = x[0];
  distanceInjector[1] = x[1];
  distanceInjector[2] = x[2];
  distanceInjector[3] = x[3];
};

inline void StSsdGeometry::setLaserPosition(double x[5])
{laserPosition[0] = x[0];
 laserPosition[1] = x[1];
 laserPosition[2] = x[2];
 laserPosition[3] = x[3];
 laserPosition[4] = x[4];
};

#endif

/***************************************************************************
 *
 * $Id: StSsdGeometry.hh,v 1.1 2004/03/12 04:24:20 jeromel Exp $
 *
 **************************************************************************/

/*!
 * \class StSsdGeometry
 * \author christelle roy
 *
 * SSD Geometry object. It makes the link with the Data Base
 */

#ifndef STSSDGEOMETRY_HH
#define STSSDGEOMETRY_HH

#include "StSsdWaferCollection.hh"

class StSsdConfig;
class ssdWafersPosition_st;
class StSsdGeometry: public StSsdWaferCollection
{
public:
  StSsdGeometry();
  StSsdGeometry(const char* config);
  StSsdGeometry(StSsdConfig* config);
  virtual ~StSsdGeometry();

  StSsdGeometry(const StSsdGeometry&);
  StSsdGeometry& operator = (const StSsdGeometry&);

  void setSectorRadius(double x[4]);
  void setWaferLength(double x);
  void setWaferThickness(double x);
  void setWaferWidth(double x); 
  void setStripPitch(double x);

  double getSectorRadius(int sector){return sectorRadius[sector-1];}
  double getWaferLength(){return waferLength;}
  double getWaferThickness(){return waferThickness;}
  double getWaferWidth(){return waferWidth;}
  double getStripPitch(){return stripPitch;}

private:

  double sectorRadius[4];
  double waferLength;        
  double waferThickness;     
  double waferWidth;         
  double stripPitch;         

  ClassDef(StSsdGeometry,1)
};

inline void StSsdGeometry::setSectorRadius(double x[4])
{
sectorRadius[0] = x[0];
sectorRadius[1] = x[1];
sectorRadius[2] = x[2];
sectorRadius[3] = x[3];
};

inline void StSsdGeometry::setWaferLength(double x)
{waferLength = x;};

inline void StSsdGeometry::setWaferThickness(double x)
{waferThickness = x;};

inline void StSsdGeometry::setWaferWidth(double x)
{waferWidth = x;};

inline void StSsdGeometry::setStripPitch(double x)
{stripPitch = x;};

#endif

/***************************************************************************
 *
 * $Id: StSsdGeometry.hh,v 1.2 2004/07/20 13:58:54 croy Exp $
 *
 **************************************************************************/

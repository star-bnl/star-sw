/***************************************************************************
 *
 *  StSsdWaferGeometry.hh,v 1
 *
 * Author: cr
 ***************************************************************************
 *
 * Description: SSD Wafer Geometry object. It makes the link with the Data Base
 *
 **************************************************************************/

#ifndef STSSDWAFERGEOMETRY_HH
#define STSSDWAFERGEOMETRY_HH

#include "StSsdHybridObject.hh"

class StSsdWaferGeometry: public StSsdHybridObject
{
public:
  StSsdWaferGeometry();
  StSsdWaferGeometry(int barrel, int ladder, int wafer);
  virtual ~StSsdWaferGeometry();

  StSsdWaferGeometry(const StSsdWaferGeometry&);
  StSsdWaferGeometry& operator = (const StSsdWaferGeometry&);

  void setID(int i){id = i;}
  void setDriftDirection(double x1, double x2, double x3);
  void setNormalDirection(double x1, double x2, double x3);
  void setTransverseDirection(double x1, double x2, double x3);
  void setCenterPosition(double x1, double x2, double x3);

  int getID(){return id;}
  double d(int i){return driftDirection[i];}
  double n(int i){return normalDirection[i];}
  double t(int i){return transverseDirection[i];}
  double x(int i){return centerPosition[i];}

private:

  int id;

  double  driftDirection[3]; // driftDirection
  double  normalDirection[3]; // normalDirection
  double  transverseDirection[3]; // transverseDirection
  double  centerPosition[3]; // centerPosition

  ClassDef(StSsdWaferGeometry,1)
};

inline void StSsdWaferGeometry::setDriftDirection(double x1, double x2, double x3)
{driftDirection[0] = x1; driftDirection[1] = x2; driftDirection[2] = x3;}
inline void StSsdWaferGeometry::setNormalDirection(double x1, double x2, double x3)
{normalDirection[0] = x1; normalDirection[1] = x2; normalDirection[2] = x3;}
inline void StSsdWaferGeometry::setTransverseDirection(double x1, double x2, double x3)
{transverseDirection[0] = x1; transverseDirection[1] = x2; transverseDirection[2] = x3;}
inline void StSsdWaferGeometry::setCenterPosition(double x1, double x2, double x3)
{centerPosition[0] = x1; centerPosition[1] = x2; centerPosition[2] = x3;}

#endif

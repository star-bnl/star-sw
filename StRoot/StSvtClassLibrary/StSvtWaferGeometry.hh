/***************************************************************************
 *
 * $Id: StSvtWaferGeometry.hh,v 1.1 2001/08/16 21:02:04 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Wafer Geometry object. It makes the link with the Data Base
 *
 ***************************************************************************
 *
 * $Log: StSvtWaferGeometry.hh,v $
 * Revision 1.1  2001/08/16 21:02:04  munhoz
 * changing StObjArray to StStrArray. StSvtConfig reestructured. New classes for geometry DB
 *
 *
 **************************************************************************/

#ifndef STSVTWAFERGEOMETRY_HH
#define STSVTWAFERGEOMETRY_HH

#include "StSvtHybridObject.hh"

class StSvtWaferGeometry: public StSvtHybridObject
{
public:
  StSvtWaferGeometry();
  StSvtWaferGeometry(int barrel, int ladder, int wafer);
  virtual ~StSvtWaferGeometry();

  StSvtWaferGeometry(const StSvtWaferGeometry&);
  StSvtWaferGeometry& operator = (const StSvtWaferGeometry&);

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

  ClassDef(StSvtWaferGeometry,1)
};

inline void StSvtWaferGeometry::setDriftDirection(double x1, double x2, double x3)
{driftDirection[0] = x1; driftDirection[1] = x2; driftDirection[2] = x3;}
inline void StSvtWaferGeometry::setNormalDirection(double x1, double x2, double x3)
{normalDirection[0] = x1; normalDirection[1] = x2; normalDirection[2] = x3;}
inline void StSvtWaferGeometry::setTransverseDirection(double x1, double x2, double x3)
{transverseDirection[0] = x1; transverseDirection[1] = x2; transverseDirection[2] = x3;}
inline void StSvtWaferGeometry::setCenterPosition(double x1, double x2, double x3)
{centerPosition[0] = x1; centerPosition[1] = x2; centerPosition[2] = x3;}

#endif

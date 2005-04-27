#include "StiCylindricalShape.h"
ostream& operator<<(ostream& os, const StiShape& m)
{
  os << "Name:"<< m.getName()
     << " ShapeCode:"<< m.getShapeCode()
     << " HalfDepth:"<< m.getHalfDepth()
     << " HalfWidth:"<< m.getHalfWidth();
  if (m.getShapeCode() == kCylindrical) {
    StiCylindricalShape *cyl = (StiCylindricalShape *) &m;
    os << " OuterRadius:"<< cyl->getOuterRadius()
       << " OpeningAngle:"<< cyl->getOpeningAngle();
  }
  os << " Thickness:"<< m.getThickness()
     << " EdgeWidth:"<< m.getEdgeWidth();
  return os;
}

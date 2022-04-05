#include "StiCylindricalShape.h"


ostream& operator<<(ostream& os, const StiShape& m)
{
  os << "StiShape: " << endl
     << "Name: "<< m.getName()
     << " ShapeCode: "<< m.getShapeCode()
     << " HalfDepth (dZ): " << m.getHalfDepth()
     << " HalfWidth (dY): " << m.getHalfWidth();

  if (m.getShapeCode() == kCylindrical) {
    StiCylindricalShape *cyl = (StiCylindricalShape *) &m;
    os << " OuterRadius: "<< cyl->getOuterRadius()
       << " OpeningAngle: "<< cyl->getOpeningAngle();
  }

  os << " Thickness (2*dX): " << m.getThickness()
     << " EdgeWidth: " << m.getEdgeWidth()
     << endl;

  return os;
}

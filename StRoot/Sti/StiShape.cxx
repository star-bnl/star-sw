#include "StiCylindricalShape.h"
#include "TMath.h"
#include "TString.h"
ostream& operator<<(ostream& os, const StiShape& m)
{
  os << Form(" %15s dZ:%6.2f",m.getName().c_str(),m.getHalfDepth());
  if (TMath::Abs(m.getHalfWidth()) > 1e-3) os << Form(" dY:%5.2f",m.getHalfWidth());
  if (m.getShapeCode() == kCylindrical) {
    StiCylindricalShape *cyl = (StiCylindricalShape *) &m;
    os << Form(" R:%5.2f",cyl->getOuterRadius());
  }
  os << Form(" dX:%5.2f",m.getThickness());
  return os;
}

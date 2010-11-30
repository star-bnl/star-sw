// Box

#ifndef G_SOLID_BOX_H
#define G_SOLID_BOX_H

#include "Solid.hh"

namespace Garfield {

class SolidBox : public Solid {

  public:
    // Constructors
    SolidBox(const double cx, const double cy, const double cz,
             const double lx, const double ly, const double lz);
    SolidBox(const double cx, const double cy, const double cz, 
             const double lx, const double ly, const double lz,
             const double dx, const double dy, const double dz);
    // Destructor
    ~SolidBox() {}
    
    bool IsInside(const double x, const double y, const double z);
    bool GetBoundingBox(double& xmin, double& ymin, double& zmin,
                        double& xmax, double& ymax, double& zmax);
    bool IsBox() {return true;}

    bool GetCenter(double& x, double& y, double& z);
    bool GetDimensions(double& l1, double& l2, double& l3);
    bool GetOrientation(double& ctheta, double& stheta, 
                        double& cphi, double& sphi);
    
    
  private:

    // Center of the box
    double cX, cY, cZ;
    // Half lengths
    double lX, lY, lZ;
    // Direction
    double dX, dY, dZ;
    double cPhi, sPhi;    
    double cTheta, sTheta;

};

}

#endif

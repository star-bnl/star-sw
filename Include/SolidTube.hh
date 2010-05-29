// Cylindrical tube

#ifndef G_SOLID_TUBE_H
#define G_SOLID_TUBE_H

#include "Solid.hh"

namespace Garfield {

class SolidTube : public Solid {

  public:
    // Constructors
    SolidTube(const double cx, const double cy, const double cz,
              const double rmin, const double rmax, const double lz);
    SolidTube(const double cx, const double cy, const double cz, 
              const double rmin, const double rmax, const double lz,
              const double dx, const double dy, const double dz);
    // Destructor
    ~SolidTube() {}
    
    bool IsInside(const double x, const double y, const double z);
    bool GetBoundingBox(double& xmin, double& ymin, double& zmin,
                        double& xmax, double& ymax, double& zmax);
    bool IsTube() {return true;}

    bool GetCenter(double& x, double& y, double& z);
    bool GetDimensions(double& l1, double& l2, double& l3);
    bool GetDirection(double& x, double& y, double& z);  
    
  private:

    // Center of the tube
    double cX, cY, cZ;
    // Inner and outer radius
    double rMin, rMax;
    // Length
    double lZ;
    // Direction
    double dX, dY, dZ;
    double cPhi, sPhi;    
    double cTheta, sTheta;

};

}

#endif

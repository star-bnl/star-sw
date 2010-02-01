// Abstract base class for solids

#ifndef G_SOLID_H
#define G_SOLID_H

namespace Garfield {

class Solid {

  public:
    // Constructor    
    Solid() : debug(false), warning(false) {}
    // Destructor
    virtual ~Solid() {}
    
    virtual bool IsInside(const double x, const double y, const double z) = 0;
    virtual bool GetBoundingBox(double& xmin, double& ymin, double& zmin,
                                double& xmax, double& ymax, double& zmax) = 0;
                                
    // Switch on/off debugging and warning messages
    void EnableDebugging() {debug = true;}
    void DisableDebugging() {debug = false;}
    void EnableWarnings() {warning = true;}    
    void DisableWarnings() {warning = false;}
    
  protected:
    bool debug, warning;
    
};

}

#endif

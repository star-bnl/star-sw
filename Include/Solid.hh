// Abstract base class for solids

#ifndef G_SOLID_H
#define G_SOLID_H

namespace Garfield {

class Solid {

  public:
    // Constructor    
    Solid() : debug(false) {}
    // Destructor
    virtual ~Solid() {}
    
    virtual bool IsInside(const double x, const double y, const double z) = 0;
    virtual bool GetBoundingBox(double& xmin, double& ymin, double& zmin,
                                double& xmax, double& ymax, double& zmax) = 0;
    // Solid type
    virtual bool IsBox()  {return false;}
    virtual bool IsTube() {return false;}

    virtual bool GetCenter(double& x, double& y, double& z) = 0;
    virtual bool GetDimensions(double& l1, double& l2, double& l3) = 0;
    virtual bool GetDirection(double& x, double& y, double& z) = 0;
                            
    // Switch on/off debugging messages
    void EnableDebugging() {debug = true;}
    void DisableDebugging() {debug = false;}
    
  protected:
    bool debug;
    
};

}

#endif

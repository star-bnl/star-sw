// Abstract base class for components

#ifndef G_COMPONENT_BASE_H
#define G_COMPONENT_BASE_H

#include <vector>

#include "GeometryBase.hh"

namespace Garfield {

class ComponentBase {

  public:
    // Constructor
    ComponentBase();
    virtual ~ComponentBase() {}

    virtual
    void SetGeometry(GeometryBase* geo);
    virtual
    void Clear();

    // Get the medium at a given location (x, y, z)
    virtual
    bool GetMedium(const double x, const double y, const double z, Medium*& m);

    // Electric field
    //
    // Status flags:
    //
    //             0: Inside an active medium
    //           > 0: Inside a wire of type X
    //     -4 ... -1: On the side of a plane where no wires are
    //            -5: Inside the mesh but not in an active medium
    //            -6: Outside the mesh
    //           -10: Unknown potential type (should not occur)
    //         other: Other cases (should not occur)
    //
    // Calculate the drift field [V/cm] at (x, y, z)
    virtual 
    void ElectricField(const double x, const double y, const double z,
                       double& ex, double& ey, double& ez, 
                       Medium*& m, int& status) = 0;
    // Calculate the drift field [V/cm] and potential [V] at (x, y, z)
    virtual 
    void ElectricField(const double x, const double y, const double z, 
                       double& ex, double& ey, double& ez, double& v, 
                       Medium*& m, int& status) = 0;
    // Calculate the voltage range [V]
    virtual
    bool GetVoltageRange(double& vmin, double& vmax) = 0;
    
    virtual
    void WeightingField(const double x, const double y, const double z,
                        double& wx, double& wy, double& wz,
                        const std::string label);
    virtual
    double WeightingPotential(const double x, const double y, const double z,
                              const std::string label);

    // Magnetic field
    // Calculate the magnetic field [hGauss] at (x, y, z)
    virtual 
    void MagneticField(const double x, const double y, const double z,
    	               double& bx, double& by, double& bz, int& status);
    // Set a constant magnetic field 
    void SetMagneticField(const double bx, const double by, const double bz);

    // Ready for use?
    bool IsReady() {return ready;}

    // Get the bounding box coordinates
    virtual
    bool GetBoundingBox(double& xmin, double& ymin, double& zmin,
                        double& xmax, double& ymax, double& zmax);

    // Periodicities
    void EnablePeriodicityX()  {xPeriodic = true;  UpdatePeriodicity();}
    void DisablePeriodicityX() {xPeriodic = false; UpdatePeriodicity();}
    void EnablePeriodicityY()  {yPeriodic = true;  UpdatePeriodicity();}
    void DisablePeriodicityY() {yPeriodic = false; UpdatePeriodicity();}
    void EnablePeriodicityZ()  {zPeriodic = true;  UpdatePeriodicity();}
    void DisablePeriodicityZ() {zPeriodic = false; UpdatePeriodicity();}

    void EnableMirrorPeriodicityX() {
      xMirrorPeriodic = true;  UpdatePeriodicity();
    }
    void DisableMirrorPeriodicityX() {
      xMirrorPeriodic = false; UpdatePeriodicity();
    }
    void EnableMirrorPeriodicityY() {
      yMirrorPeriodic = true;  UpdatePeriodicity();
    }
    void DisableMirrorPeriodicityY() {
      yMirrorPeriodic = false; UpdatePeriodicity();
    }
    void EnableMirrorPeriodicityZ() {
      zMirrorPeriodic = true;  UpdatePeriodicity();
    }
    void DisableMirrorPeriodicityZ() {
      zMirrorPeriodic = false; UpdatePeriodicity();
    }

    void EnableAxialPeriodicityX() {
      xAxiallyPeriodic = true;  UpdatePeriodicity();
    }
    void DisableAxialPeriodicityX() {
      xAxiallyPeriodic = false; UpdatePeriodicity();
    }
    void EnableAxialPeriodicityY() {
      yAxiallyPeriodic = true;  UpdatePeriodicity();
    }
    void DisableAxialPeriodicityY() {
      yAxiallyPeriodic = false; UpdatePeriodicity();
    }
    void EnableAxialPeriodicityZ() {
      zAxiallyPeriodic = true;  UpdatePeriodicity();
    }
    void DisableAxialPeriodicityZ() {
      zAxiallyPeriodic = false; UpdatePeriodicity();
    }

    void EnableRotationSymmetryX() {
      xRotationSymmetry = true;  UpdatePeriodicity();
    }
    void DisableRotationSymmetryX() {
      xRotationSymmetry = false; UpdatePeriodicity();
    }
    void EnableRotationSymmetryY() {
      yRotationSymmetry = true;  UpdatePeriodicity();
    }
    void DisableRotationSymmetryY() {
      yRotationSymmetry = false; UpdatePeriodicity();
    }
    void EnableRotationSymmetryZ() {
      zRotationSymmetry = true;  UpdatePeriodicity();
    }
    void DisableRotationSymmetryZ() {
      zRotationSymmetry = false; UpdatePeriodicity();
    }

    // Switch on/off debugging and warning messages
    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

  protected:
   
    GeometryBase* theGeometry;
 
    // Ready for use?
    bool ready;

    // Simple periodicity in x, y, z
    bool xPeriodic,         yPeriodic,         zPeriodic;
    // Mirror periodicity in x, y, z
    bool xMirrorPeriodic,   yMirrorPeriodic,   zMirrorPeriodic;
    // Axial periodicity in x, y, z
    bool xAxiallyPeriodic,  yAxiallyPeriodic,  zAxiallyPeriodic;
    // Rotation symmetry around x-axis, y-axis, z-axis
    bool xRotationSymmetry, yRotationSymmetry, zRotationSymmetry;

    // Constant magnetic field
    double bx0, by0, bz0;
    
    // Switch on/off debugging messages
    bool debug;  
    
    // Geometry checks
    virtual void Reset() = 0;
    // Verify periodicities
    virtual void UpdatePeriodicity() = 0;

};
  
}

#endif

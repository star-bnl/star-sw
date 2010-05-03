// Abstract base class for components

#ifndef G_COMPONENT_BASE_H
#define G_COMPONENT_BASE_H

#include <vector>

#include "Medium.hh"
#include "Solid.hh"

namespace Garfield {

class ComponentBase {

  public:
    // Constructor
    ComponentBase();
    virtual ~ComponentBase() {}

    // Add a solid to the geometry
    void AddSolid(Solid* s, Medium* m, int bctype, double bcval);
    // Get the solid at a given location (x, y, z)
    bool GetSolid(const double x, const double y, const double z, Solid*& s);
    // Get the medium at a given location (x, y, z)
    virtual
    bool GetMedium(const double x, const double y, const double z, Medium*& m);
    // Number of solids/media in the component
    int GetNumberOfSolids() const {return nSolids;}
    int GetNumberOfMedia() const  {return nMedia; }
    // Get a solid/medium from the list
    bool GetSolid(const int i, Solid*& s) const;
    virtual
    bool GetMedium(const int i, Medium*& m) const;
    // Reset the geometry
    void Clear();

    // Electric field
    //
    // Flags:
    //
    //   - Status
    //             0: Inside an active medium
    //           > 0: Inside a wire of type X
    //     -4 ... -1: On the side of a plane where no wires are
    //            -5: Inside the mesh but not in an active medium
    //            -6: Outside the mesh
    //           -10: Unknown potential type (should not occur)
    //         other: Other cases (should not occur)
    //
    //   - Medium
    //          >= 0: Id number of the medium at this location
    //           < 0: No medium associated to this point
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
    void WeightingPotential(const double x, const double y, const double z,
                            double& w, const std::string label);

    // Magnetic field
    // Calculate the magnetic field [hGauss] at (x, y, z)
    virtual 
    void MagneticField(const double x, const double y, const double z,
    	               double& bx, double& by, double& bz, int& status);
    // Set a constant magnetic field 
    void SetMagneticField(const double bx, const double by, const double bz);

    // Bounding box (envelope of geometry)
    bool IsInBoundingBox(const double x, const double y, const double z);
    bool GetBoundingBox(double& xmin, double& ymin, double& zmin,
                        double& xmax, double& ymax, double& zmax) {
      xmin = xMinBoundingBox; ymin = yMinBoundingBox; zmin = zMinBoundingBox; 
      xmax = xMaxBoundingBox; ymax = yMaxBoundingBox; zmax = zMaxBoundingBox;
      return true;
    }

    // Ready for use?
    bool IsReady() {return ready;}

    // Periodicities
    void XPeriodic()         {xPeriodic = true;         UpdatePeriodicity();}
    void YPeriodic()         {yPeriodic = true;         UpdatePeriodicity();}
    void ZPeriodic()         {zPeriodic = true;         UpdatePeriodicity();}
    void XMirrorPeriodic()   {xMirrorPeriodic = true;   UpdatePeriodicity();}
    void YMirrorPeriodic()   {yMirrorPeriodic = true;   UpdatePeriodicity();}
    void ZMirrorPeriodic()   {zMirrorPeriodic = true;   UpdatePeriodicity();}
    void XAxiallyPeriodic()  {xAxiallyPeriodic = true;  UpdatePeriodicity();}
    void YAxiallyPeriodic()  {yAxiallyPeriodic = true;  UpdatePeriodicity();}
    void ZAxiallyPeriodic()  {zAxiallyPeriodic = true;  UpdatePeriodicity();}
    void XRotationSymmetry() {xRotationSymmetry = true; UpdatePeriodicity();}
    void YRotationSymmetry() {yRotationSymmetry = true; UpdatePeriodicity();}
    void ZRotationSymmetry() {zRotationSymmetry = true; UpdatePeriodicity();}

    // Switch on/off debugging and warning messages
    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

  protected:
    
    // List of media
    int nMedia;
    struct medium {
      Medium* medium;
    };
    std::vector<medium> media;
    
    // List of solids
    int nSolids;
    struct solid {
      Solid* solid;
      int medium;
      int bctype;
      double bcval;
    };
    std::vector<solid> solids;

    // Bounding box ranges
    bool hasBoundingBox;
    double xMinBoundingBox, yMinBoundingBox, zMinBoundingBox;
    double xMaxBoundingBox, yMaxBoundingBox, zMaxBoundingBox;

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
    virtual bool CheckSolidType(Solid* s) = 0;
    virtual void CheckBoundaryConditionType(int& bctype, double& bcval) = 0;
    // Reset the component
    virtual void Reset() = 0;
    // Verify periodicities
    virtual void UpdatePeriodicity() = 0;

};
  
}

#endif

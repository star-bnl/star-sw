// Abstract base class for components

#ifndef G_COMPONENT_BASE_H
#define G_COMPONENT_BASE_H

#include <vector>
#include <string>

#include "GeometryBase.hh"

namespace Garfield {

class ComponentBase {

 public:
  // Constructor
  ComponentBase();
  // Destructor
  virtual ~ComponentBase() {}

  virtual void SetGeometry(GeometryBase* geo);
  virtual void Clear();

  // Get the medium at a given location (x, y, z)
  virtual Medium* GetMedium(const double& x, const double& y, 
                            const double& z);

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
  virtual void ElectricField(const double x, const double y, const double z,
                             double& ex, double& ey, double& ez, Medium*& m,
                             int& status) = 0;
  // Calculate the drift field [V/cm] and potential [V] at (x, y, z)
  virtual void ElectricField(const double x, const double y, const double z,
                             double& ex, double& ey, double& ez, double& v,
                             Medium*& m, int& status) = 0;
  // Calculate the voltage range [V]
  virtual bool GetVoltageRange(double& vmin, double& vmax) = 0;

  // Calculate the weighting field [1/cm] at (x,y,z)
  // for an electrode (specified by its label)
  virtual void WeightingField(const double x, const double y, const double z,
                              double& wx, double& wy, double& wz,
                              const std::string label);
  virtual double WeightingPotential(const double x, const double y,
                                    const double z, const std::string label);

  // Magnetic field
  // Calculate the magnetic field [Tesla] at (x, y, z)
  virtual void MagneticField(const double x, const double y, const double z,
                             double& bx, double& by, double& bz, int& status);
  // Set a constant magnetic field
  void SetMagneticField(const double bx, const double by, const double bz);

  // Ready for use?
  virtual bool IsReady() { return ready; }

  // Get the bounding box coordinates
  virtual bool GetBoundingBox(double& xmin, double& ymin, double& zmin,
                              double& xmax, double& ymax, double& zmax);

  virtual bool IsWireCrossed(const double x0, const double y0, const double z0,
                             const double x1, const double y1, const double z1,
                             double& xc, double& yc, double& zc);
  virtual bool IsInTrapRadius(const double q0, const double x0, 
                              const double y0, const double z0, 
                              double& xw, double& yw, double& rw);

  // Enable and disable periodicities
  void EnablePeriodicityX() {
    xPeriodic = true;
    UpdatePeriodicity();
  }
  void DisablePeriodicityX() {
    xPeriodic = false;
    UpdatePeriodicity();
  }
  void EnablePeriodicityY() {
    yPeriodic = true;
    UpdatePeriodicity();
  }
  void DisablePeriodicityY() {
    yPeriodic = false;
    UpdatePeriodicity();
  }
  void EnablePeriodicityZ() {
    zPeriodic = true;
    UpdatePeriodicity();
  }
  void DisablePeriodicityZ() {
    zPeriodic = false;
    UpdatePeriodicity();
  }

  void EnableMirrorPeriodicityX() {
    xMirrorPeriodic = true;
    UpdatePeriodicity();
  }
  void DisableMirrorPeriodicityX() {
    xMirrorPeriodic = false;
    UpdatePeriodicity();
  }
  void EnableMirrorPeriodicityY() {
    yMirrorPeriodic = true;
    UpdatePeriodicity();
  }
  void DisableMirrorPeriodicityY() {
    yMirrorPeriodic = false;
    UpdatePeriodicity();
  }
  void EnableMirrorPeriodicityZ() {
    zMirrorPeriodic = true;
    UpdatePeriodicity();
  }
  void DisableMirrorPeriodicityZ() {
    zMirrorPeriodic = false;
    UpdatePeriodicity();
  }

  void EnableAxialPeriodicityX() {
    xAxiallyPeriodic = true;
    UpdatePeriodicity();
  }
  void DisableAxialPeriodicityX() {
    xAxiallyPeriodic = false;
    UpdatePeriodicity();
  }
  void EnableAxialPeriodicityY() {
    yAxiallyPeriodic = true;
    UpdatePeriodicity();
  }
  void DisableAxialPeriodicityY() {
    yAxiallyPeriodic = false;
    UpdatePeriodicity();
  }
  void EnableAxialPeriodicityZ() {
    zAxiallyPeriodic = true;
    UpdatePeriodicity();
  }
  void DisableAxialPeriodicityZ() {
    zAxiallyPeriodic = false;
    UpdatePeriodicity();
  }

  void EnableRotationSymmetryX() {
    xRotationSymmetry = true;
    UpdatePeriodicity();
  }
  void DisableRotationSymmetryX() {
    xRotationSymmetry = false;
    UpdatePeriodicity();
  }
  void EnableRotationSymmetryY() {
    yRotationSymmetry = true;
    UpdatePeriodicity();
  }
  void DisableRotationSymmetryY() {
    yRotationSymmetry = false;
    UpdatePeriodicity();
  }
  void EnableRotationSymmetryZ() {
    zRotationSymmetry = true;
    UpdatePeriodicity();
  }
  void DisableRotationSymmetryZ() {
    zRotationSymmetry = false;
    UpdatePeriodicity();
  }

  // Switch on/off debugging messages
  void EnableDebugging() { debug = true; }
  void DisableDebugging() { debug = false; }

 protected:
  std::string m_className;

  GeometryBase* m_geometry;

  // Ready for use?
  bool ready;

  // Simple periodicity in x, y, z
  bool xPeriodic, yPeriodic, zPeriodic;
  // Mirror periodicity in x, y, z
  bool xMirrorPeriodic, yMirrorPeriodic, zMirrorPeriodic;
  // Axial periodicity in x, y, z
  bool xAxiallyPeriodic, yAxiallyPeriodic, zAxiallyPeriodic;
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

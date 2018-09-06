#ifndef G_COMPONENT_BASE_H
#define G_COMPONENT_BASE_H

#include <array>
#include <string>

#include "GeometryBase.hh"

namespace Garfield {

/// Abstract base class for components.

class ComponentBase {

 public:
  /// Constructor
  ComponentBase();
  /// Destructor
  virtual ~ComponentBase() {}

  /// Define the geometry.
  virtual void SetGeometry(GeometryBase* geo);
  /// Reset.
  virtual void Clear();

  /// Get the medium at a given location (x, y, z).
  virtual Medium* GetMedium(const double x, const double y, const double z);

  /** Calculate the drift field at given point.
    *
    * \param x,y,z coordinates [cm].
    * \param ex,ey,ez components of the electric field [V/cm].
    * \param m pointer to the medium at this location.
    * \param status status flag
    *
    * Status flags:
    *
    *             0: Inside an active medium
    *           > 0: Inside a wire of type X
    *     -4 ... -1: On the side of a plane where no wires are
    *            -5: Inside the mesh but not in an active medium
    *            -6: Outside the mesh
    *           -10: Unknown potential type (should not occur)
    *         other: Other cases (should not occur)
    */
  virtual void ElectricField(const double x, const double y, const double z,
                             double& ex, double& ey, double& ez, Medium*& m,
                             int& status) = 0;
  //// Calculate the drift field [V/cm] and potential [V] at (x, y, z).
  virtual void ElectricField(const double x, const double y, const double z,
                             double& ex, double& ey, double& ez, double& v,
                             Medium*& m, int& status) = 0;
  /// Calculate the voltage range [V].
  virtual bool GetVoltageRange(double& vmin, double& vmax) = 0;

  /** Calculate the weighting field at a given point and for a given electrode.
    * \param x,y,z coordinates [cm].
    * \param wx,wy,wz components of the weighting field [1/cm].
    * \param label name of the electrode
    */
  virtual void WeightingField(const double x, const double y, const double z,
                              double& wx, double& wy, double& wz,
                              const std::string& label);
  virtual double WeightingPotential(const double x, const double y,
                                    const double z, const std::string& label);

  /** Calculate the magnetic field at a given point.
    *
    * \param x,y,z coordinates [cm].
    * \param bx,by,bz components of the magnetic field [Tesla].
    * \param status status flag.
    */
  virtual void MagneticField(const double x, const double y, const double z,
                             double& bx, double& by, double& bz, int& status);
  /// Set a constant magnetic field.
  void SetMagneticField(const double bx, const double by, const double bz);

  /// Ready for use?
  virtual bool IsReady() { return m_ready; }

  /// Get the bounding box coordinates.
  virtual bool GetBoundingBox(double& xmin, double& ymin, double& zmin,
                              double& xmax, double& ymax, double& zmax);

  /** Determine whether the line between two points crosses a wire.
    * \param x0,y0,z0 first point [cm].
    * \param x1,y1,z1 second point [cm]
    * \param xc,yc,zc point [cm] where the line crosses the wire.
    */
  virtual bool IsWireCrossed(const double x0, const double y0, const double z0,
                             const double x1, const double y1, const double z1,
                             double& xc, double& yc, double& zc);
  /** Determine whether a particle is inside the trap radius of a wire.
    * \param q0 charge of the particle [in elementary charges].
    * \param x0,y0,z0 position [cm] of the particle.
    * \param xw,yw coordinates of the wire (if applicable).
    * \param rw radius of the wire (if applicable).
    */
  virtual bool IsInTrapRadius(const double q0, const double x0, const double y0,
                              const double z0, double& xw, double& yw,
                              double& rw);

  /// Enable simple periodicity in the \f$x\f$ direction.
  void EnablePeriodicityX(const bool on = true) {
    m_periodic[0] = on;
    UpdatePeriodicity();
  }
  void DisablePeriodicityX() { EnablePeriodicityX(false); }
  /// Enable simple periodicity in the \f$y\f$ direction.
  void EnablePeriodicityY(const bool on = true) {
    m_periodic[1] = on;
    UpdatePeriodicity();
  }
  void DisablePeriodicityY() { EnablePeriodicityY(false); }
  /// Enable simple periodicity in the \f$z\f$ direction.
  void EnablePeriodicityZ(const bool on = true) {
    m_periodic[2] = on;
    UpdatePeriodicity();
  }
  void DisablePeriodicityZ() { EnablePeriodicityZ(false); }

  /// Enable mirror periodicity in the \f$x\f$ direction.
  void EnableMirrorPeriodicityX(const bool on = true) {
    m_mirrorPeriodic[0] = on;
    UpdatePeriodicity();
  }
  void DisableMirrorPeriodicityX() { EnableMirrorPeriodicityX(false); }
  /// Enable mirror periodicity in the \f$y\f$ direction.
  void EnableMirrorPeriodicityY(const bool on = true) {
    m_mirrorPeriodic[1] = on;
    UpdatePeriodicity();
  }
  void DisableMirrorPeriodicityY() { EnableMirrorPeriodicityY(false); }
  /// Enable mirror periodicity in the \f$y\f$ direction.
  void EnableMirrorPeriodicityZ(const bool on = true) {
    m_mirrorPeriodic[2] = on;
    UpdatePeriodicity();
  }
  void DisableMirrorPeriodicityZ() { EnableMirrorPeriodicityZ(false); }

  /// Enable axial periodicity in the \f$x\f$ direction.
  void EnableAxialPeriodicityX(const bool on = true) {
    m_axiallyPeriodic[0] = on;
    UpdatePeriodicity();
  }
  void DisableAxialPeriodicityX() { EnableAxialPeriodicityX(false); }
  /// Enable axial periodicity in the \f$y\f$ direction.
  void EnableAxialPeriodicityY(const bool on = true) {
    m_axiallyPeriodic[1] = on;
    UpdatePeriodicity();
  }
  void DisableAxialPeriodicityY() { EnableAxialPeriodicityY(false); }
  /// Enable axial periodicity in the \f$z\f$ direction.
  void EnableAxialPeriodicityZ(const bool on = true) {
    m_axiallyPeriodic[2] = on;
    UpdatePeriodicity();
  }
  void DisableAxialPeriodicityZ() { EnableAxialPeriodicityZ(false); }

  /// Enable rotation symmetry around the \f$x\f$ axis.
  void EnableRotationSymmetryX(const bool on = true) {
    m_rotationSymmetric[0] = on;
    UpdatePeriodicity();
  }
  void DisableRotationSymmetryX() { EnableRotationSymmetryX(false); }
  /// Enable rotation symmetry around the \f$y\f$ axis.
  void EnableRotationSymmetryY(const bool on = true) {
    m_rotationSymmetric[1] = on;
    UpdatePeriodicity();
  }
  void DisableRotationSymmetryY() { EnableRotationSymmetryY(false); }
  /// Enable rotation symmetry around the \f$z\f$ axis.
  void EnableRotationSymmetryZ(const bool on = true) {
    m_rotationSymmetric[2] = on;
    UpdatePeriodicity();
  }
  void DisableRotationSymmetryZ() { EnableRotationSymmetryZ(false); }

  /// Switch on debugging messages.
  void EnableDebugging() { m_debug = true; }
  /// Switch off debugging messages.
  void DisableDebugging() { m_debug = false; }

  /// Request trapping to be taken care of by the component (for TCAD).
  void ActivateTraps() { m_activeTraps = true; }
  void DeactivateTraps() { m_activeTraps = false; }
  bool IsTrapActive() { return m_activeTraps; }

  /// Request velocity to be taken care of by the component (for TCAD).
  void ActivateVelocityMap() { m_hasVelocityMap = true; }
  void DectivateVelocityMap() { m_hasVelocityMap = false; }
  bool IsVelocityActive() { return m_hasVelocityMap; }

  /// Get the electron attachment coefficient.
  virtual bool ElectronAttachment(const double /*x*/, const double /*y*/,
                                  const double /*z*/, double& eta) {
    eta = 0;
    return false;
  }
  /// Get the hole attachment coefficient.
  virtual bool HoleAttachment(const double /*x*/, const double /*y*/,
                              const double /*z*/, double& eta) {
    eta = 0;
    return false;
  }
  /// Get the electron drift velocity.
  virtual void ElectronVelocity(const double /*x*/, const double /*y*/,
                                const double /*z*/, double& vx, double& vy,
                                double& vz, Medium*& /*m*/, int& status) {
    vx = vy = vz = 0;
    status = -100;
  }
  /// Get the hole drift velocity.
  virtual void HoleVelocity(const double /*x*/, const double /*y*/,
                            const double /*z*/, double& vx, double& vy,
                            double& vz, Medium*& /*m*/, int& status) {
    vx = vy = vz = 0;
    status = -100;
  }
  virtual bool GetElectronLifetime(const double /*x*/, const double /*y*/,
                                   const double /*z*/, double& etau) {
    etau = -1;
    return false;
  }
  virtual bool GetHoleLifetime(const double /*x*/, const double /*y*/,
                               const double /*z*/, double& htau) {
    htau = -1;
    return false;
  }

 protected:
  /// Class name.
  std::string m_className = "ComponentBase";

  /// Pointer to the geometry.
  GeometryBase* m_geometry = nullptr;

  /// Ready for use?
  bool m_ready = false;

  /// Does the component have traps?
  bool m_activeTraps = false;
  /// Does the component have velocity maps?
  bool m_hasVelocityMap = false;

  /// Simple periodicity in x, y, z.
  std::array<bool, 3> m_periodic = {{false, false, false}};
  /// Mirror periodicity in x, y, z.
  std::array<bool, 3> m_mirrorPeriodic = {{false, false, false}};
  /// Axial periodicity in x, y, z.
  std::array<bool, 3> m_axiallyPeriodic = {{false, false, false}};
  /// Rotation symmetry around x-axis, y-axis, z-axis.
  std::array<bool, 3> m_rotationSymmetric = {{false, false, false}};

  double m_bx0 = 0., m_by0 = 0., m_bz0 = 0.; //< Constant magnetic field.

  /// Switch on/off debugging messages
  bool m_debug = false;

  /// Reset the component.
  virtual void Reset() = 0;
  /// Verify periodicities.
  virtual void UpdatePeriodicity() = 0;
};
}

#endif

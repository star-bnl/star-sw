#ifndef G_COMPONENT_USER_H
#define G_COMPONENT_USER_H

#include "ComponentBase.hh"

namespace Garfield {

/// Simple component with electric field given by a user function.

class ComponentUser : public ComponentBase {

 public:
  /// Constructor
  ComponentUser();
  /// Destructor
  ~ComponentUser() {}

  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, Medium*& m, int& status);
  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, double& v, Medium*& m,
                     int& status);
  bool GetVoltageRange(double& vmin, double& vmax);
  void MagneticField(const double x, const double y, const double z,
                     double& bx, double& by, double& bz, int& status);
  void WeightingField(const double x, const double y, const double z,
                      double& wx, double& wy, double& wz,
                      const std::string& label);
  double WeightingPotential(const double x, const double y, const double z,
                            const std::string& label);

  /// Set the function to be called for calculating the electric field.
  void SetElectricField(void (*f)(const double, const double, const double,
                                  double&, double&, double&));
  /// Set the function to be called for calculating the potential.
  void SetPotential(void (*f)(const double, const double, const double,
                              double&));
  /// Set the function to be called for calculating the weighting field.
  void SetWeightingField(void (*f)(const double, const double, const double,
                                   double&, double&, double&,
                                   const std::string));
  /// Set the function to be called for calculating the weighting potential.
  void SetWeightingPotential(void (*f)(const double, const double, const double,
                                       double&, const std::string));
  /// Set the function to be called for calculating the magnetic field.
  void SetMagneticField(void (*f)(const double, const double, const double,
                                  double&, double&, double&));

 private:
  /// Electric field function
  void (*m_efield)(const double, const double, const double, double&, double&,
                   double&);

  /// Potential function
  void (*m_potential)(const double, const double, const double, double&);

  /// Weighting field function
  void (*m_wfield)(const double, const double, const double, double&, double&,
                   double&, const std::string);

  /// Weighting potential function
  void (*m_wpot)(const double, const double, const double, double&,
                 const std::string);

  /// Magnetic field function
  void (*m_bfield)(const double, const double, const double, double&, double&,
                   double&);

  /// Reset the component
  void Reset();
  // Verify periodicities
  void UpdatePeriodicity();
};
}
#endif

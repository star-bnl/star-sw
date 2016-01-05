// Simple component with electric field given by a user function

#ifndef G_COMPONENT_USER_H
#define G_COMPONENT_USER_H

#include "ComponentBase.hh"

namespace Garfield {

class ComponentUser : public ComponentBase {

 public:
  // Constructor
  ComponentUser();
  // Destructor
  ~ComponentUser() {}

  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, Medium*& m, int& status);
  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, double& v, Medium*& m,
                     int& status);
  bool GetVoltageRange(double& vmin, double& vmax);
  void WeightingField(const double x, const double y, const double z,
                      double& wx, double& wy, double& wz,
                      const std::string& label);
  double WeightingPotential(const double x, const double y, const double z,
                            const std::string& label);

  void SetElectricField(void (*f)(const double, const double, const double,
                                  double&, double&, double&));
  void SetPotential(void (*f)(const double, const double, const double,
                              double&));

  void SetWeightingField(void (*f)(const double, const double, const double,
                                   double&, double&, double&,
                                   const std::string));
  void SetWeightingPotential(void (*f)(const double, const double, const double,
                                       double&, const std::string));

 private:
  // Electric field function
  bool m_hasField;
  void (*m_field)(const double, const double, const double, double&, double&,
                  double&);

  // Potential
  bool m_hasPotential;
  void (*m_potential)(const double, const double, const double, double&);

  // Weighting field
  bool m_hasWeightingField;
  void (*m_wfield)(const double, const double, const double, double&, double&,
                   double&, const std::string);

  // Weighting potential
  bool m_hasWeightingPotential;
  void (*m_wpot)(const double, const double, const double, double&,
                 const std::string);

  // Reset the component
  void Reset();
  // Verify periodicities
  void UpdatePeriodicity();
};
}
#endif

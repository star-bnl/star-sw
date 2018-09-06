#ifndef G_COMPONENT_CONSTANT_H
#define G_COMPONENT_CONSTANT_H

#include "ComponentBase.hh"

namespace Garfield {

/// Component with constant electric field.

class ComponentConstant : public ComponentBase {

 public:
  // Constructor
  ComponentConstant();
  // Destructor
  ~ComponentConstant() {}

  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, Medium*& m, int& status) override;
  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, double& v, Medium*& m,
                     int& status) override;
  bool GetVoltageRange(double& vmin, double& vmax) override;
  void WeightingField(const double x, const double y, const double z,
                      double& wx, double& wy, double& wz,
                      const std::string& label) override;
  double WeightingPotential(const double x, const double y, const double z,
                            const std::string& label) override;

  void SetElectricField(const double ex, const double ey, const double ez);
  void SetPotential(const double x, const double y, const double z,
                    const double v = 0.);

  void SetWeightingField(const double wx, const double wy, const double wz,
                         const std::string label);
  void SetWeightingPotential(const double x, const double y, const double z,
                             const double v = 0.);

 private:
  // Electric field
  double m_fx = 0.;
  double m_fy = 0.;
  double m_fz = 0.;

  // Potential
  bool m_hasPotential = false;
  // Point where potential was specified
  double m_x0 = 0., m_y0 = 0., m_z0 = 0.;
  // Potential at this point
  double m_v0 = 0.;

  // Weighting field
  bool m_hasWeightingField = false;
  std::string m_wfield = "";
  double m_fwx = 0., m_fwy = 0., m_fwz = 0.;
  bool m_hasWeightingPotential = false;
  // Point where the weighting potential was specified
  double m_wx0 = 0., m_wy0 = 0., m_wz0 = 0.;
  // Weighting potential at this point
  double m_w0 = 0.;

  void Reset() override;
  void UpdatePeriodicity() override;
};
}
#endif

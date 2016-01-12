// Component with constant electric field

#ifndef G_COMPONENT_CONSTANT_H
#define G_COMPONENT_CONSTANT_H

#include "ComponentBase.hh"

namespace Garfield {

class ComponentConstant : public ComponentBase {

 public:
  // Constructor
  ComponentConstant();
  // Destructor
  ~ComponentConstant() {}

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

  void SetElectricField(const double ex, const double ey, const double ez);
  void SetPotential(const double x, const double y, const double z,
                    const double v = 0.);

  void SetWeightingField(const double wx, const double wy, const double wz,
                         const std::string label);
  void SetWeightingPotential(const double x, const double y, const double z,
                             const double v = 0.);

 private:
  // Electric field
  double m_fx, m_fy, m_fz;

  // Potential
  bool m_hasPotential;
  // Point where potential was specified
  double m_x0, m_y0, m_z0;
  // Potential at this point
  double m_v0;

  // Weighting field
  bool m_hasWeightingField;
  std::string m_wfield;
  double m_fwx, m_fwy, m_fwz;
  bool m_hasWeightingPotential;
  // Point where the weighting potential was specified
  double m_wx0, m_wy0, m_wz0;
  // Weighting potential at this point
  double m_w0;

  // Reset the component
  void Reset();
  // Verify periodicities
  void UpdatePeriodicity();
};
}
#endif

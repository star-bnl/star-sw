#ifndef G_COMPONENT_ANSYS121_H
#define G_COMPONENT_ANSYS121_H

#include "ComponentFieldMap.hh"

namespace Garfield {

/// Component for importing and interpolating two-dimensional ANSYS field maps.

class ComponentAnsys121 : public ComponentFieldMap {

 public:
  /// Constructor
  ComponentAnsys121();
  /// Destructor
  ~ComponentAnsys121() {}

  Medium* GetMedium(const double x, const double y, const double z) override;
  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, Medium*& m, int& status) override;
  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, double& v, Medium*& m,
                     int& status) override;

  void WeightingField(const double x, const double y, const double z,
                      double& wx, double& wy, double& wz,
                      const std::string& label) override;

  double WeightingPotential(const double x, const double y, const double z,
                            const std::string& label) override;

  bool Initialise(std::string elist = "ELIST.lis",
                  std::string nlist = "NLIST.lis",
                  std::string mplist = "MPLIST.lis",
                  std::string prnsol = "PRNSOL.lis", std::string unit = "cm");
  bool SetWeightingField(std::string prnsol, std::string label);

  void SetRangeZ(const double zmin, const double zmax);

 protected:
  void UpdatePeriodicity() override;

  double GetElementVolume(const unsigned int i) override;
  void GetAspectRatio(const unsigned int i, double& dmin, double& dmax) override;
};
}

#endif

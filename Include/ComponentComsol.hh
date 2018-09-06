#pragma once

#include "ComponentFieldMap.hh"

namespace Garfield {

/// Component for importing and interpolating Comsol field maps.

class ComponentComsol : public ComponentFieldMap {

 public:
  /// Default constructor
  ComponentComsol();
  ComponentComsol(std::string mesh, std::string mplist, std::string field);
  /// Destructor
  ~ComponentComsol() {}

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

  Medium* GetMedium(const double x, const double y, const double z) override;

  bool Initialise(std::string header = "mesh.mphtxt",
                  std::string mplist = "dielectrics.dat",
                  std::string field = "field.txt");

  bool SetWeightingField(std::string file, std::string label);

 protected:
  void UpdatePeriodicity() override { UpdatePeriodicityCommon(); }

  double GetElementVolume(const unsigned int i) override;
  void GetAspectRatio(const unsigned int i, 
                      double& dmin, double& dmax) override;

  struct nodeCmp {
    bool operator()(const ComponentFieldMap::Node& lhs,
                    const ComponentFieldMap::Node& rhs) const {
      double dx = round(lhs.x * 1e6) - round(rhs.x * 1e6);
      double dy = round(lhs.y * 1e6) - round(rhs.y * 1e6);
      double dz = round(lhs.z * 1e6) - round(rhs.z * 1e6);
      return dx < 0 || (dx == 0 && (dy < 0 || (dy == 0 && dz < 0)));
    }
  };
};
}

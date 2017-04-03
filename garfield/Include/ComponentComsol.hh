// Copied and modified ComponentAnsys123.hh

#pragma once

#include "ComponentFieldMap.hh"

namespace Garfield {

class ComponentComsol : public ComponentFieldMap {

 public:
  // Constructors
  ComponentComsol();
  ComponentComsol(std::string mesh, std::string mplist, std::string field);
  // Destructor
  ~ComponentComsol() {}

  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, Medium*& m, int& status);
  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, double& v, Medium*& m,
                     int& status);

  void WeightingField(const double x, const double y, const double z,
                      double& wx, double& wy, double& wz,
                      const std::string& label);

  double WeightingPotential(const double x, const double y, const double z,
                            const std::string& label);

  Medium* GetMedium(const double x, const double y, const double z);

  bool IsInBoundingBox(const double x, const double y, const double z) {
    return x >= xMinBoundingBox && x <= xMaxBoundingBox &&
           y >= yMinBoundingBox && y <= yMaxBoundingBox &&
           z >= zMinBoundingBox && y <= zMaxBoundingBox;
  }

  bool Initialise(std::string header = "mesh.mphtxt",
                  std::string mplist = "dielectrics.dat",
                  std::string field = "field.txt");

  bool SetWeightingField(std::string file, std::string label);

 protected:
  // Verify periodicities
  void UpdatePeriodicity() { UpdatePeriodicityCommon(); }

  double GetElementVolume(const unsigned int i);
  void GetAspectRatio(const unsigned int i, double& dmin, double& dmax);

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

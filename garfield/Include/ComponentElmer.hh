// Copied and modified ComponentAnsys123.hh

#ifndef G_COMPONENT_ELMER_H
#define G_COMPONENT_ELMER_H

#include "ComponentFieldMap.hh"

namespace Garfield {

class ComponentElmer : public ComponentFieldMap {

 public:
  // Constructors
  ComponentElmer();
  ComponentElmer(std::string header, std::string elist, std::string nlist,
                 std::string mplist, std::string volt, std::string unit);
  // Destructor
  ~ComponentElmer() {}

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

  bool Initialise(std::string header = "mesh.header",
                  std::string elist = "mesh.elements",
                  std::string nlist = "mesh.nodes",
                  std::string mplist = "dielectrics.dat",
                  std::string volt = "out.result", std::string unit = "cm");

  bool SetWeightingField(std::string prnsol, std::string label);

 protected:
  // Verify periodicities
  void UpdatePeriodicity() { UpdatePeriodicityCommon(); }

  double GetElementVolume(const unsigned int i);
  void GetAspectRatio(const unsigned int i, double& dmin, double& dmax);
};
}
#endif

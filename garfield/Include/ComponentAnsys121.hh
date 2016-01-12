#ifndef G_COMPONENT_ANSYS121_H
#define G_COMPONENT_ANSYS121_H

#include "ComponentFieldMap.hh"

namespace Garfield {

class ComponentAnsys121 : public ComponentFieldMap {

 public:
  // Constructor
  ComponentAnsys121();
  // Destructor
  ~ComponentAnsys121() {}

  Medium* GetMedium(const double x, const double y, const double z);
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

  bool Initialise(std::string elist = "ELIST.lis",
                  std::string nlist = "NLIST.lis",
                  std::string mplist = "MPLIST.lis",
                  std::string prnsol = "PRNSOL.lis", std::string unit = "cm");
  bool SetWeightingField(std::string prnsol, std::string label);

  // Range
  bool IsInBoundingBox(const double x, const double y, const double z) {
    return x >= xMinBoundingBox && x <= xMaxBoundingBox &&
           y >= yMinBoundingBox && y <= yMaxBoundingBox &&
           z >= zMinBoundingBox && y <= zMaxBoundingBox;
  }

  void SetRangeZ(const double zmin, const double zmax);

 protected:
  // Verify periodicities
  void UpdatePeriodicity();

  double GetElementVolume(const int i);
  void GetAspectRatio(const int i, double& dmin, double& dmax);
};
}

#endif

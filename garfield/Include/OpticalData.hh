#ifndef G_OPTICAL_DATA_H
#define G_OPTICAL_DATA_H

#include <string>
#include <vector>

namespace Garfield {

class OpticalData {

 public:
  // Constructor
  OpticalData() : debug(false) {}
  // Destructor
  ~OpticalData() {}

  bool IsAvailable(const std::string material) const;

  bool GetPhotoabsorptionCrossSection(const std::string material,
                                      const double e, double& cs, double& eta);

  void EnableDebugging() { debug = true; }
  void DisableDebugging() { debug = false; }

 private:
  bool debug;

  bool PhotoAbsorptionCsNeon(const double e, double& cs, double& eta);
  bool PhotoAbsorptionCsArgon(const double e, double& cs, double& eta);

  bool PhotoAbsorptionCsCO2(const double e, double& cs, double& eta);

  bool PhotoAbsorptionCsMethane(const double e, double& cs, double& eta);
  bool PhotoAbsorptionCsEthane(const double e, double& cs, double& eta);
  bool PhotoAbsorptionCsButane(const double e, double& cs, double& eta);
  bool PhotoAbsorptionCsAcetylene(const double e, double& cs, double& eta);
  bool PhotoAbsorptionCsCF4(const double e, double& cs, double& eta);

  bool PhotoAbsorptionCsNitrogen(const double e, double& cs, double& eta);
};
}

#endif

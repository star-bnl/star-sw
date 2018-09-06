#ifndef G_OPTICAL_DATA_H
#define G_OPTICAL_DATA_H

#include <string>
#include <vector>

namespace Garfield {

/// Photoabsorption cross-sections for some gases.
 
class OpticalData {

 public:
  /// Constructor
  OpticalData();
  /// Destructor
  ~OpticalData() {}

  /// Check whether optical data have been implemented for a given gas.
  bool IsAvailable(const std::string& material) const;
  /// Photo-absorption cross-section and ionisation yield at a given energy.
  bool GetPhotoabsorptionCrossSection(const std::string& material,
                                      const double e, double& cs, double& eta);

  void EnableDebugging(const bool on = true) { m_debug = on; }

 private:
  static constexpr double OscToPacs = 8.067283e-18;
  static constexpr double Mbarn = 1.e-18;

  bool m_debug = false;

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

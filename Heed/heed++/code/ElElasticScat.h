#ifndef EL_ELASTIC_SCAT_H
#define EL_ELASTIC_SCAT_H

#include <vector>

namespace Heed {

/// Fit parameters for a particular element and energy.
/// The fit gives dependence of cross section on angle.
class ElElasticScatDataStruct {
 public:
  double A[4];  ///< If -1.0 then the combination is not valid.
  double C[7];
  double B;
  double CS(const double theta) const;  ///< Return -1 if not valid
};

/// Array of ElElasticScatDataStruct objects for a set of energies.
class ElElasticScatData {
 public:
  long Z = 0;
  /// Fit parameters at different energies.
  std::vector<ElElasticScatDataStruct> data;
  /// Default constructor
  ElElasticScatData() = default;
  /// Constructor with atomic number and number of energies.
  ElElasticScatData(long fZ, long qe) : Z(fZ), data(qe) {}
};

/// Definition of elastic scattering for low-energy delta-electron.
/// The class contains the data for all atoms presents them by request.
/// 2003, I. Smirnov

class ElElasticScat {
 public:
  /** Get the cross-section (in angstrom^2/srad).
    * \param Z atomic number
    * \param energy kinetic energy in MeV
    * \param angle angle
    * \param s_interp flag for debugging and various checks
    *
    * fill_hist call this function with s_interp=1 for histograms "int...".
    */
  double get_CS(long Z, double energy, double angle, int s_interp = 0);

  /** Get the cross-section (in angstrom^2).
    * \param Z atomic number
    * \param energy kinetic energy in MeV
    * \param angle angle in internal units (radian)
    */
  double get_CS_Rutherford(long Z, double energy, double angle);

  long get_qe(void) const { return qe; }
  double get_energy_mesh(long ne) const { return energy_mesh[ne]; }

  /// Default constructor
  ElElasticScat() = default;
  /// Constructor with file name.
  ElElasticScat(const std::string& file_name);
  void print(std::ostream& file, int l) const;

 private:
  /// Number of energies (local mesh)
  long qe = 0;
  /// Energies [keV]
  std::vector<double> energy_mesh;
  /// gamma * beta2 for electron of this energy
  std::vector<double> gamma_beta2;
  /// Data for different atoms.
  std::vector<ElElasticScatData> atom;

  // kinetic energy in MeV
  double get_CS_for_presented_atom(long na, double energy, double angle);
};

class ElElasticScatLowSigma {
 public:
  double get_mean_coef(const long Z, const long ne) const {
    return mean_coef[Z - 1][ne];
  }
  double get_coef(const long Z, const long ne) const { return coef[Z - 1][ne]; }
  long get_qscat(void) const { return qscat; }
  ElElasticScat* get_ees() const { return ees; }
 
  /// Default constructor
  ElElasticScatLowSigma() = default;
  ElElasticScatLowSigma(ElElasticScat* fees, const std::string& file_name);

 private:
  ElElasticScat* ees = nullptr;
  /// Number of atoms registered in this class (Z is sequential)
  long qat = 0;
  /// Maximal number of scatterings
  long qscat = 0;
  /// mean((1 - cos(theta))) as function of Z and energy
  std::vector<std::vector<double> > mean_coef;
  /// sqrt(mean((1-cos(theta))^2))
  std::vector<std::vector<double> > coef;
};
}

#endif

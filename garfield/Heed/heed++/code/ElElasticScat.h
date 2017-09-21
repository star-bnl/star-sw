#ifndef ELELASCTICSCAT_H
#define ELELASCTICSCAT_H

#include <vector>

#include "wcpplib/safetl/AbsPtr.h"
#include "HeedGlobals.h"

// People, who does not want to link with graphics,
// can uncomment the following macro.
// The result will be the disappearance of the two functions
// from class ElElasticScat, which are the only functions in heed++,
// which call histdef.
// These functions were used at preparation of the condensed simulation
// scheme, namely fitting condensed cross-sections.
// During actual simulations these two functions are not necessary.
//#define EXCLUDE_FUNCTIONS_WITH_HISTDEF

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

/// Contains array of the structures defined above for a set of energies.
class ElElasticScatData {
 public:
  long Z;
  /// Fit parameters at different energies.
  std::vector<ElElasticScatDataStruct> data;
  /// Default constructor
  ElElasticScatData(void) : Z(0) {}
  /// Constructor with atomic number and number of energies.
  ElElasticScatData(long fZ, long qe) : Z(fZ), data(qe) {}
};

/// Definition of elastic scattering for low-energy delta-electron.
/// The class contains the data for all atoms presents them by request.
/// 2003, I. Smirnov

class ElElasticScat : public RegPassivePtr {
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
  ElElasticScat(void) : atom(0) {}
  /// Constructor with file name.
  ElElasticScat(const std::string& file_name);
  void print(std::ostream& file, int l) const;
#ifndef EXCLUDE_FUNCTIONS_WITH_HISTDEF
  void fill_hist(void);
  // Makes a package of histograms for all atoms for which the
  // fit is presented in the data file.
  // There are 6 types of histograms:
  // raw..., cor..., corrad, int..., rut..., rutrad
  // Difference between raw and cor I forgot for the moment.
  // The plots looks the same but created by diffeent manner.
  // raw by the call of atom[na].data[ne].CS(angle/180.0 * M_PI );
  // cor by the normal call:
  // get_CS(atom[na].Z, energyMeV, angle/180.0 * M_PI );
  // int is produced by interpolation between neighboring presented atoms.
  // It is useful, in particular, to check the precision of interpolation.
  // rut is Rutherford cross section.
  // histograms with suffix rad are the same but with factor
  //  2.0 * M_PI * sin(anglerad)
  // path length is inverse linear coefficient of absorption
  // for unit A and density (they are not known in this program)
  // So you should multiply by A and divide by density in gr/cm3.

  void fill_hist_low_scat(const std::string& file_name,
                          const std::string& file_name_dist);
// It fills some histograms and write file with tables
// energy vs coefficient which gives dependency (proportional)
// of root from dispertion
// on number of interactions.
// If file_name_dist != "" and "none",
// the program will write there the shapes of distributions.
#endif
 private:
  /// Number of energies (local mesh)
  long qe;
  /// Energies [keV]
  std::vector<double> energy_mesh;
  /// gamma * beta2 for electron of this energy
  std::vector<double> gamma_beta2;
  /// Data for different atoms.
  std::vector<ElElasticScatData> atom;

  // kinetic energy in MeV
  double get_CS_for_presented_atom(long na, double energy, double angle);
};

class ElElasticScatLowSigma : public RegPassivePtr {
 public:
  double get_mean_coef(const long Z, const long ne) const {
    return mean_coef[Z - 1][ne];
  }
  double get_coef(const long Z, const long ne) const { return coef[Z - 1][ne]; }
  long get_qscat(void) const { return qscat; }
  ElElasticScat* get_ees(void) const { return ees.get(); }
  ElElasticScatLowSigma(void) {}
  ElElasticScatLowSigma(ElElasticScat* fees, const std::string& file_name);

 private:
  PassivePtr<ElElasticScat> ees;
  /// Number of atoms registered in this class (Z is sequential)
  long qat;
  /// Maximal number of scatterings
  long qscat;
  /// mean((1 - cos(theta))) as function of Z and energy
  std::vector<std::vector<double> > mean_coef;
  /// sqrt(mean((1-cos(theta))^2))
  std::vector<std::vector<double> > coef;
};
}

#endif

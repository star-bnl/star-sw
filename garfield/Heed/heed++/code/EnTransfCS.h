#ifndef ENTRANFCS_H
#define ENTRANFCS_H

#include "heed++/code/HeedMatterDef.h"

namespace Heed {

#define EXCLUDE_A_VALUES   // exclude absorption values

/// The PAI cross section of energy transfers from charged particle to media.
/// The particle has fixed parameters (energy, speed, etc.), which
/// are not affected by energy transfers, since they are considered
/// too small compared with the particle energy.
///
/// 2003, I. Smirnov

class EnTransfCS {
 public:
  /// Default constructor
  EnTransfCS() = default;
  /// Constructor
  EnTransfCS(double fparticle_mass, double fgamma_1, bool fs_primary_electron,
             HeedMatterDef* fhmd, long fparticle_charge = 1);

  void print(std::ostream& file, int l) const;
  EnTransfCS* copy() const { return new EnTransfCS(*this); }

  /// Particle mass [MeV]
  double particle_mass = 0.;
  /// Charge in units of electron charge (used square, sign does not matter).
  long particle_charge = 0;

  /// Lorentz factor - 1 (the best dimensionless measurement of speed).
  double gamma_1 = 0.;

  /// Max. energy transfer [MeV]
  double max_etransf = 0.;
  /// Flag controlling the form of Rutherford scattering.
  /// For our purposes it is good to have simple form,
  /// so this variable is initialized to 1.
  /// Simple form means that there are two terms.
  /// The third term is assumed zero.
  bool s_simple_form = true;
  /// Flag indicating whether the primary particle is an electron.
  bool s_primary_electron = false;

  HeedMatterDef* hmd = nullptr;

  /// In the following arrays there is the only index: the energy.
  /// The meaning: the average value on the energy interval.
  std::vector<double> log1C;        ///< common first log without cs
  std::vector<double> log2C;        ///< common second log without cs
  std::vector<double> chereC;       ///< Cherenkov's radiation
  std::vector<double> chereCangle;  ///< angle of Cherenkov's radiation
  std::vector<double> Rruth;        ///< term called R in my paper

  /// Sum of (ionization) differential cross-section terms
  std::vector<double> addaC;
  /// Integrated (ionization) cross-section
  double quanC = 0.;

#ifndef EXCLUDE_A_VALUES
  /// Sum of (absorption) differential cross-section terms
  std::vector<double> addaC_a;
  /// Integrated (absorption) cross-section
  double quanC_a = 0.;
#endif

  // First moment (mean restricted energy loss) [MeV]
  double meanC = 0.;
  // First moment with additional tail to max. kinematically allowed transfer,
  // calculated only for heavy particles (integral for electrons non-trivial).
  double meanC1 = 0.;
#ifndef EXCLUDE_A_VALUES
  double meanC1_a = 0.;
  double meanC_a = 0.;
#endif

  /// In the following arrays there are three indices:
  /// atom number in the matter, shell number in atom, energy
  /// Fraction of Cherenkov term.
  std::vector<std::vector<std::vector<double> > > cher;
  /// Rutherford term
  std::vector<std::vector<std::vector<double> > > fruth;
  /// Sum
  std::vector<std::vector<std::vector<double> > > adda;
  /// Integral, normalised to unity
  std::vector<std::vector<std::vector<double> > > fadda;
#ifndef EXCLUDE_A_VALUES
  /// Cherenkov term (total absorption)
  std::vector<std::vector<std::vector<double> > > cher_a;
  /// Sum (total absorption)
  std::vector<std::vector<std::vector<double> > > adda_a;
  /// Integral (total absorption), normalised to unity
  std::vector<std::vector<std::vector<double> > > fadda_a;
#endif

  /// Number of collisions / cm, for each atom and shell.
  std::vector<std::vector<double> > quan;
  /// First moment, for each atom and shell.
  std::vector<std::vector<double> > mean;
#ifndef EXCLUDE_A_VALUES
  /// Number of collisions / cm (total absorption), for each atom and shell.
  std::vector<std::vector<double> > quan_a;
  /// First moment (total absorption), for each atom and shell.
  std::vector<std::vector<double> > mean_a;
#endif

  std::vector<double> length_y0;
};
}

#endif

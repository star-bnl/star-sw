#ifndef ENTRANFCS_H
#define ENTRANFCS_H
#include "heed++/code/HeedMatterDef.h"

/*
The PAI cross section of energy transfers from charged particle
to media.
The particle has fixed parameters: energy, speed, etc., which
are not affected to energy transfers, since they are considered
too little if compared with the particle energy.

2003, I. Smirnov
*/

//#define DEBUG_EnTransfCS  // allows to print additional information
// and keeps it in class, which makes it bigger

#define EXCLUDE_A_VALUES   // exclude absorption values
#define EXCLUDE_VAL_FADDA  // exclude values not necessary for MC
// #define EXCLUDE_MEAN       // exclude calculation of means

#define CLEAR_ARRAYS

class EnTransfCS : public RegPassivePtr {
 public:
  /// Constructors
  EnTransfCS(void) {}
  ;
  EnTransfCS(double fparticle_mass, double fgamma_1, int fs_primary_electron,
             HeedMatterDef* fhmd, long fparticle_charge = 1);
  virtual void print(std::ostream& file, int l) const;
  macro_copy_total(EnTransfCS);

  /// Particle mass [MeV]
  double particle_mass;
  /// Kinetic energy [MeV]
  double particle_tkin;
  /// Total energy [MeV]
  double particle_ener;
  /// Charge in units of electron charge (used square, sign does not matter).
  long particle_charge;

  double beta;
  double beta2;   // beta^2
  double beta12;  // 1 - beta^2
  double gamma;
  double gamma_1;  // gamma - 1 (the best dimensionless measurement of speed)

  /// Max. energy transfer [MeV]
  double maximal_energy_trans;
  /// Flag controlling the form of Rutherford scattering.
  /// For our purposes it is good to have simple form,
  /// so this variable is initialized to 1.
  /// Simple form means that there are two terms.
  /// The third term is assumed zero.
  int s_simple_form;
  // Flag that the primary particle is the electron
  int s_primary_electron;

  PassivePtr<HeedMatterDef> hmd;

  /// In the following arrays there is the only index: the energy.
  /// The meaning: the average value on the energy interval.
  DynLinArr<double> log1C;        // common first log without cs
  DynLinArr<double> log2C;        // common second log without cs
  DynLinArr<double> chereC;       // Cherenkov's radiation
  DynLinArr<double> chereCangle;  // angle of Cherenkov's radiation
  DynLinArr<double> Rreser;       // term called R in my paper
#ifdef DEBUG_EnTransfCS
  DynLinArr<double> treser;  // total rezer, sum of frezer
// by atoms and shells, per one electron ( in the paper it is per atom)
#endif

  /// Sum of (ionization) differential cross-section terms
  DynLinArr<double> addaC;
  /// Integrated (ionization) cross-section
  double quanC;

#ifndef EXCLUDE_A_VALUES
  /// Sum of (absorption) differential cross-section terms
  DynLinArr<double> addaC_a;
  /// Integrated (absorption) cross-section
  double quanC_a;
#endif

#ifndef EXCLUDE_MEAN
  // First moment (mean restricted energy loss) [MeV]
  double meanC;
  // First moment, calculated using left side of energy interval.
  // The value should be less than above.
  // The difference shows the uncertainty of the value above
  // arising from the finite number of intervals
  double meanCleft;
  // First moment with additional tail to max. kinematically allowed transfer,
  // calculated only for heavy particles (integral for electrons non-trivial).
  double meanC1;
#ifndef EXCLUDE_A_VALUES
  double meanC1_a;
  double meanC_a;
#endif
  // Secondary ionization
  double meaneleC;
  double meaneleC1;
#endif  // EXCLUDE_MEAN

  /// In the following arrays there are three indices:
  /// atom number in the matter, shell number in atom, energy
  /// Fraction of Cherenkov term.
  DynLinArr<DynLinArr<DynLinArr<double> > > cher;
  /// Rutherford term
  DynLinArr<DynLinArr<DynLinArr<double> > > frezer;
  /// Sum
  DynLinArr<DynLinArr<DynLinArr<double> > > adda;
  /// Integral, normalised to unity
  DynLinArr<DynLinArr<DynLinArr<double> > > fadda;
#ifndef EXCLUDE_A_VALUES
  DynLinArr<DynLinArr<DynLinArr<double> > > cher_a;
  DynLinArr<DynLinArr<DynLinArr<double> > > adda_a;
  DynLinArr<DynLinArr<DynLinArr<double> > > fadda_a;
#endif

#ifndef EXCLUDE_VAL_FADDA
  // The true values of the integral (should be equal to quan)
  DynLinArr<DynLinArr<double> > val_fadda;  // integral * hmd->xeldens;
#ifndef EXCLUDE_A_VALUES
  DynLinArr<DynLinArr<double> > val_fadda_a;  // integral * hmd->xeldens;
#endif
#endif

  /// In the following arrays there are two indices:
  /// atom number in the matter, shell number in atom.
  DynLinArr<DynLinArr<double> > quan;  // per 1 cm, used for path length
#ifndef EXCLUDE_A_VALUES
  DynLinArr<DynLinArr<double> > quan_a;
#endif
#ifndef EXCLUDE_MEAN
  DynLinArr<DynLinArr<double> > mean;
#ifndef EXCLUDE_A_VALUES
  DynLinArr<DynLinArr<double> > mean_a;
#endif
#endif

  DynLinArr<double> length_y0;

};

class EnTransfCSType {
 public:
  PassivePtr<EnTransfCS> etcs;
  EnTransfCSType(void) : etcs() { ; }
  EnTransfCSType(EnTransfCS* md) : etcs(md) { ; }
};
std::ostream& operator<<(std::ostream& file, const EnTransfCSType& f);

#endif

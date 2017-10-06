#ifndef PHOTOABSCS_H
#define PHOTOABSCS_H

#include <vector>
#include <fstream>
#include <cmath>
#include <cfloat>
#include <climits>
#include "wcpplib/safetl/AbsPtr.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"
#include "heed++/code/PhysicalConstants.h"

namespace Heed {

/// TRK sum rule [1/MeV], constant per one electron.
const double Thomas_sum_rule_const =
    2 * CLHEP::pi2 * CLHEP::fine_structure_const / CLHEP::electron_mass_c2;
/// TRK sum rule [Mb * MeV].
const double Thomas_sum_rule_const_Mb =
    Thomas_sum_rule_const * 1.0E-6 / C1_MEV2_BN;

/// Photoabsorption cross-section base class.
///
/// The literature data on photoabsorption cross section are fragmentar and
/// not always consistent. This class hierarchy is designed to
/// gather them in a consistent library.
/// The principle is ordinary: definition of an abstract class
/// which defines the interface available for the rest of program,
/// and definition of derived classes with this or that realization.
/// To the contrary with wcpplib/matter, there is no any global "database"
/// and no formal ban to duplicate these definitions.
/// So these are simple classes determining photoabsorption
/// cross sections for atomic shells, for atoms, and for molecules.
/// Also the atomic relaxation cascades are defined.
/// The system requires some memory for keeping data, and some disk files
/// with input information. It takes some time for initializations, so
/// it is not intended to be used in a loop.
/// In the fortran version of HEED, the equivalent data structure
/// kept in a common block was depending on energy mesh.
/// But in this C++ version it was found possible to avoid this dependence.
/// The data are kept and handled as is, without unnecessary conversions.
/// This improves precision of handling energies near ionization thresholds.
///
/// 2004, I. Smirnov

class PhotoAbsCS {
 public:
  /// Default constructor.
  PhotoAbsCS();
  /// Constructor
  PhotoAbsCS(const std::string& fname, int fZ, double fthreshold);
  /// Destructor
  virtual ~PhotoAbsCS() {}

  /// Name of this shell or atom.
  const std::string& get_name() const { return name; }
  /// Number of this shell.
  int get_number() const { return number; }
  /// Charge or number of electrons at this shell or in this atom
  /// (in principle the integral of CS should
  /// satisfy the Thomas-Reiche-Kuhn sum rule).
  int get_Z() const { return Z; }
  /// Return the threshold energy.
  double get_threshold() const { return threshold; }
  /// Retrieve cross-section [Mb] at a given energy [MeV].
  virtual double get_CS(double energy) const = 0;
  /// Retrieve integral cross-section [Mb * MeV] in a given interval [MeV].
  virtual double get_integral_CS(double energy1, double energy2) const = 0;
  /// Multiply by some factor. Can be useful for debugging and other purposes.
  virtual void scale(double fact) = 0;

  virtual void print(std::ostream& file, int l) const;
  virtual PhotoAbsCS* copy() const = 0;

 protected:
  std::string name;
  int number;
  int Z;
  double threshold;  // in MeV
};

/// Smoothed/smeared photoabsorption cross-section
class AveragePhotoAbsCS : public PhotoAbsCS {
  ActivePtr<PhotoAbsCS> real_pacs;
  double width;
  /// Max. number of integration steps (in get_integral_CS).
  /// If real q is more, the function calls PhotoAbsCs::get_integral_CS.
  long max_q_step;
  /// Integration step, for example, 1/20 from width.
  double step;

 public:
  /// Default constructor.
  AveragePhotoAbsCS() : PhotoAbsCS() {}
  /** Constructor.
    * \param apacs
             photoabsorption cross-section
    * \param fwidth
             width [MeV] for smoothing
    * \param fstep
             step size [MeV] for numerical integration
    * \param fmax_q_step
             max number of integration steps
    */
  AveragePhotoAbsCS(PhotoAbsCS* apacs, double fwidth, double fstep,
                    long fmax_q_step);
  /// Destructor
  virtual ~AveragePhotoAbsCS() {}
  virtual double get_CS(double energy) const;
  virtual double get_integral_CS(double energy1, double energy2) const;

  virtual void scale(double fact);

  virtual void print(std::ostream& file, int l) const;
  virtual AveragePhotoAbsCS* copy() const {
    return new AveragePhotoAbsCS(*this);
  }
};

/// Hydrogen: empirical fit of Kosarev & Podoliak.
/// Original formula for molecular hydrogen.
/// Since this class for separated shell, we divide the molecular CS by two.
class HydrogenPhotoAbsCS : public PhotoAbsCS {
 public:
  /// Constructor
  HydrogenPhotoAbsCS();
  /// Destructor
  virtual ~HydrogenPhotoAbsCS() {}
  virtual double get_CS(double energy) const;
  virtual double get_integral_CS(double energy1, double energy2) const;
  virtual void scale(double fact);

  virtual void print(std::ostream& file, int l) const;
  virtual HydrogenPhotoAbsCS* copy() const {
    return new HydrogenPhotoAbsCS(*this);
  }

 private:
  double prefactor;
};

/// Typically this is for reading Experimental CS, for example of argon,
/// as if there is one shell.
/// File is two-column table, the first row is energy in eV,
/// the second one is CS in Mbarn.
/// The points are understood as local points.
/// The interpolation between them are either straight or by power
/// function.
/// the choice is determined by function sign_nonlinear_interpolation
/// (see above).
/// If the first point is not zero cross section,
/// and the threshold is to the left,
/// the straight extrapolation is performed from the two edge points.
/// The extrapolation to the right is performed to the end of
/// the energy mesh by power function with power -2.75.
/// The minimal number of points is 2, as in PointCoorMesh from tline.h
/// The zero number of points is allowed as well.
/// Then the cross section is assumed to be zero.

class SimpleTablePhotoAbsCS : public PhotoAbsCS {
 public:
  /// Default constructor.
  SimpleTablePhotoAbsCS();
  /// Constructor for reading table from file.
  SimpleTablePhotoAbsCS(const std::string& fname, int fZ, double fthreshold,
                        const std::string& ffile_name);
  /// Constructor from given energy and cross-section tables.
  SimpleTablePhotoAbsCS(const std::string& fname, int fZ, double fthreshold,
                        const std::vector<double>& fener,
                        const std::vector<double>& fcs);
  /// Constructor from fit parameters.
  /// Fit formula from Band-Band-Trzaskovskaya et al.
  /// It is difficult to integrate those formulas analytically,
  /// so I create numerical array and treat it as input data.
  SimpleTablePhotoAbsCS(const std::string& fname, int fZ, double fthreshold,
                        int l, double E0, double yw, double ya, double P,
                        double sigma);
  /// Replace part of the cross-section table.
  SimpleTablePhotoAbsCS(const SimpleTablePhotoAbsCS& total,
                        const SimpleTablePhotoAbsCS& part, double emax_repl);
  /// Destructor
  virtual ~SimpleTablePhotoAbsCS() {}
  /// Remove points with zero cross-section from the table.
  void remove_leading_zeros();
  /// Remove points with cross section below a given level from the table.
  /// The function is designed for Henke tables, which are prepared for database
  /// with leading values like 1.0e-15 .
  /// Both functions allow to use the straight interpolation to threshold
  void remove_leading_tiny(double level);

  virtual double get_CS(double energy) const;
  virtual double get_integral_CS(double energy1, double energy2) const;
  const std::vector<double>& get_arr_ener() const { return ener; }
  const std::vector<double>& get_arr_CS() const { return cs; }
  virtual void scale(double fact);
  virtual void print(std::ostream& file, int l) const;
  virtual SimpleTablePhotoAbsCS* copy() const {
    return new SimpleTablePhotoAbsCS(*this);
  }

 private:
  /// Filename (saved for printing).
  std::string file_name;
  /// Table of energies [MeV].
  std::vector<double> ener;
  /// Cross-section values at these energies.
  std::vector<double> cs;
};

/// Simple phenomenological CS for any shell (analytic formula).
class PhenoPhotoAbsCS : public PhotoAbsCS {
 public:
  /// Default constructor.
  PhenoPhotoAbsCS();
  /** Constructor
    * \param fname name of the shell or atom
    * \param fZ number of electrons
    * \param fthreshold threshold level
    * \param fpower positive number \f$x\f$ in \f$1/E^{-x}\f$
    */
  PhenoPhotoAbsCS(const std::string& fname, int fZ, double fthreshold,
                  double fpower = 2.75);
  /// Destructor.
  virtual ~PhenoPhotoAbsCS() {}
  virtual double get_CS(double energy) const;
  virtual double get_integral_CS(double energy1, double energy2) const;
  virtual void scale(double fact);
  virtual void print(std::ostream& file, int l) const;
  virtual PhenoPhotoAbsCS* copy() const { return new PhenoPhotoAbsCS(*this); }

 private:
  double power;
  double factor;
};

/// Sampling of secondary particles.
/// The initial photo-electron is not included.
/// The photo-electron is completely independent on the secondary channel
/// and it always has an energy equal to transferred energy minus shell energy.
/// This class proposes particles which can be emitted at filling this shell.
/// This class can be interpreted as an additional channels for
/// the default one.
/// The sum of channel_prob_dens may be less than unity.
/// The concrete channel for the particular event is sampled by get_channel().
/// If the random sampling does not point to one of the channels registering
/// in this class, get_channel returns 0, which must be interpreted as
/// the use of standard channel.

class AtomicSecondaryProducts : public RegPassivePtr {
 public:
  /// Constructor
  AtomicSecondaryProducts()
      : channel_prob_dens(), electron_energy(), photon_energy() {}
  /// Destructor
  virtual ~AtomicSecondaryProducts() {}
  /// Sample a channel (photon and electron energies in MeV).
  /// Return 1 if the channel is generated and 0 if not.
  int get_channel(std::vector<double>& felectron_energy,
                  std::vector<double>& fphoton_energy) const;

  /** Add new decay channel. Should be used at initialization.
    * \param fchannel_prob_dens probability for this channel.
    * \param felectron_energy electron energies [MeV]
    * \param fphoton_energy photon energies [MeV]
    * \param s_all_rest if 1, the probability of this channel is assigned
    to that what is left to 1.
    fchannel_prob_dens is then ignored, it can be just 0.
    */
  void add_channel(double fchannel_prob_dens,
                   const std::vector<double>& felectron_energy,
                   const std::vector<double>& fphoton_energy,
                   int s_all_rest = 0);

  virtual void print(std::ostream& file, int l) const;

 protected:
  // Probability of specific channel.
  std::vector<double> channel_prob_dens;
  // Arrays of decay products for each channel.
  std::vector<std::vector<double> > electron_energy;
  std::vector<std::vector<double> > photon_energy;
};

/// Atomic photoabsorption cross-section base class.
class AtomPhotoAbsCS : public RegPassivePtr {
 public:
  /// Default constructor.
  AtomPhotoAbsCS();

  int get_Z() const { return Z; }
  inline unsigned int get_qshell() const { return qshell; }
  virtual double get_threshold(int nshell) const = 0;
  virtual double get_I_min() const;
  virtual double get_ACS(double energy) const = 0;
  virtual double get_integral_ACS(double energy1, double energy2) const = 0;
  virtual double get_ACS(int nshell, double energy) const = 0;
  virtual double get_integral_ACS(int nshell, double energy1,
                                  double energy2) const = 0;
  // photo-absorption cross section, energy in MeV, CS in Mbarns.
  // It can include excitation.

  virtual double get_ICS(double energy) const = 0;
  virtual double get_TICS(double energy,
                          double factual_minimal_threshold) const;
  virtual double get_integral_ICS(double energy1, double energy2) const = 0;
  virtual double get_integral_TICS(double energy1, double energy2,
                                   double factual_minimal_threshold) const;
  virtual double get_ICS(int nshell, double energy) const = 0;
  virtual double get_TICS(int nshell, double energy,
                          double factual_minimal_threshold) const;
  virtual double get_integral_ICS(int nshell, double energy1,
                                  double energy2) const = 0;
  virtual double get_integral_TICS(int nshell, double energy1, double energy2,
                                   double factual_minimal_threshold) const;
  // The last function is convenient to redefine ionization threshold in
  // atomic mixtures, where one atom can pass excitation to another,
  // which is easier to ionize.

  // photo-ionization cross section, energy in MeV, CS in Mbarns.
  // It does not include excitation.

  // always assuming ionization
  // Energy could be a little bit less than threshold.
  // This is considered as numerical inprecision
  // The photo-electron has to be the first in array el_energy.
  // Later (in class HeedPhoton) the photo-electron is emitted ahead.
  // The other ones fly  in any direction.
  virtual void get_escape_particles(int nshell,     // input
                                    double energy,  // input
                                    std::vector<double>& el_energy,
                                    std::vector<double>& ph_energy) const;

  virtual int get_main_shell_number(int nshell) const = 0;
  // returns the shell number (1,2,...)
  // The current versions read it from shell name,
  // so they interprete the shell name as the number.
  // If the shell number cannot be recovered, the function
  // returns -1.

  virtual void remove_shell(int nshell);
  virtual void restore_shell(int nshell);
  virtual void print(std::ostream& file, int l) const;
  virtual AtomPhotoAbsCS* copy() const = 0;

  AtomicSecondaryProducts* get_asp(int nshell);

 protected:
  std::string name;
  int Z;
  int qshell;
  // 0 - sign to use shell. 1 - sign to ignore it
  // It does not affect threshold and escape sequences and assumed to
  // manipulate with larger  shells, to investigate their
  // influence at the final characteristics.
  // By default all is 0
  std::vector<bool> s_ignore_shell;
  std::vector<AtomicSecondaryProducts> asp;
};
std::ostream& operator<<(std::ostream& file, const AtomPhotoAbsCS& f);

/// Simple atomic photoabsorption cross-section
/// (no difference between absorption and ionization).

class SimpleAtomPhotoAbsCS : public AtomPhotoAbsCS {
 public:
  /// Default constructor.
  SimpleAtomPhotoAbsCS();
  /// Constructor for reading name and shell energies from file.
  /// Generates the CS by PhenoPhotoAbsCS.
  SimpleAtomPhotoAbsCS(int fZ, const std::string& ffile_name);
  /// Constructor with one prepared preliminary shell with Z electrons.
  /// Convenient for hydrogen.
  SimpleAtomPhotoAbsCS(int fZ, const PhotoAbsCS& fasc);
  /// Destructor
  virtual ~SimpleAtomPhotoAbsCS() {}

  virtual double get_threshold(int nshell) const;
  virtual double get_ACS(double energy) const;
  virtual double get_integral_ACS(double energy1, double energy2) const;
  virtual double get_ACS(int nshell, double energy) const;
  virtual double get_integral_ACS(int nshell, double energy1,
                                  double energy2y) const;
  // photo-absorption cross section, energy in MeV, CS in Mbarns.
  virtual double get_ICS(double energy) const;
  virtual double get_integral_ICS(double energy1, double energy2) const;
  virtual double get_ICS(int nshell, double energy) const;
  virtual double get_integral_ICS(int nshell, double energy1,
                                  double energy2) const;
  // photo-ionization cross section, energy in MeV, CS in Mbarns.
  virtual int get_main_shell_number(int nshell) const {
    return acs[nshell]->get_number();
  }
  virtual void print(std::ostream& file, int l) const;
  virtual SimpleAtomPhotoAbsCS* copy() const {
    return new SimpleAtomPhotoAbsCS(*this);
  }

 protected:
  /// Filename (saved for printing).
  std::string file_name;
  std::vector<ActivePtr<PhotoAbsCS> > acs;
};

const double low_boundary_of_excitations = 0.7;  // from ionization threshold

/// Atomic photo-absorption with excitation.
class ExAtomPhotoAbsCS : public AtomPhotoAbsCS {
 public:
  virtual double get_threshold(int nshell) const;
  /// Photo-absorption cross section (energy in MeV, CS in Mbarns).
  virtual double get_ACS(double energy) const;
  /// Integral photo-absorption cross-section.
  virtual double get_integral_ACS(double energy1, double energy2) const;
  /// Photo-absorption cross-section for a given shell.
  virtual double get_ACS(int nshell, double energy) const;
  /// Integral photo-absorption cross-section for a given shell.
  virtual double get_integral_ACS(int nshell, double energy1,
                                  double energy2) const;

  /// Photo-ionization cross section (energy in MeV, CS in Mb).
  virtual double get_ICS(double energy) const;
  /// Integral photo-ionization cross-section.
  virtual double get_integral_ICS(double energy1, double energy2) const;
  /// Photo-ionization cross-section for a given shell.
  virtual double get_ICS(int nshell, double energy) const;
  /// Integral photo-ionization cross-section for a given shell.
  virtual double get_integral_ICS(int nshell, double energy1,
                                  double energy2) const;
  virtual int get_main_shell_number(int nshell) const {
    return acs[nshell]->get_number();
  }
  void replace_shells_by_average(double fwidth,  // MeV
                                 double fstep, long fmax_q_step);
  virtual void print(std::ostream& file, int l) const;
  virtual ExAtomPhotoAbsCS* copy() const { return new ExAtomPhotoAbsCS(*this); }

  /// Default constructor.
  ExAtomPhotoAbsCS() : AtomPhotoAbsCS() {}

  /** Constructor,
    * \param fZ 
             atomic number
    * \param fthreshold_file_name
             file from which to read name and shell energies
    * \param fsimple_table_file_name
             file from which to read the cross-sections
    * \param fname
             name of the atom, if "none" it is taken from fthreshold_file_name
    * \param fminimal_threshold
             threshold
    */
  ExAtomPhotoAbsCS(int fZ, const std::string& fthreshold_file_name,
                   const std::string& fsimple_table_file_name,
                   const std::string& fname = "none",
                   double fminimal_threshold = 0.0);

  /** Constructor, shells from Band and Thragzkovskaya.
    * \param fZ
             atomic number
    * \param fname
             name of the atom
    * \param fBT_file_name
             file with shell names and energies
    *  \param id
             1 - old files without fluorescence rate
             2 - new files with fluorescence rate
             other values - error
    */
  ExAtomPhotoAbsCS(int fZ, const std::string& fname,
                   const std::string& fBT_file_name, int id,
                   double fminimal_threshold = 0.0);

  /** Constructor, shells and fit parameters from Band and Thragzkovskaya.
    * \param fZ
             atomic number
    * \param fname
             name of the atom
    * \param fFitBT_file_name
             file with shell names, energies, and fit parameters
    * \param id
             1 - old files without fluorescence rate
             2 - new files with fluorescence rate
             other values - error
    * \param s_no_scale
             scaling is not done, needs for next (?)
    * \param fminimal_threshold
             threshold 
    **/
  ExAtomPhotoAbsCS(int fZ, const std::string& fname,
                   const std::string& fFitBT_file_name,
                   int id, int s_no_scale,
                   double fminimal_threshold = 0.0);

  /** Constructor, combination of Band and Thragzkovskaya fit and Henke tables.
    * Initialize BT fit and replaces the part of the first shell
    * from threshold taken from BT- fit to emax_repl by values from the table.
    * \param fZ
             atomic number
    * \param fname
             name of the atom
    * \param fFitBT_file_name
             file with shell names, energies, and fit parameters
    * \param fsimple_table_file_name
             file with cross-section table
    * \param emax_repl
             energy up to which to use the cross-section table
    * \param id
             1 - old files without fluorescence rate
             2 - new files with fluorescence rate
             other values - error
    * \param fminimal_threshold
             threshold 
    **/
  ExAtomPhotoAbsCS(int fZ, const std::string& fname,
                   const std::string& fFitBT_file_name,
                   const std::string& fsimple_table_file_name, double emax_repl,
                   int id, double fminimal_threshold = 0.0);
  /// Destructor.
  virtual ~ExAtomPhotoAbsCS() {}

 protected:
  std::string threshold_file_name;
  std::string simple_table_file_name;
  std::string BT_file_name;
  // the name acs is misleading:
  // actually here there is ionization cross section.
  // Excitations are added separately as height_of_excitation.
  // So it should be more logical to call this ics.
  // Initially ics was meant to be separate array.
  // But during development it was found that it is not necessary.
  std::vector<ActivePtr<PhotoAbsCS> > acs;

  // 3 variables for printing listings
  double integ_abs_before_corr;
  double integ_abs_after_corr;
  double integ_ioniz_after_corr;
  double height_of_excitation;  // assumed  in the lowest shell
  double exener[2];             // boundaries of excitation
  double minimal_threshold;     // make shifts if necessary
  // The shells are corrected on the minimal_threshold "on the fly".
  // It the threshold of the atomic shell is less then minimal_threshold,
  // the difference is subtracted from the energy for which
  // the cross section is requested.
  // exener[2] is corrected at initialization in first main constructor.
  // In the second one there is no implementation of minimal_threshold so far.

  /// Flag whether to add excitations.
  /// If 0 excitations will not be added (useful for debugging and for checking
  /// the effect produced by adding excitations). For real work, this variable
  /// should always be set to 1.
  static const int s_add_excitations_to_normalize = 1;
  static const int s_scale_to_normalize_if_more = 1;
};

//---------------------------------------------------------

const double standard_factor_Fano = 0.19;

#define CALC_W_USING_CHARGES
// the opposite is just averaging potentials
// This way is averaging potentials with taking into account charges.
// So the atom or molecula with larger charge will affect more -
// this looks much more reasonable.
// The opposite case is preserved for debug and research purposes.
// F is calculated by the same way as W

const double coef_I_to_W = 2.0;

/// Molecular photoabsorption cross-section.
/// Molecules refer to atoms by passive pointers.
/// If atom is changed, its image for molecule is also changed.

class MolecPhotoAbsCS : public RegPassivePtr {
 public:
  /// Total number of atoms of all sorts in the molecule.
  int get_qatom(void) { return qatom; }
  /// Number of atoms of a particular sort in the molecule.
  int get_gatom_ps(int n) { return qatom_ps[n]; }
  const PassivePtr<const AtomPhotoAbsCS> get_atom(int n) { return atom[n]; }

  /// Photo-absorption cross-section (energy in MeV, CS in Mb).
  virtual double get_ACS(double energy) const;
  /// Integral photo-absorption cross-section.
  virtual double get_integral_ACS(double energy1, double energy2) const;
  /// Photo-ionization cross-section (energy in MeV, CS in Mb).
  virtual double get_ICS(double energy) const;
  /// Integral photo-ionization cross-section.
  virtual double get_integral_ICS(double energy1, double energy2) const;

  int get_total_Z() const;
  /// Retrieve W value [MeV].
  double get_W(void) const { return W; }
  /// Retrieve Fano factor.
  double get_F(void) const { return F; }

  /// Default constructor.
  MolecPhotoAbsCS() : qatom(0) {}
  /// Constructor for one sort of atoms.
  /// If fW == 0.0, the program assigns it as 2 * mean(I_min).
  MolecPhotoAbsCS(const AtomPhotoAbsCS& fatom, int fqatom, double fW = 0.0,
                  double fF = standard_factor_Fano);
  /// Constructor for two sorts of atoms.
  /// If fW == 0.0, the program assigns it as 2 * mean(I_min).
  MolecPhotoAbsCS(const AtomPhotoAbsCS& fatom1, int fqatom_ps1,
                  const AtomPhotoAbsCS& fatom2, int fqatom_ps2, double fW = 0.0,
                  double fF = standard_factor_Fano);
  /// Constructor for three sorts of atoms.
  /// If fW == 0.0, the program assigns it as 2 * mean(I_min).
  MolecPhotoAbsCS(const AtomPhotoAbsCS& fatom1, int fqatom_ps1,
                  const AtomPhotoAbsCS& fatom2, int fqatom_ps2,
                  const AtomPhotoAbsCS& fatom3, int fqatom_ps3, double fW = 0.0,
                  double fF = standard_factor_Fano);
  /// Destructor
  virtual ~MolecPhotoAbsCS() {}
  virtual void print(std::ostream& file, int l) const;

 private:
  /// Total number of atoms, NOT number of sorts, NOT qel in atom.
  int qatom;
  std::vector<int> qatom_ps;
  std::vector<PassivePtr<const AtomPhotoAbsCS> > atom;
  /// Mean work per pair production [MeV].
  double W;
  /// Fano factor.
  double F;
};
std::ostream& operator<<(std::ostream& file, const MolecPhotoAbsCS& f);
}

#endif

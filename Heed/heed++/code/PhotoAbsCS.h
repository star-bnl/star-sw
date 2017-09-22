#ifndef PHOTOABSCS_H
#define PHOTOABSCS_H

#include <vector>
#include <fstream>
#include <cmath>
#include <cfloat>
#include <climits>
#include "wcpplib/safetl/AbsPtr.h"
#include "heed++/code/PhysicalConstants.h"

namespace Heed {

/// TRK sum rule [1/MeV], constant per one electron.
const double Thomas_sum_rule_const = 2 * M_PI * M_PI / (FSCON * ELMAS);
/// TRK sum rule [Mb * MeV].
const double Thomas_sum_rule_const_Mb =
    2 * M_PI * M_PI / (FSCON * ELMAS) * 1.0E-6 / C1_MEV2_BN;

/// Determine whether to use linear or nonlinear interpolation.
/// Usually photoabsorption cross sections decrease as inverse power function
/// of power like -2.75. Its linear interpolation inside large energy
/// intervals is remarkably not precise. This function is designed
/// to determine the cases common for the total program when the inverse power
/// function is applied in such intervals.
/// Conditions are empirical.
/// 1 - nonlinear, 0 - linear.
/// Energies and threshold are given in MeV. This is important!
/// e1 should be < e2.
int sign_nonlinear_interpolation(double e1, double cs1, double e2, double cs2,
                                 double threshold);

/// Fit table by a straight line or by inverse power function
/// (see sign_nonlinear_interpolation) and integrate the area below it.
/// The function is assumed to be non-negative, and the result of integration
/// as well.
/// The tail is not added right here, but added after the call of this function.
/// The theshold is used to restrict this function from the left.
/// If threshold is less than e[0], the function is extrapolated
/// by the straight line till threshold.
/// If this line crosses zero, it is extrapolated only till this point.
double glin_integ_ar(std::vector<double> e, std::vector<double> cs, long q,
                     double e1, double e2, double threshold);

double my_integr_fun(double xp1, double yp1, double xp2, double yp2,
                     double xmin, double xmax, double x1, double x2);
double my_val_fun(double xp1, double yp1, double xp2, double yp2, double xmin,
                  double xmax, double x);

// double old_glin_integ_ar(DynLinArr< double > e, DynLinArr< double > cs,
//                     long q, double e1, double e2,
//                     double threshold );

// double glin_val_ar(DynLinArr< double > e, DynLinArr< double > cs,
//                   long q, double x
//                   double threshold );

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
 
  const std::string& get_name() const { return name; }
  /// Charge or number of electrons at this shell or in this atom
  /// (in principle the integral of CS should
  /// satisfy the Thomas-Reiche-Kuhn sum rule).
  int get_Z() const { return Z; }
  double get_threshold() const { return threshold; }
  /// Retrieve cross-section [Mb] at a given energy [MeV].
  virtual double get_CS(double energy) const = 0;
  /// Retrieve integral cross-section [Mb * MeV] in a given interval [MeV].
  virtual double get_integral_CS(double energy1, double energy2) const = 0;
  /// Multiply by some factor (sometimes useful for debug and other purposes).
  virtual void scale(double fact) = 0;

  virtual void print(std::ostream& file, int l) const;
  virtual PhotoAbsCS* copy() const = 0;

 protected:
  std::string name;
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
  /// Default constructor
  AveragePhotoAbsCS() {}
  /** Constructor
    * \param apacs 
             photoabsorption cross-section
    * \param fwidth 
             width [MeV] for smoothing
    * \param fstep 
             step size [MeV] for numerical integration
    * \param fmax_q_step
             max number of integration steps
    */
  AveragePhotoAbsCS(PhotoAbsCS* apacs, double fwidth,
                    double fstep, long fmax_q_step);
  /// Destructor
  virtual ~AveragePhotoAbsCS() {}
  virtual double get_CS(double energy) const;
  virtual double get_integral_CS(double energy1, double energy2) const;

  /// Multiply the height, y-axis, by some factor.
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
  SimpleTablePhotoAbsCS();
  SimpleTablePhotoAbsCS(const std::string& fname, int fZ, double fthreshold,
                        const std::string& ffile_name);
  SimpleTablePhotoAbsCS(const std::string& fname, int fZ, double fthreshold,
                        const std::vector<double>& fener,
                        const std::vector<double>& fcs);
  // Fit from one of the papers of Band-Trzaskovskaya et al, CS for any shell
  // It is difficult to integrate that formulas analytically.
  // So I create numerical array and treat it as input data.
  SimpleTablePhotoAbsCS(const std::string& fname, int fZ, double fthreshold,
                        int l, double E0, double yw, double ya, double P,
                        double sigma);
  // Replace part:
  SimpleTablePhotoAbsCS(const SimpleTablePhotoAbsCS& total,
                        const SimpleTablePhotoAbsCS& part, double emax_repl);
  /// Destructor
  virtual ~SimpleTablePhotoAbsCS() {}
  void remove_leading_zeros(void);         // very useful operation
                                           // for preparation of some tables
  void remove_leading_tiny(double level);  // very useful operation
                                           // for preparation of some tables,
  // removes points with cross section below determined level.
  // It is designed for Henke tables, which somewhy are prepared for database
  // with leading values like 1.0e-15 .
  // Both functions allow to use the straight interpolation to threshold

  // void remove_leading_zeros(double minimal_theshold); //very useful operation
  // removes all points which are less or equal to minimal_theshold
  // the case of equal needs for argon

  virtual double get_CS(double energy) const;
  virtual double get_integral_CS(double energy1, double energy2) const;
  inline const std::vector<double>& get_arr_ener() const { return ener; }
  inline const std::vector<double>& get_arr_CS() const { return cs; }
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

/// Simple phenomenological CS for any shell.
class PhenoPhotoAbsCS : public PhotoAbsCS {
 public:
  PhenoPhotoAbsCS();
  PhenoPhotoAbsCS(const std::string& fname, int fZ, double fthreshold,
                  double fpower = 2.75);
  virtual ~PhenoPhotoAbsCS() {}
  // power is here positive, but it is meay that there is division,
  // the the actual power is negative.
  virtual double get_CS(double energy) const;
  virtual double get_integral_CS(double energy1, double energy2) const;
  virtual void scale(double fact);
  virtual void print(std::ostream& file, int l) const;
  virtual PhenoPhotoAbsCS* copy() const { return new PhenoPhotoAbsCS(*this); }

 private:
  double power;  // positive power 1/E^power
  double factor;
};

/*
// See above
// Fit from one of the papers of Band-Trzaskovskaya et al, CS for any shell
class FitBTPhotoAbsCS: public PhotoAbsCS
{
public:
  FitBTPhotoAbsCS(void);
  FitBTPhotoAbsCS(const std::string& fname, int fZ, double fthreshold,
                  int  lPas, double E0, double yw, double ya,
                  double P, double sigma);

  // power is here positive, but it is meay that there is division,
  // the the actual power is negative.
  virtual double get_CS(double energy) const ;
  virtual double get_integral_CS(double energy1, double energy2) const ;
  virtual void scale(double fact);
  virtual void print(std::ostream& file, int l) const ;
  virtual FitBTPhotoAbsCS* copy() const { return new FitBTPhotoAbsCS(*this); }

private:
  int  l;
  double E0;
  double yw;
  double ya;
  double P;
  double sigma;
  static EnergyMesh mesh_for_FitBT;
  std::vector<double> cs;
};
*/
//------------------------------------------------------------------------

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
  AtomPhotoAbsCS();
 
  inline int get_Z() const { return Z; }
  inline unsigned int get_qshell() const { return qshell; }
  virtual double get_threshold(int nshell) const = 0;
  virtual double get_I_min(void) const;
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

  AtomicSecondaryProducts* get_asp(int nshell);  // needs in order
  // to allow modification (such as addition of new channels)
  // after the main constructor is executed (but, of course, before the
  // regular event generation).
 protected:
  std::string name;
  int Z;
  int qshell;
  // 0 - sign to use shell. 1 - sign to ignore it
  // It does not affect threshold and escape sequences and assumed to
  // manipulate with larger  shells, to investigate their
  // influence at the final characteristics.
  // By default all is 0
  std::vector<int> s_ignore_shell;
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
  virtual int get_main_shell_number(int nshell) const;

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
  virtual int get_main_shell_number(int nshell) const;
  void replace_shells_by_average(double fwidth,  // MeV
                                 double fstep, long fmax_q_step);
  virtual void print(std::ostream& file, int l) const;
  virtual ExAtomPhotoAbsCS* copy() const { 
    return new ExAtomPhotoAbsCS(*this); 
  }

  /// Default constructor.
  ExAtomPhotoAbsCS(void) : AtomPhotoAbsCS() {}
  /** Constructor, 
    * \param fthreshold_file_name 
             file from which to read name and shell energies
    * \param fsimple_table_file_name
             file from which to read the cross-sections 
    * \param fname 
             name of the atom, if "none" it is taken from fthreshold_file_name
    * \param fminimal 
             threshold (normally it is used only with other threshold) 
    */
  ExAtomPhotoAbsCS(int fZ, const std::string& fthreshold_file_name,
                   const std::string& fsimple_table_file_name,
                   const std::string& fname = "none", 
                   double fminimal_threshold = 0.0);
  /** Constructor, shells from Band and Band and Thragzkovskaya.
    * \param fname
             name of the atom
      \param fBT_file_name
             file with shell names and energies 
      \param id
             1 - old files without fluorescence rate
             2 - new files with fluorescence rate
             other values - error
    */
  ExAtomPhotoAbsCS(int fZ, const std::string& fname,
                   const std::string& fBT_file_name,
                   int id, double fminimal_threshold = 0.0);
  // Takes the  shell names and shell energies from file generated by
  // Band and Thragzkovskaya.
  // Old comm: Currently no fluorescence.
  // Today (22.04.2005) flyorescence is included in this constructor.
  ExAtomPhotoAbsCS(int fZ, const std::string& fname,  // just name of this atom
                   const std::string& fFitBT_file_name,
                   int id,  // to distinguish it from constructor above
                   // 1 - old files without fluorescence rate
                   // 2 - new files with fluorescence rate
                   // other values - error
                   int s_no_scale,  // scaling is not done, needs for next
                   double fminimal_threshold = 0.0);
  // Takes the shell energies and fit parameters made by
  // Band and Thragzkovskaya
  // from a file.
  ExAtomPhotoAbsCS(int fZ, const std::string& fname,  // just name of this atom
                   const std::string& fFitBT_file_name,
                   const std::string& fsimple_table_file_name, double emax_repl,
                   int id,  // to distinguish it from constructor above
                   // and fluorescense (2) or not(1)
                   double fminimal_threshold = 0.0);
  // The combination of BT- fit and simple-table(Henke) tables,
  // made by the same way as in the old fortran HEED.
  // It initializes BT- fit and replaces the part of the first shell
  // from threshold taken from BT- fit to emax_repl
  // by values from  the simple table.
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
  /// Total quantity of atoms of all sorts in the molecule.
  inline int get_qatom(void) { return qatom; }  
  /// Quantity of atoms of a particular sort in the molecule.
  inline int get_gatom_ps(int n) { return qatom_ps[n]; }  
  inline const PassivePtr<const AtomPhotoAbsCS> get_atom(int n) {
    return atom[n];
  }
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
  MolecPhotoAbsCS(void) : qatom(0) { ; }
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
  /// Total quantity of atoms, NOT number of sorts, NOT qel in atom.
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

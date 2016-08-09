#ifndef PHOTOABSCS_H
#define PHOTOABSCS_H
#include <fstream>
#include <cmath>
#include <cfloat>
#include <climits>
#include "wcpplib/util/String.h"
#include "wcpplib/safetl/AbsArr.h"
#include "wcpplib/safetl/AbsPtr.h"
#include "wcpplib/math/minmax.h"
#include "heed++/code/PhysicalConstants.h"
/*
The literature data on photoabsorption cross section are fragmentar and
not always consistent. This class hierarchy is designed to
gather them in a consistent library.
The principle is ordinary: definition of an abstract class
which defines the interface available for the rest of program,
and definition of derived classes with this or that realization.
To the contrary with wcpplib/matter, there is no any global "database"
and no formal ban to duplicate these definitions (also there would not be sense
in duplication). So these are simple classes determining photoabsorption
cross sections for atomic shells, for atoms, and for moleculas.
Aso the atomic relaxation cascades are defined.
The system requires some memory for keeping data, and some disk files
with input information. It takes some time for initializations, so
it is not intended to be used in a loops, but only for initial or interactive
initializations.
Interesting that in the fortran version of HEED, the equivalent data structure
kept in a common block was depending on energy mesh.
But in this C++ version it was found possible to avoid this dependence.
The data are kept and handled as is, without unnecessary conversions.
This improves precision of handling energies near ionization threshoulds.
In total this system is not trivial, and it needs to spend a lot of time
for more detailed descriptions.

2004, I. Smirnov

*/

namespace Heed {

const double Thomas_sum_rule_const =
    2.0 * M_PI * M_PI / (FSCON * ELMAS);  // [1/MeV]
                                          // constant per one electron.
const double Thomas_sum_rule_const_Mb =
    2.0 * M_PI * M_PI / (FSCON * ELMAS) * 1.0E-6 / C1_MEV2_BN;  // [Mb*MeV]

// Usually photo-absorption cross section decreases as inverse power function
// of power like -2.75. Its linear interpolation inside large energy
// intervals is remarkably not precise. This function (below) is designed
// to determine the cases common for the total program when the inverse power
// function is applied in such intervals.
// Conditions are empirical.
// 1 - nonlinear, 0 - linear.
// Energies and threshold are given in MeV. This is important!
// e1 should be < e2.
int sign_nonlinear_interpolation(double e1, double cs1, double e2, double cs2,
                                 double threshold);

double glin_integ_ar(DynLinArr<double> e, DynLinArr<double> cs, long q,
                     double e1, double e2, double threshold);
// fit table by a straight line or by inverse power function
// (see comment above)
// and integrate the area below it.
// The function is assumed to be non-negative, and the result of integration
// as well.
// The tail is not added right here, but added after the call of this function.
// The theshold is used to restrict this function from the left.
// If threshold is less than e[0], the function is extrapolated
// by the straight line till threshold.
// If this line crosses zero, it is extrapolated only till this point.

double my_integr_fun(double xp1, double yp1, double xp2, double yp2,
                     double xmin, double xmax, double x1, double x2);
double my_val_fun(double xp1, double yp1, double xp2, double yp2, double xmin,
                  double xmax, double x);

//double old_glin_integ_ar(DynLinArr< double > e, DynLinArr< double > cs,
//                     long q, double e1, double e2,
//                     double threshold );

//double glin_val_ar(DynLinArr< double > e, DynLinArr< double > cs,
//                   long q, double x
//                   double threshold );

class PhotoAbsCS virt_common_base_col {
 public:
  inline const String& get_name() const { return name; }
  // charge or number of electrons at this shell or in this atom
  // (in principle the integral of CS should
  // satisfy the Thomas-Reiche-Kuhn sum rule)
  inline int get_Z() const { return Z; }
  inline double get_threshold() const { return threshold; }
  virtual double get_CS(
      double energy) const = 0;  // energy in MeV, CS in Mbarns
  virtual double get_integral_CS(double energy1, double energy2) const = 0;
  // energy in MeV, CS in Mbarns * MeV
  // multiply by some factor
  // it is sometimes useful for debug and other purposes.
  virtual void scale(double fact) = 0;

  virtual void print(std::ostream& file, int l) const;
  macro_copy_total_zero(PhotoAbsCS);
  PhotoAbsCS(void);
  PhotoAbsCS(const String& fname, int fZ, double fthreshold);
  virtual ~PhotoAbsCS() {}

 protected:
  String name;
  int Z;
  double threshold;  // in MeV
};

class OveragePhotoAbsCS : public PhotoAbsCS {
  ActivePtr<PhotoAbsCS> real_pacs;
  double width;
  // parameters for get_integral_CS
  // (it can be done only approximate by numerical integration)
  long max_q_step;  // if real q is more, the
                    // function calls "precise" get_integral_CS from PhotoAbsCS,
                    // so it will be without smoothing.
  double step;      // the step of integration, for example, 1/20 from width
 public:
  // constructors
  OveragePhotoAbsCS() { ; }
  OveragePhotoAbsCS(PhotoAbsCS* apacs, double fwidth,  // MeV
                    double fstep, long fmax_q_step);
  // destructor
  virtual ~OveragePhotoAbsCS() {}
  virtual double get_CS(double energy) const;  // energy in MeV, CS in Mbarns
  virtual double get_integral_CS(double energy1, double energy2) const;
  // energy in MeV, CS in Mbarns * MeV

  // scale (multiply the height, y-axis, by some factor)
  // it is sometimes useful for debug and other purposes.
  virtual void scale(double fact);

  virtual void print(std::ostream& file, int l) const;
  macro_copy_total(OveragePhotoAbsCS);
};

// Hydrogen: empirical fit of Kosarev & Podoliak
// Original formula for molecular hydrogen
// Since this class for separated shell, we divide molecular CS by 2
class HydrogenPhotoAbsCS : public PhotoAbsCS {
 public:
  HydrogenPhotoAbsCS();
  virtual ~HydrogenPhotoAbsCS() {}
  virtual double get_CS(double energy) const;
  virtual double get_integral_CS(double energy1, double energy2) const;
  virtual void scale(double fact);
  macro_copy_total(HydrogenPhotoAbsCS);
  virtual void print(std::ostream& file, int l) const;

 private:
  double prefactor;
};

// Typically this is for reading Experimental CS, for example of argon,
// as if there is one shell.
// File is two-column table, the first row is energy in eV,
// the second one is CS in Mbarn.
// The points are understood as local points.
// The interpolation between them are either straight or by power
// function.
// the choice is determined by function sign_nonlinear_interpolation
// (see above).
// If the first point is not zero cross section,
// and the threshold is to the left,
// the straight extrapolation is performed from the two edge points.
// The extrapolation to the right is performed to the end of
// the energy mesh by power function with power -2.75.
// The minimal number of points is 2, as in PointCoorMesh from tline.h
// The zero number of points is allowed as well.
// Then the cross section is assumed to be zero.

class SimpleTablePhotoAbsCS : public PhotoAbsCS {
 public:
  SimpleTablePhotoAbsCS();
  SimpleTablePhotoAbsCS(const String& fname, int fZ, double fthreshold,
                        const String& ffile_name);
  SimpleTablePhotoAbsCS(const String& fname, int fZ, double fthreshold,
                        const DynLinArr<double>& fener,
                        const DynLinArr<double>& fcs);
  // Fit from one of the papers of Band-Trzaskovskaya et al, CS for any shell
  // It is difficult to integrate that formulas analytically.
  // So I create numerical array and treat it as input data.
  SimpleTablePhotoAbsCS(const String& fname, int fZ, double fthreshold, int l,
                        double E0, double yw, double ya, double P,
                        double sigma);
  // Replace part:
  SimpleTablePhotoAbsCS(const SimpleTablePhotoAbsCS& total,
                        const SimpleTablePhotoAbsCS& part, double emax_repl);
  virtual ~SimpleTablePhotoAbsCS() {}
  void remove_leading_zeros(void);         // very useful operation
                                           // for preparation of some tables
  void remove_leading_tiny(double level);  // very useful operation
                                           // for preparation of some tables,
  // removes points with cross section below determined level.
  // It is designed for Henke tables, which somewhy are prepared for database
  // with leading values like 1.0e-15 .
  // Both functions allow to use the straight interpolation to threshold

  //void remove_leading_zeros(double minimal_theshold); //very useful operation
  // removes all points which are less or equal to minimal_theshold
  // the case of equal needs for argon

  virtual double get_CS(double energy) const;  // energy in MeV
  virtual double get_integral_CS(double energy1, double energy2) const;
  inline const DynLinArr<double>& get_arr_ener() const { return ener; }
  inline const DynLinArr<double>& get_arr_CS() const { return cs; }
  virtual void scale(double fact);  // just miltiply the table
  virtual void print(std::ostream& file, int l) const;
  macro_copy_total(SimpleTablePhotoAbsCS);

 private:
  String file_name;  // saved for printing
                     // The following arrays are interpreted as
                     // the value of cross section at this value of energy.
  DynLinArr<double> ener;  // MeV
  DynLinArr<double> cs;
};

// Simple phenomenological CS for any shell
class PhenoPhotoAbsCS : public PhotoAbsCS {
 public:
  PhenoPhotoAbsCS(void);
  PhenoPhotoAbsCS(const String& fname, int fZ, double fthreshold,
                  double fpower = 2.75);
  virtual ~PhenoPhotoAbsCS() {}
  // power is here positive, but it is meay that there is division,
  // the the actual power is negative.
  virtual double get_CS(double energy) const;
  virtual double get_integral_CS(double energy1, double energy2) const;
  virtual void scale(double fact);
  virtual void print(std::ostream& file, int l) const;
  macro_copy_total(PhenoPhotoAbsCS);

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
  FitBTPhotoAbsCS(const String& fname, int fZ, double fthreshold,
                  int  lPas, double E0, double yw, double ya,
                  double P, double sigma);

  // power is here positive, but it is meay that there is division,
  // the the actual power is negative.
  virtual double get_CS(double energy) const ;
  virtual double get_integral_CS(double energy1, double energy2) const ;
  virtual void scale(double fact);
  virtual void print(std::ostream& file, int l) const ;
  macro_copy_total(FitBTPhotoAbsCS);
  //AnyType_copy(PhenoPhotoAbsCS, PhotoAbsCS)
private:
  int  l;
  double E0;
  double yw;
  double ya;
  double P;
  double sigma;
  static EnergyMesh mesh_for_FitBT;
  DynLinArr< double > cs;
};
*/
//------------------------------------------------------------------------

/* The following class keeps only secondary particles and not include
photo-electron kicked off from the orbit.
The photo-electron is completely independent on the secondary channel
and it always bears an energy equal to transferred energy minus shell energy.
This class proposes particles which can be emitted at filling this shell.
This class can be interpreted as an additional channels for
the default one.
The sum of channel_prob_dens may be less than unity.
The concrete channel for the particular event is sampled by get_channel().
If the random sampling does not point to one of the channels registering
in this class, get_channel returns 0, which must be interpreted as
the use of standard channel.
*/

class AtomicSecondaryProducts : public RegPassivePtr {
 public:
  int get_channel(DynLinArr<double>& felectron_energy,       // MeV
                  DynLinArr<double>& fphoton_energy) const;  // MeV
  // return value - sign that channel is generated (1) or not (0).

  AtomicSecondaryProducts()
      : channel_prob_dens(), electron_energy(), photon_energy() {
    ;
  }
  virtual ~AtomicSecondaryProducts() {}
  void add_channel(double fchannel_prob_dens,
                   const DynLinArr<double>& felectron_energy,  // MeV
                   const DynLinArr<double>& fphoton_energy,    // MeV
                   int s_all_rest = 0);  // if 1 , the probability of this
  // channel is assigned to that what is left to 1.
  // fchannel_prob_dens is then ignored, it can be just 0.
  // This function adds new decay channel. Should be used at initialization.

  //AtomicSecondaryProducts(DynLinArr< double > fchannel_prob_dens;
  //                          DynLinArr< DynLinArr< double > > felectron_energy;
  //                          DynLinArr< DynLinArr< double > > fphoton_energy;
  virtual void print(std::ostream& file, int l) const;

 protected:
  //long q_channel;
  DynLinArr<double> channel_prob_dens;  // probability of specific channel.
  // Arrays of decay products for each channel:
  DynLinArr<DynLinArr<double> > electron_energy;  // MeV
  DynLinArr<DynLinArr<double> > photon_energy;    // MeV
};

class AtomPhotoAbsCS : public RegPassivePtr {
 public:
  inline int get_Z() const { return Z; }
  inline int get_qshell() const { return qshell; }
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
                                    DynLinArr<double>& el_energy,
                                    DynLinArr<double>& ph_energy) const;

  virtual int get_main_shell_number(int nshell) const = 0;
  // returns the shell number (1,2,...)
  // The current versions read it from shell name,
  // so they interprete the shell name as the number.
  // If the shell number cannot be recovered, the function
  // returns -1.

  virtual void remove_shell(int nshell);
  virtual void restore_shell(int nshell);
  virtual void print(std::ostream& file, int l) const;
  macro_copy_total_zero(AtomPhotoAbsCS);
  //virtual AtomPhotoAbsCS* copy(void) const = 0;
  AtomPhotoAbsCS(void);
  AtomicSecondaryProducts* get_asp(int nshell);  // needs in order
  // to allow modification (such as addition of new channels)
  // after the main constructor is executed (but, of course, before the
  // regular event generation).
 protected:
  String name;
  int Z;
  int qshell;
  DynLinArr<int> s_ignore_shell;  // 0 - sign to use shell
                                  // 1 - sign to ignore it
  // It does not affect threshold and escape sequences and assumed to
  // manipulate with larger  shells, to investigate their
  // influence at the final characteristics.
  // By default all is 0
  DynLinArr<AtomicSecondaryProducts> asp;
};
std::ostream& operator<<(std::ostream& file, const AtomPhotoAbsCS& f);

/*Simple means that there is no difference between absorption and ionization.
So there is one single internal array for both.
 */
class SimpleAtomPhotoAbsCS : public AtomPhotoAbsCS {
 public:
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
  macro_copy_total(SimpleAtomPhotoAbsCS);
  SimpleAtomPhotoAbsCS(void);
  SimpleAtomPhotoAbsCS(int fZ, const String& ffile_name);
  // Takes the  name and shell energies from file,
  // genetares the CS by PhenoPhotoAbsCS
  SimpleAtomPhotoAbsCS(int fZ, const PhotoAbsCS& fasc);
  // The simplest thing: one prepared preliminary shell with Z electrons
  // Convenient for Hydrogen
  virtual ~SimpleAtomPhotoAbsCS() {}

 protected:
  String file_name;  // saved for printing
  DynLinArr<ActivePtr<PhotoAbsCS> > acs;
};

const int s_add_excitations_to_normalize = 1;
// if 0 - in the following class the excitations will not be added.
// It is useful for debug and for checking the effect produced by
// adding excitations.
// But, if it is 0 - the Rezerford gets less. This is not correct.
// So for real work this variable should always be equal to 1.

const int s_scale_to_normalize_if_more = 1;

const double low_boundary_of_excitations = 0.7;  // from ionization threshold

// With exitation:
class ExAtomPhotoAbsCS : public AtomPhotoAbsCS {
 public:
  // photo-absorption cross section, energy in MeV, CS in Mbarns.
  virtual double get_threshold(int nshell) const;
  virtual double get_ACS(double energy) const;
  virtual double get_integral_ACS(double energy1, double energy2) const;
  virtual double get_ACS(int nshell, double energy) const;
  virtual double get_integral_ACS(int nshell, double energy1,
                                  double energy2) const;

  // photo-ionization cross section, energy in MeV, CS in Mbarns.
  virtual double get_ICS(double energy) const;
  virtual double get_integral_ICS(double energy1, double energy2) const;
  virtual double get_ICS(int nshell, double energy) const;
  virtual double get_integral_ICS(int nshell, double energy1,
                                  double energy2) const;
  virtual int get_main_shell_number(int nshell) const;
  void replace_shells_by_overage(double fwidth,  // MeV
                                 double fstep, long fmax_q_step);
  virtual void print(std::ostream& file, int l) const;
  macro_copy_total(ExAtomPhotoAbsCS);
  ExAtomPhotoAbsCS(void) : AtomPhotoAbsCS() {}
  ExAtomPhotoAbsCS(int fZ, const String& fthreshold_file_name,
                   const String& fsimple_table_file_name,
                   const String& fname = "none",  // The name of atom
                   // ^ if "none", it is taken from fthreshold_file_name
                   // Normally it is used only with other threshold
                   double fminimal_threshold = 0.0);
  // Takes the  name (see remark above)
  // and shell energies from file fthreshold_file_name ,
  // takes cross section from file fsimple_table_file_name
  ExAtomPhotoAbsCS(int fZ, const String& fname,  // just name of this atom
                   const String& fBT_file_name,
                   int id,  // to distinguish it from constructor above
                   // No, now it will be this way
                   // 1 - old files without fluorescence rate
                   // 2 - new files with fluorescence rate
                   // other values - error
                   double fminimal_threshold = 0.0);
  // Takes the  shell names and shell energies from file generated by
  // Band and Thragzkovskaya.
  // Old comm: Currently no fluorescence.
  // Today (22.04.2005) flyorescence is included in this constructor.
  ExAtomPhotoAbsCS(int fZ, const String& fname,  // just name of this atom
                   const String& fFitBT_file_name,
                   int id,          // to distinguish it from constructor above
                   // 1 - old files without fluorescence rate
                   // 2 - new files with fluorescence rate
                   // other values - error
                   int s_no_scale,  // scaling is not done, needs for next
                   double fminimal_threshold = 0.0);
  // Takes the shell energies and fit parameters made by
  // Band and Thragzkovskaya
  // from a file.
  ExAtomPhotoAbsCS(int fZ, const String& fname,  // just name of this atom
                   const String& fFitBT_file_name,
                   //const String& fthreshold_file_name,  this can be avoided
                   const String& fsimple_table_file_name, double emax_repl,
                   int id,  // to distinguish it from constructor above
                   // and fluorescense (2) or not(1)
                   double fminimal_threshold = 0.0);
  // The combination of BT- fit and simple-table(Henke) tables,
  // made by the same way as in the old fortran HEED.
  // It initializes BT- fit and replaces the part of the first shell
  // from threshold taken from BT- fit to emax_repl
  // by values from  the simple table.
  virtual ~ExAtomPhotoAbsCS() {}

 protected:
  String threshold_file_name;  // saved for printing
  String simple_table_file_name;
  String BT_file_name;
  DynLinArr<ActivePtr<PhotoAbsCS> > acs;  // the name acs is misleading:
  // actually here there is ionization cross section.
  // Excitations are added separately as height_of_excitation.
  // So it should be more logical to call this ics.
  // Initially ics was meant to be separate array.
  // But during development it was found that it is not necessary.

  // 3 variables for printing listings
  double integ_abs_before_corr;
  double integ_abs_after_corr;
  double integ_ioniz_after_corr;
  double height_of_excitation;  // assumed  in the lowest shell
  double exener[2];             // boundaries of excitation
                                //DynLinArr< ActivePtr< PhotoAbsCS > > ics;
  double minimal_threshold;     // make shifts if necessary
  // The shells are corrected on the minimal_threshold "on the fly".
  // It the threshold of the atomic shell is less then minimal_threshold,
  // the difference is subtracted from the energy for which
  // the cross section is requested.
  // exener[2] is corrected at initialization in first main constructor.
  // In the second one there is no implementation of minimal_threshold so far.
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

/*
Moleculas refer to atoms by passive pointers.
If atom is changed, its image for molecula is also changed.
*/

class MolecPhotoAbsCS : public RegPassivePtr {
 public:
  inline int get_qatom(void) {
    return qatom;
  }  // total quantity of atoms
     // of all sorts in molecula
  inline int get_gatom_ps(int n) {
    return qatom_ps[n];
  }  // quantity of atom
     // of particular sort in molecula
  inline const PassivePtr<const AtomPhotoAbsCS> get_atom(int n) {
    return atom[n];
  }
  virtual double get_ACS(double energy) const;
  virtual double get_integral_ACS(double energy1, double energy2) const;
  // photo-absorption cross section, energy in MeV, CS in Mbarns.
  virtual double get_ICS(double energy) const;
  virtual double get_integral_ICS(double energy1, double energy2) const;
  // photo-ionization cross section, energy in MeV, CS in Mbarns.

  int get_total_Z() const;
  double get_W(void) const {
    return W;
  }  // MeV
  double get_F(void) const { return F; }

  MolecPhotoAbsCS(void) : qatom(0) { ; }

  // In the following, if fW == 0.0, the program assigns it as 2 * mean(I_min),
  // This is in general correct
  // one sort of atoms
  MolecPhotoAbsCS(const AtomPhotoAbsCS& fatom, int fqatom, double fW = 0.0,
                  double fF = standard_factor_Fano);
  // two sorts of atoms
  MolecPhotoAbsCS(const AtomPhotoAbsCS& fatom1, int fqatom_ps1,
                  const AtomPhotoAbsCS& fatom2, int fqatom_ps2, double fW = 0.0,
                  double fF = standard_factor_Fano);
  // 3 sorts of atoms
  MolecPhotoAbsCS(const AtomPhotoAbsCS& fatom1, int fqatom_ps1,
                  const AtomPhotoAbsCS& fatom2, int fqatom_ps2,
                  const AtomPhotoAbsCS& fatom3, int fqatom_ps3, double fW = 0.0,
                  double fF = standard_factor_Fano);
  virtual ~MolecPhotoAbsCS() {}
  virtual void print(std::ostream& file, int l) const;

 private:
  int qatom;  // total quantity of atoms, NOT number of sorts, NOT qel in atom
  DynLinArr<int> qatom_ps;
  DynLinArr<PassivePtr<const AtomPhotoAbsCS> > atom;
  double W;  // The mean work per pair production,  MeV.
  double F;  // Fano parameter.
};
std::ostream& operator<<(std::ostream& file, const MolecPhotoAbsCS& f);

/*

class PhotoAbsorptionCS
{public:
  int z;  // charge (in principle the integral of CS should
          // satisfy the Thomas-Reiche-Kuhn sum rule).
  PassivePtr< EnergyMesh > energy_mesh;
  DinLinArr< double > total_cs;
  int qsh;                     // number of shells
  DinArr< double > shell_cs;   // first dimension - shell number
                               // second dimension - energy
  DinLinArr< double > shell_energy;  // dimension - shell number
  DinLinArr< int > z_shell; // number_of_electrons in shell
  DinLinArr< double > fluorescence_yield;

  PhotoAbsorptionCS(void);

  PhotoAbsorptionCS(const String& ffile_name,
                    PassivePtr< EnergyMesh > fenergy_mesh);
                                          // mesh will be created

  PhotoAbsorptionCS(const PhotoAbsorptionCS& pacs ,
                    PassivePtr< EnergyMesh > fenergy_mesh);
  // convert to another existing mesh
};
*/

}

#endif

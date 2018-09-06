#ifndef ATOM_DEF_H
#define ATOM_DEF_H

#include <iostream>
#include <vector>
#include <list>

namespace Heed {

/// Definition of atoms.
/// Only the basic information: name, notation, atomic weight and charge.
///
/// The principle of definitions of atoms is dictionary or a database:
/// the atoms are not repeated,
/// each atom is presented in the total system no more than one time.
/// The system knows each atom presented in it.
/// The atom characteristics can be obtained by literal notation.
/// The system declines the secondary initialization.
/// The copying is not declined.
/// When the user program wants to refer to atom,
/// it has to use either char* (string) notation, or pointer (or reference)
/// to one of these objects.
/// The user pogram can initialize the new atoms.
/// The standard atoms are initiated in files GasLib.h and GasLib.c.
///
/// In principle I am going to initiate all atoms from Mendeleev's table,
/// but I haven't finished yet. Only its first half is filled at the moment.
///
/// The atoms are registered in the static element of class AtomDef
/// private:
///   static std::list<AtomDef*> logbook;
/// The can be obtained by notations by:
/// public:
///   static const std::list<AtomDef*>& get_AtomDefLogbook();
///   static AtomDef* get_AtomDef(const std::string& fnotation);
///  returns the address of atom with this name if it is registered in system,
/// or NULL otherwise.
///
/// In these files and in the other components of the matter package
/// the principles are similar. This is the principle of database, the principle
/// of the strict protection of internal data (variables marked by
/// suffix 'h') and granting access though the functions which have similar
/// names without this suffix 'h'.
///
/// 1998-2004, I. Smirnov.

class AtomDef {
  std::string nameh = "none";
  std::string notationh = "none";
  /// Atomic number.
  int Zh = 0;
  /// Atomic mass in internal units. Transfer to gram/mole if need.
  double Ah = 0.;
  static constexpr int max_poss_atom_z = 100;

 public:
  /// Default constructor
  AtomDef();
  /// Constructor
  AtomDef(const std::string& fnameh, const std::string& fnotationh, int fZh,
          double fAh);
  /// Destructor
  ~AtomDef();

  const std::string& name() const { return nameh; }
  const std::string& notation() const { return notationh; }
  int Z() const { return Zh; }
  double A() const { return Ah; }
  /// Print all registered atoms.
  static void printall(std::ostream& file);
  /// Check that there is no atom with the same name in the container.
  void verify();
  /// Initialize the logbook at the first request
  /// and keep it as internal static variable.
  static std::list<AtomDef*>& get_logbook();
  static const std::list<AtomDef*>& get_const_logbook();
  /// Return the address of atom with this name if it is registered in system,
  /// or NULL otherwise
  static AtomDef* get_AtomDef(const std::string& fnotation);
  /// Return the atomic number corresponding to a given Z.
  /// If the atom is not registered, the current version
  /// terminates the program through spexit(). Be careful!
  static double get_A(int fZ);
  /// Return the address of atom corresponding to a given Z.
  /// If the atom is not registered, the current version
  /// terminates the program through spexit(). Be careful!
  static AtomDef* get_AtomDef(int fZ);

  void print(std::ostream& file, int l = 0) const;
  AtomDef* copy() const { return new AtomDef(*this); } 
};
std::ostream& operator<<(std::ostream& file, const AtomDef& f);

/// Definition of atomic mixtures. Pointers to atoms, weights and
/// various mean parameters.

class AtomMixDef {
  /// Number of different atoms.
  long qatomh = 0;
  /// Constituent atoms.
  std::vector<AtomDef*> atomh;
  std::vector<double> weight_quanh;  // sum is 1
  std::vector<double> weight_massh;  // sum is 1

  /// Weighted mean Z
  double Z_meanh = 0.;
  /// Weighted mean A (in internal units). Transfer to gram/mole if needed.
  double A_meanh = 0.;
  /// Weighted mean 1 / A (in internal units).
  double inv_A_meanh = 0.;
  /// Weighted mean ratio Z / A.
  double mean_ratio_Z_to_Ah = 0.;
  double NumberOfElectronsInGramh = 0.;

 public:
  /// Default constructor
  AtomMixDef() = default;
  AtomMixDef(unsigned long fqatom, const std::vector<std::string>& fatom_not,
             const std::vector<double>& fweight_quan);
  AtomMixDef(unsigned long fqatom, const std::vector<std::string>& fatom_not,
             const std::vector<long>& fweight_quan);
  AtomMixDef(const std::string& fatom_not);
  AtomMixDef(const std::string& fatom_not1, double fweight_quan1,
             const std::string& fatom_not2, double fweight_quan2);
  AtomMixDef(const std::string& fatom_not1, double fweight_quan1,
             const std::string& fatom_not2, double fweight_quan2,
             const std::string& fatom_not3, double fweight_quan3);
  AtomMixDef(const std::string& fatom_not1, double fweight_quan1,
             const std::string& fatom_not2, double fweight_quan2,
             const std::string& fatom_not3, double fweight_quan3,
             const std::string& fatom_not4, double fweight_quan4);
  void print(std::ostream& file, int l) const;
  long qatom() const { return qatomh; }
  const std::vector<AtomDef*>& atom() const { return atomh; }
  AtomDef* atom(long n) const { return atomh[n]; }
  const std::vector<double>& weight_quan() const { return weight_quanh; }
  const std::vector<double>& weight_mass() const { return weight_massh; }
  double weight_quan(long n) const { return weight_quanh[n]; }
  double weight_mass(long n) const { return weight_massh[n]; }
  double Z_mean() const { return Z_meanh; }
  double A_mean() const { return A_meanh; }
  double inv_A_mean() const { return inv_A_meanh; }
  double mean_ratio_Z_to_A() const { return mean_ratio_Z_to_Ah; }
  double NumberOfElectronsInGram() const {
    return NumberOfElectronsInGramh;
  }
};
std::ostream& operator<<(std::ostream& file, const AtomMixDef& f);
}

#endif

#ifndef ATOM_DEF_H
#define ATOM_DEF_H

#include <iostream>
#include "wcpplib/util/String.h"
#include "wcpplib/safetl/AbsArr.h"
#include "wcpplib/safetl/AbsList.h"

/*
Definition of atoms. Only the basic information: the name, the notation,
the atomic weight and charge.

Definition of atomic mixtures. Pointers to atoms, weights and
various mean parameters.

The principle of definitions of atoms is dictionary or a database:
the atoms are not repeated,
each atom is presented in the total system no more than one time.
The system knows each atom presented in it.
The atom characteristics can be obtained by literal notation.
The system declines the secondary initialization.
The copying is not declined.
When the user program wants to refer to atom,
it has to use either char* (String) notation, or pointer (or reference)
to one of these objects.
As usually, in the case of pointers I recommend to use protected pointers
to external objects PassivePtr.
The user pogram can initialize the new atoms.
The standard atoms are initiated in files GasLib.h and GasLib.c.

In principle I am going to initiate all atoms from Mendeleev's table,
but I haven't finished yet. Only its first half is filled at the moment.

The atoms are registered in the static element of class AtomDef
private:
  static AbsList< AtomDef* > logbook;
The can be obtained by notations by:
public:
  static const AbsList< AtomDef* >& get_AtomDefLogbook(void);
  static AtomDef* get_AtomDef(const String& fnotation);
  // returns the address of atom with this name if it is registered in system,
  // or NULL otherwise

The definition of atomic mixture is a simple class. It is heavily used
for definition of matter and gas.

In these files and in the other components of the matter package
the principles are similar. This is the principle of database, the principle
of the strict protection of internal data (variables marked by
suffix 'h') and granting access though the functions which have similar
names without this suffix 'h'.


1998-2004, I. Smirnov.
*/

namespace Heed {

class AtomDef : public RegPassivePtr {
  String nameh;
  String notationh;
  int Zh;
  // Atomic mass in internal units. Transfer to gram/mole if need.
  double Ah;
  static const int max_poss_atom_z = 100;

 public:
  AtomDef(void);
  AtomDef(const String& fnameh, const String& fnotationh, int fZh, double fAh);
  ~AtomDef();
  void print(std::ostream& file, int l = 0) const;
  const String& name(void) const { return nameh; }
  const String& notation(void) const { return notationh; }
  int Z(void) const { return Zh; }
  double A(void) const { return Ah; }
  // Print all registered atoms
  static void printall(std::ostream& file);
  // Check that there is no atom with the same name in the container
  void verify(void);
  // Initialize the logbook at the first request
  // and keep it as internal static variable.
  static AbsList<AtomDef*>& get_logbook(void);
  static const AbsList<AtomDef*>& get_const_logbook(void);
  // Return the address of atom with this name if it is registered in system,
  // or NULL otherwise
  static AtomDef* get_AtomDef(const String& fnotation);
  // Return the atomic number corresponding to a given Z.
  // If the atom is not registered, the current version
  // terminates the program through spexit(). Be careful!
  static double get_A(int fZ);
  // Return the address of atom corresponding to a given Z.
  // If the atom is not registered, the current version
  // terminates the program through spexit(). Be careful!
  static AtomDef* get_AtomDef(int fZ);

  macro_copy_total(AtomDef);
};
std::ostream& operator<<(std::ostream& file, const AtomDef& f);

class AtomMixDef : public RegPassivePtr {
  // Number of different atoms
  long qatomh;
  DynLinArr<PassivePtr<AtomDef> > atomh;
  DynLinArr<double> weight_quanh;  // sum is 1
  DynLinArr<double> weight_massh;  // sum is 1

  // Weighted means
  double Z_meanh;
  double A_meanh;      // in internal units. Transfer to gram/mole if needed.
  double inv_A_meanh;  // in internal units. Transfer to (1.0/(gram/mole)),
                       // if needed
                       // Z_meanh / A_meanh;
  double mean_ratio_Z_to_Ah;
  double NumberOfElectronsInGramh;

 public:
  AtomMixDef(void)
      : qatomh(0),
        Z_meanh(0.0),
        A_meanh(0.0),
        inv_A_meanh(0.0),
        mean_ratio_Z_to_Ah(0.0),
        NumberOfElectronsInGramh(0.0) {
    ;
  }
  AtomMixDef(long fqatom, const DynLinArr<String>& fatom_not,
             const DynLinArr<double>& fweight_quan);
  AtomMixDef(long fqatom, const DynLinArr<String>& fatom_not,
             const DynLinArr<long>& fweight_quan);
  AtomMixDef(const String& fatom_not);
  AtomMixDef(const String& fatom_not1, double fweight_quan1,
             const String& fatom_not2, double fweight_quan2);
  AtomMixDef(const String& fatom_not1, double fweight_quan1,
             const String& fatom_not2, double fweight_quan2,
             const String& fatom_not3, double fweight_quan3);
  AtomMixDef(const String& fatom_not1, double fweight_quan1,
             const String& fatom_not2, double fweight_quan2,
             const String& fatom_not3, double fweight_quan3,
             const String& fatom_not4, double fweight_quan4);
  void print(std::ostream& file, int l) const;
  long qatom(void) const { return qatomh; }
  const DynLinArr<PassivePtr<AtomDef> >& atom(void) const { return atomh; }
  PassivePtr<AtomDef> atom(long n) const { return atomh[n]; }
  const DynLinArr<double>& weight_quan(void) const { return weight_quanh; }
  const DynLinArr<double>& weight_mass(void) const { return weight_massh; }
  double weight_quan(long n) const { return weight_quanh[n]; }
  double weight_mass(long n) const { return weight_massh[n]; }
  double Z_mean(void) const { return Z_meanh; }
  double A_mean(void) const { return A_meanh; }
  double inv_A_mean(void) const { return inv_A_meanh; }
  double mean_ratio_Z_to_A(void) const { return mean_ratio_Z_to_Ah; }
  double NumberOfElectronsInGram(void) const {
    return NumberOfElectronsInGramh;
  }
};
std::ostream& operator<<(std::ostream& file, const AtomMixDef& f);

}

#endif

#ifndef ATOM_DEF_H
#define ATOM_DEF_H

#include <iostream>
using std::ostream;
#include "wcpplib/util/String.h"
//#include "list/AbsPtr.h"
#include "wcpplib/safetl/AbsArr.h"
#include "wcpplib/safetl/AbsList.h"
//#include "wcpplib/util/List.h"
//#include "CLHEP/Alist/AList.h"

/*
Definition of atoms. Only the basic information: the name, the notation,
the atomic weight and charge.

Definition of atomics mixtures. Pointers to atoms, weights and
various mean parameters.

The principle of definitions of atoms is dictionary or a database: 
the atoms are not repreated,
each atom is presented in the total system no more than one time. 
The system knows each atom presented in it. 
The atom characteristics can be obtained by literal notation.
The system declines the secondary initialization.
The copying is not declined. 
When the user program wants to refere to atom, 
it has to use either char* (String) notation, or pointer (or reference) 
to one of these objects.
As usually, in the case of pointers I recommend to use protected pointers 
to external objects PassivePtr.
The user pogram can initialize the new atoms.
The standard atoms are inited in files GasLib.h and GasLib.c. 

In principle I am going to inite all atoms from Mendeleev's table,
but I havn't finished yet. Only its first half is filled at the moment.

The atoms are registered in the static element of class AtomDef
private:
  static AbsList< AtomDef* > logbook;
The can be ontained by nonations by:
public:
  static const AbsList< AtomDef* >& get_AtomDefLogbook(void);
  static AtomDef* get_AtomDef(const String& fnotation);
  // returns the address of atom with this name if it is registered in system,
  // of NULL otherwise 


The definition of atomic mixture is a simple class. It is heavily used
for definition of matter and gas.

In these files and in the other components of the matter package
the principles are similar. This is the principle of database, the principle 
of the strict protection of internal data (variables marked by
suffix 'h') and granting access though the functions which have similar
names without this suffix 'h'. 


1998-2004, I. Smirnov.
*/

const int max_poss_atom_z=100;

class AtomDef: public RegPassivePtr
{
  String nameh;
  String notationh;
  int Zh;
  double Ah;  // in internal units. Transfer to gram/mole if need.
public:
  inline const String& name(void) const {return nameh;}
  inline const String& notation(void) const {return notationh;}
  inline int Z(void) const {return Zh;}
  inline double A(void) const {return Ah;}
  AtomDef(void);
  //AtomDef(const AtomDef& f);            // call is forbidden, terminates
  //AtomDef& operator=(const AtomDef& f); // call is forbidden, terminates
  AtomDef(const String& fnameh, const String& fnotationh,
	   int fZh, double fAh);
  ~AtomDef();
  void print(ostream & file, int l=0) const;
  static void printall(ostream & file); // print all registered atoms
  void verify(void); 
  // checks that there is no atom with the same name in
  // the container
  //private:
  //  static AbsList< AtomDef* > logbook;
public:  // declared public, but not modify externnally.
  // Actually it is private, but the static function can not be
  // declared public somewhy
  static AbsList< AtomDef* >& get_logbook(void);
  // This function initializes the logbook at the first request
  // and keeps it as internal static variable.

public:
  static const AbsList< AtomDef* >& get_const_logbook(void);
  // const for external use

public:
  //static const AbsList< AtomDef* >& get_AtomDefLogbook(void);
  static AtomDef* get_AtomDef(const String& fnotation);
  // returns the address of atom with this name if it is registered in system,
  // of NULL otherwise 
public:
  static double get_A(int fZ);  // return the atomic number corresponding
  // to given Z provided that the atom is registered.
  // If the atom is not registered, the current version
  // terminates the program through spexit(). Be careful!
  static AtomDef* get_AtomDef(int fZ); 
  // If the atom is not registered, the current version
  // terminates the program through spexit(). Be careful!

  macro_copy_total(AtomDef);
  //static const AtomDef::cont& get_AtomDefCont(void);
  //AnyType_copy(AtomDef, AtomDef);
  //virtual void print(ostream& file, int l) const { print(file); }  
};
ostream & operator << (ostream & file, const AtomDef & f);


class AtomMixDef: public RegPassivePtr
{
  long qatomh;  // number of different atoms
  DynLinArr< PassivePtr<AtomDef> >atomh;
  DynLinArr< double > weight_quanh;  // sum is 1
  DynLinArr< double > weight_massh;  // sum is 1

  // mean per one atom (ordinary means with taking into account 
  // quantitative weights)
  double Z_meanh;
  double A_meanh;  // in internal units. Transfer to gram/mole if need.
  double inv_A_meanh;  // in internal units. Transfer to (1.0/(gram/mole)),
                       // if need
  double mean_ratio_Z_to_Ah;  // is Z_meanh / A_meanh;  
  double NumberOfElectronsInGramh;

public:

  inline long qatom(void) const {return qatomh;}
  inline const DynLinArr< PassivePtr<AtomDef> >& atom(void) const 
    {return atomh;}
  inline PassivePtr<AtomDef> atom(long n) const {return atomh[n]; }
  inline const DynLinArr< double >& weight_quan(void) const 
    {return weight_quanh; }
  inline const DynLinArr< double >& weight_mass(void) const 
    {return weight_massh; }
  inline double  weight_quan(long n) const 
    {return weight_quanh[n]; }
  inline double weight_mass(long n) const 
    {return weight_massh[n]; }
  inline double Z_mean(void) const {return Z_meanh;}
  inline double A_mean(void) const {return A_meanh;}
  inline double inv_A_mean(void) const {return inv_A_meanh;}
  inline double mean_ratio_Z_to_A(void) const {return mean_ratio_Z_to_Ah;}
  inline double NumberOfElectronsInGram(void) const 
    {return NumberOfElectronsInGramh;}
  AtomMixDef(void):qatomh(0), Z_meanh(0.0), A_meanh(0.0), 
      inv_A_meanh(0.0), mean_ratio_Z_to_Ah(0.0), NumberOfElectronsInGramh(0.0)
    {;}
  AtomMixDef(long fqatom, const DynLinArr< String >& fatom_not,
	     const DynLinArr< double >& fweight_quan);
  AtomMixDef(long fqatom, const DynLinArr< String >& fatom_not,
	     const DynLinArr< long >& fweight_quan);
  //AtomMixDef(long fqatom, const DynLinArr< String >& fatom_not,
  //	     const DynLinArr< long >& fqatom_ps);
  AtomMixDef(const String& fatom_not);
  AtomMixDef(const String& fatom_not1, double  fweight_quan1,
	     const String& fatom_not2, double  fweight_quan2);
  AtomMixDef(const String& fatom_not1, double  fweight_quan1,
	     const String& fatom_not2, double  fweight_quan2,
	     const String& fatom_not3, double  fweight_quan3);
  AtomMixDef(const String& fatom_not1, double  fweight_quan1,
	     const String& fatom_not2, double  fweight_quan2,
	     const String& fatom_not3, double  fweight_quan3,
	     const String& fatom_not4, double  fweight_quan4);
  //AtomMixDef(const String& fatom_not1, long fqatom_ps1,
  //	     const String& fatom_not2, long fqatom_ps2);

  void print(ostream & file) const;

};

ostream & operator << (ostream & file, const AtomMixDef & f);




#endif

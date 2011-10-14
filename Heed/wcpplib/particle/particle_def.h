#ifndef PARTICLE_DEF_H
#define PARTICLE_DEF_H

//#include <iostream.h>
#include "wcpplib/util/String.h"
//#include "wcpplib/util/List.h"
#include "wcpplib/safetl/AbsPtr.h"
#include "wcpplib/safetl/AbsList.h"
//#include "CLHEP/Alist/AList.h"

/*
Definition of particles. Only the basic information: the name, the notation,
the mass, the charge, and other auxiliary data.

The organization is similar to AtomDef from directory matter, with
the exception that the internal data are not declared as private.
Of course, the user should not shange them.

The principle of definitions of particles is dictionary or a database: 
the particles are not repreated,
each particle is presented in the total system no more than one time. 
The system knows each particle presented in it. 
The particle characteristics can be obtained by literal notation.
The system declines the secondary initialization.
The copying is not declined. 
When the user program wants to refere to particle, 
it has to use either char* (String) notation, or pointer (or reference) 
to one of these objects.
As usually, in the case of pointers I recommend to use protected pointers 
to external objects PassivePtr.
The user pogram can initialize the new particles.
The standard particles are inited right here, below.

There is auxiliary class particle_type, convenient for
definition of classes derived from particle.
The derivation from particle is not possible by the standard way,
since the system rejects the second particle with the same name.
One cannot derive the class from pointer. But the pointer can be
allocated as member of another little class, and from this class
one can derive anything. This little class with pointer is 
class particle_type.
This class is also convenient identity of particles by comparing the
pointers.

1999 - 2004,   I. Smirnov

*/


class spin_def
{public:
  float total;
  float projection;
  spin_def(void): total(0), projection(0) {;}
  spin_def(float ftotal, float fprojection);
};
ostream & operator << (ostream & file, const spin_def & f);


class particle_def: public RegPassivePtr
{public:
  String name;
  String notation;  // short name to make data summary files short
  double mass;
  double charge;
  //  The following is not yet used in programs
  int lepton_n;
  int barion_n;
  float spin;
  //float isospin;
  spin_def isospin;
  //int parity;
  particle_def(void):name("none"), notation("none"), mass(0), charge(0), 
    lepton_n(0), barion_n(0), spin(0), isospin(0,0) 
    {
      particle_def::get_logbook().append(this);
    }
  particle_def(const String& fname, const String& fnotation, 
	       double fmass, double fcharge, 
	       int flepton_n, int fbarion_n, 
	       float fspin, const spin_def& fisospin);
  particle_def(const String& fname, const String& fnotation, 
	       double fmass, double fcharge, 
	       int flepton_n, int fbarion_n, 
	       float fspin, float fisospin_total, float fisospin_proj)
    { *this=particle_def(fname, fnotation, fmass, fcharge, 
			 flepton_n, fbarion_n,
			 fspin, spin_def(fisospin_total, fisospin_proj));
    }

  particle_def(const particle_def& f)
    {
      *this=f; verify(); 
      particle_def::get_logbook().append(this);
    }
  //void operator=(const particle_def& f);

  particle_def anti_particle(const particle_def& p); // function for making of 
  // anti particle

  particle_def(const String& fname, const String& fnotation, particle_def& p); 
    // creates anti-particle through the call of anti_particle(p)

  ~particle_def() { particle_def::get_logbook().remove(this); }
  void print(ostream & file);
  static void printall(ostream & file);

  void set_mass(const double m);
  void set_charge(const double z);

  //private:
  //static AbsList< particle_def* > logbook;
public:  // declared public, but not modify externally.
  // Actually it is private, but the static function can not be
  // declared public somewhy
  static AbsList< particle_def* >& get_logbook(void);
  // This function initializes the logbook atits first request
  // and keeps it as internal static variable.

public:
  static const AbsList< particle_def* >& get_const_logbook(void);
  // const for external use

  static particle_def* get_particle_def(const String& fnotation);
  // returns the address of particle with this name 
  // if it is registered in system,
  // of NULL otherwise 
  void verify(void){}; 
  // checks that there is no particle with the same name in
  // the container
};

ostream& operator << (ostream& file, const particle_def& f);


extern particle_def electron_def;
extern particle_def positron_def;
extern particle_def muon_minus_def;
extern particle_def muon_plus_def;
extern particle_def proton_def;
extern particle_def anti_proton_def;
extern particle_def neutron_def;
extern particle_def anti_neutron_def;
extern particle_def P11_def;
extern particle_def D13_def;
extern particle_def S11_def;

// light unflavored mesons
extern particle_def pi_plus_meson_def;  
extern particle_def pi_0_meson_def;
extern particle_def pi_minus_meson_def;
extern particle_def eta_meson_def;
extern particle_def K_plus_meson_def;
extern particle_def K_minus_meson_def;

extern particle_def deuteron_def;
extern particle_def alpha_particle_def;

// "exotic" particles with properties specified by user
extern particle_def user_particle_def;

//const int pqallapardef=50;
//extern int qallapardef; // user can define more particles
//extern particle_def* allapardef[pqallapardef];

class particle_type
{public:
  PassivePtr< particle_def > pardef;
  particle_type(void): pardef(NULL) {};
  //particle_type(const particle_type& f): pardef(f.pardef) {};
  particle_type(particle_def* f): pardef(f) {};
  particle_type(const char* name, int s=0);
    // name is notation or name.
    // First the list of notations is checked,
    // Then the list of names is chacked as well. 
    // s controls error handling
    // If the name is absent in the particle list then
    //   If s==0, the program is terminated
    //   Otherwise the pardef is set to NULL 
  int operator==(const particle_type& f)
    { return pardef.getver() == f.pardef.getver() ? 1 : 0; }
  int operator!=(const particle_type& f)
    { return pardef.getver() != f.pardef.getver() ? 1 : 0; }
  void print_notation(ostream& file) const;
};
ostream& operator << (ostream& file, const particle_type& f);

#endif

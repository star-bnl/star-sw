#ifndef MATTER_DEF_H
#define MATTER_DEF_H

#include "wcpplib/matter/AtomDef.h"
/*
Definition of matter (material or any media). 
Only the basic information: the name, the notation,
the atomic mixture, temperature, density, effective ionization potential.

The principle of definitions of matters is the same as for atoms:
a dictionary or a database. See details there. But the logbook is different,
of course.

1998-2004 I. Smirnov
*/ 


class MatterDef: public AtomMixDef
{
  String nameh;
  String notationh;
  double temperatureh;
  double densityh;
  double I_effh;
  void calc_I_eff(void);
public:

  inline const String& name(void) const  {return nameh;}
  inline const String& notation(void) const  {return notationh;}
  inline double density(void) const {return densityh; }
  inline double temperature(void) const {return temperatureh;}
  inline double I_eff(void) const {return I_effh;}
  // I_eff is still not very reliable.
  // There is too many approximations for this.
  // Here is a simplest and probably not good.

  MatterDef(void);
  //MatterDef(const MatterDef& f);            // call is forbidden, terminates
  //MatterDef& operator=(const MatterDef& f); // call is forbidden, terminates 
  MatterDef(const String& fname, const String& fnotation,
	    long fqatom, const DynLinArr< String >& fatom_not,
	    const DynLinArr< double >& fweight_quan, 
	    double fdensity, double ftemperature);
  MatterDef(const String& fname, const String& fnotation,
	    const String& fatom_not, double fdensity, double ftemperature);
  MatterDef(const String& fname, const String& fnotation,
	    const String& fatom_not1, double fweight_quan1,
	    const String& fatom_not2, double fweight_quan2, 
	    double fdensity, double ftemperature);
  MatterDef(const String& fname, const String& fnotation,
	    const String& fatom_not1, double fweight_quan1,
	    const String& fatom_not2, double fweight_quan2, 
	    const String& fatom_not3, double fweight_quan3, 
	    double fdensity, double ftemperature);
  ~MatterDef();

  virtual void print(ostream & file, int l=0) const;
  static void printall(ostream & file);
  void verify(void); 
  // checks that there is no molecule with the same name in
  // the container
  static void verify(const String& fname, const String& fnotation); 
  //protected:
  //static AbsList< MatterDef* > logbook;
public:  // declared public, but not modify externnally.
  // Actually it is private, but the static function can not be
  // declared public somewhy
  static AbsList< MatterDef* >& get_logbook(void);
  // This function initializes the logbook atits first request
  // and keeps it as internal static variable.

public:
  static const AbsList< MatterDef* >& get_const_logbook(void);
  // const for external use
  //static const AbsList< MatterDef* >& get_MatterDefLogbook(void);
  static MatterDef* get_MatterDef(const String& fnotation);
  // If no matter with this notation, returns NULL.

  macro_copy_total(MatterDef);
  //AnyType_copy(MatterDef, MatterDef);
  //virtual void print(ostream& file, int l) const { print(file); }  

};

ostream & operator << (ostream & file, const MatterDef & f);

class MatterType
{public:
  PassivePtr<MatterDef> matdef;
  MatterType(void): matdef() {;}
  MatterType(MatterDef* md): matdef(md) {;}
  //virtual void empty_func(void){ ; }
};
ostream & operator << (ostream & file, const MatterType & f);


#endif

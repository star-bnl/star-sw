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
  // I_eff is still not very reliable.
  // There are too many approximations for this.
  // Here is a simplest and probably not good.
  void calc_I_eff(void);
public:
  MatterDef(void);
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
  virtual void print(std::ostream& file, int l) const;
  static void printall(std::ostream& file);
  const String& name(void) const  {return nameh;}
  const String& notation(void) const  {return notationh;}
  double density(void) const {return densityh; }
  double temperature(void) const {return temperatureh;}
  double I_eff(void) const {return I_effh;}
  // Check that there is no matter with the same name in the container
  void verify(void); 
  static void verify(const String& fname, const String& fnotation); 
  // Initialize the logbook at the first request
  // and keep it as internal static variable.
  static AbsList< MatterDef* >& get_logbook(void);
  static const AbsList< MatterDef* >& get_const_logbook(void);
  // Return the adress of the matter with this notation if it is registered.
  // Otherwise return NULL.
  static MatterDef* get_MatterDef(const String& fnotation);

  macro_copy_total(MatterDef);
};
std::ostream& operator << (std::ostream& file, const MatterDef& f);

class MatterType {
public:
  PassivePtr<MatterDef> matdef;
  MatterType(void): matdef() {;}
  MatterType(MatterDef* md): matdef(md) {;}
};
std::ostream & operator << (std::ostream& file, const MatterType& f);

#endif

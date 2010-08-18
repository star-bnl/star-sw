#ifndef GAS_DEF_H
#define GAS_DEF_H

#include "wcpplib/matter/MatterDef.h"
#include "wcpplib/matter/MoleculeDef.h"

/* Characteristic feature of the Gas with respect to any matter is that 
this matter consists of molecules.
Additional feature is that the density can be calculated by temperature 
and pressure. But this is not always, and therefore it is not characteristic
feature. Then with only one this feature (constisting of molecules) 
we can describe as gas also some other substances, for example, the liquids.

Only the basic information: the data of matter, plus the pressure.
Note that the class AtomMixDef undirectly appear twice.
It is the base class of matter and molecula. Therefore it is
indirectly the base class of GasDef, and the base class
of its external elements - DynLinArr< PassivePtr<MoleculeDef> >molech.

As the base class of GasDef, the class AtomMixDef determines only the
relative weights of atoms of different sorts. Also note that
the atoms of the same sorts participated in different molecula,
included in AtomMixDef as different atoms.

As the base class of MoleculeDef, the class AtomMixDef determines
also only the relative weights of atoms of different sorts in the given
molecule, since the class AtomMixDef don't have space to encapsulate
the number of atoms. But the latter number is also necessary: consider H2, 
the relative weight of H is 1, and nothing says that there are two atoms.
Therefore in the class MoleculeDef there is additional array, which
gives the numbers of atoms of each sort, and also there is another
parameter giving the total number of atoms in molecule. 

1998-2004 I. Smirnov
*/ 

class GasDef: public MatterDef
{
  double pressureh;
  long qmolech;  // number of different moleculas
  DynLinArr< PassivePtr<MoleculeDef> >molech;
  DynLinArr< double > weight_quan_molech;  // sum is 1
  DynLinArr< double > weight_mass_molech;  // sum is 1
public:
  inline double pressure(void) const {return pressureh;}
  inline long qmolec(void) const {return qmolech;}
  inline const DynLinArr< PassivePtr<MoleculeDef> >& molec(void) const 
    {return molech;}
  inline PassivePtr<MoleculeDef> molec(long n) const {return molech[n]; }
  inline const DynLinArr< double >& weight_quan_molec(void) const 
    {return weight_quan_molech; }
  inline const DynLinArr< double >& weight_mass_molec(void) const 
    {return weight_mass_molech; }
  inline double  weight_quan_molec(long n) const 
    {return weight_quan_molech[n]; }
  inline double weight_mass_molec(long n) const 
    {return weight_mass_molech[n]; }
  double Z_mean_molec(void) const; // mean charge of molecula
    // in this gas 

  GasDef(void);

  // for calculation of density assume ideal gas:
  GasDef(const String& fname, const String& fnotation,
	 long fqmolec, const DynLinArr< String >& fmolec_not,
	 const DynLinArr< double >& fweight_quan_molec,
	 double fpressure, double ftemperature, double fdensity=-1.0);

  // for calculation of density assume Van der Waals 
  // for the components for which the parameters are defined:
  GasDef(const String& fname, const String& fnotation,
	 long fqmolec, const DynLinArr< String >& fmolec_not,
	 const DynLinArr< double >& fweight_volume_molec,
	 double fpressure, double ftemperature, 
	 int s1, int s2);  // s1 and s2 are to distunguish the constructor

  // for calculation of density assume ideal gas:
  GasDef(const String& fname, const String& fnotation,
	 const String& fmolec_not,
	 double fpressure, double ftemperature, double fdensity=-1.0);

  // for calculation of density assume Van der Waals gas:
  GasDef(const String& fname, const String& fnotation,
	 const String& fmolec_not,
	 double fpressure, double ftemperature, int s1, int s2);

  // for calculation of density assume ideal gas:
  GasDef(const String& fname, const String& fnotation,
	 const String& fmolec_not1, double fweight_quan_molec1,
	 const String& fmolec_not2, double fweight_quan_molec2,
	 double fpressure, double ftemperature, double fdensity=-1.0);

  // for calculation of density assume Van der Waals gas:
  GasDef(const String& fname, const String& fnotation,
	 const String& fmolec_not1, double fweight_volume_molec1,
	 const String& fmolec_not2, double fweight_volume_molec2,
	 double fpressure, double ftemperature, int s1, int s2);

  // for calculation of density assume ideal gas:
  GasDef(const String& fname, const String& fnotation,
	 const String& fmolec_not1, double fweight_quan_molec1,
	 const String& fmolec_not2, double fweight_quan_molec2,
	 const String& fmolec_not3, double fweight_quan_molec3,
	 double fpressure, double ftemperature, double fdensity=-1.0);

  // for calculation of density assume Van der Waals gas:
  GasDef(const String& fname, const String& fnotation,
	 const String& fmolec_not1, double fweight_volume_molec1,
	 const String& fmolec_not2, double fweight_volume_molec2,
	 const String& fmolec_not3, double fweight_volume_molec3,
	 double fpressure, double ftemperature, int s1, int s2);

  // for calculation of density assume ideal gas:
  GasDef(const String& fname, const String& fnotation,
	 const GasDef& gd, 
	 double fpressure, double ftemperature, double fdensity=-1.0);

  
  virtual void print(ostream & file, int l=0) const;
  macro_copy_total(GasDef);
  //AnyType_copy(GasDef, MatterDef);
  //virtual void print(ostream& file, int l) const { print(file); }  

};

extern const double mm_rt_st_in_atmosphere;

ostream & operator << (ostream & file, const GasDef & f);



double gasdensity(double temperature, double pressure,
		  DynLinArr< PassivePtr<MoleculeDef> >molec,
		  DynLinArr< double > weight_quan_molec,
		  long qmolec);

/*
double gasdensity_Vaals(double temperature, double pressure,
			DynLinArr< PassivePtr<MoleculeDef> >molec,
			DynLinArr< double > weight_volume,
			long qmolec);
*/

#endif

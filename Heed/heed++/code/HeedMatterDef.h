#ifndef HEEDMATTERDEF_H
#define HEEDMATTERDEF_H

#include "wcpplib/matter/MatterDef.h"
#include "wcpplib/matter/GasDef.h"
#include "heed++/code/EnergyMesh.h"
#include "heed++/code/PhotoAbsCS.h"

/*
Definition of matter parameters necessary for HEED.
This is photoabsorption cross section, dielectric constant
and other parameters related to these.
All the parameters depending on energy are kept in arrays
associated with specific energy mesh, which should be defined.

The principle is ordinary: definition of just a class.
To the contrary with wcpplib/matter, there is no any global "database"
and no formal ban to duplicate these definitions 
(although there would not be sense in duplication).

2003, I. Smirnov
*/

const int s_use_mixture_thresholds = 0;
/*
Affects the mixtures in which atoms have different potentials of ionization. 
If 1, all energy transfers what is absorbed even with the energy less than
the potential of ionization of single atom, but more than the minimal
potential of ionization of the mixture, should ionize.
This is emulation of Jesse effect in extreme case.
It is likely not realistic.
So the recommended value is 0.
*/

class HeedMatterDef: public RegPassivePtr
{
public:
  PassivePtr< MatterDef > matter;
  DynLinArr< PassivePtr<const AtomPhotoAbsCS> > apacs;
  // Each element of this array corresponds to component of matter
  double eldens_cm_3;  // electron density cm**-3
  double eldens;       // electron density MeV**3
  double xeldens;      // longitudinal electron density MeV**2/cm (for x=1 cm)
  double wpla;         // squared plasm energy;
  double radiation_length;  // Radiation Length.
  double Rutherford_const;  // Const for Rutherford cross section (1/cm3).
  double W;	       // Mean work per pair production, MeV
  double F;	       // Fano factor
  PassivePtr< EnergyMesh > energy_mesh;

  // The physical definition of two previous arrays of values:
  // mean values of cross sections for each energy interval.
  DynLinArr< double > ACS;   // Photoabsorbtion cross section per one atom(Mb).
  DynLinArr< double > ICS;   // Photoionization cross section per one atom(Mb).
  DynLinArr< double > epsip; // some plasma dielectric constant. 
                             // (not used, but just initialized for print)
                             // (in order to take into account bounds, 
                             // one has to multiply this by some integral)

  DynLinArr< double > epsi1; // real part of dielectric constant (e_1 - 1)
  DynLinArr< double > epsi2; // imaginary part of dielectric constant.
  double min_ioniz_pot;	     // Minimum ionization potential,
                             // it is using only for switching off 
                             // the Cherenkov radiation below it.
  HeedMatterDef(void);
  // In the following, if fW == 0.0, the program takes mean W from 
  // molecules for gas or from atoms for matters.
  // If fF is input as 0.0, it is assigned to be mean for gas.
  // For matters this is the terminating error. 
  HeedMatterDef(EnergyMesh* fenergy_mesh,
		MatterDef* amatter, 
		AtomPhotoAbsCS* faapacs[], // array of size corresponding matter
		double fW = 0.0, double fF = standard_factor_Fano);
  // Gas consists of molecules, molecules of atoms
  // The order in which molecules appear in fampacs should correspond
  // to that of agas.
  // The order in which atoms appear in fampacs[n] should correspond to that
  // of molecules in gas.
  HeedMatterDef(EnergyMesh* fenergy_mesh,
		GasDef* agas, 
		MolecPhotoAbsCS* fampacs[], // array of size corresponding gas
		double fW = 0.0, double fF = standard_factor_Fano);
  HeedMatterDef(EnergyMesh* fenergy_mesh,
		const String& gas_notation, 
		MolecPhotoAbsCS* fampacs[], // array of size corresponding gas
		double fW = 0.0, double fF = standard_factor_Fano);
  // Replace permeability (epsi1 and epsi2) by the numbers 
  // calculated by another program and written to a file (only for debug)
  void replace_epsi12(const String& file_name);
  virtual void print(std::ostream& file, int l) const ;
  macro_copy_total(HeedMatterDef);
	
private:
  // Initialization after assignment of matter and apacs
  void inite_HeedMatterDef(void); 
};

#endif



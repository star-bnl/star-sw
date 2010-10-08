#include "heed++/code/PhotoAbsCSLib.h"
#include "heed++/code/PhysicalConstants.h"
/*
2004, I. Smirnov
*/

#ifdef NOT_INCLUDE_GASLIB_IN_PACSLIB
#else
#include "wcpplib/matter/GasLib.c"  
#endif
// to guarantee earlier initialization 
// include the source to provide preliminary initialization.
/*
Remark  14.10.2005:

The text in this file does not depend on wcpplib/matter/GasLib.c
or wcpplib/matter/GasLib.h.
The first was included earlier when it was thought to depend.
But there is anyway a sense in such inclusion.
If the user needs to provide sertain order of initialization,
he will need to include both PhotoAbsCSLib.c and GasLib.c
in certain his source file.
If GasLib.c is already included here, there is no need to include 
it again there and there will be only one line of initializations.
But some people complains that such inclusion is inconvenient
and unnecessary if they don't have global objects dependent
on gas features. To allow them to switch this inclusion off,
the macro NOT_INCLUDE_GASLIB_IN_PACSLIB is introduced.
If it is defined, GasLib.c is NOT included.
Similar behaviour applies to GasLib.h and PhotoAbsCSLib.h,
because in many current test programs header GasLib.h is not included,
but it should be included at least indirectly.
*/ 

char* a_internal_HDB;
String shelllist_dir_name = 
String((a_internal_HDB = getenv("HEED_DATABASE")) == NULL ? "" : 
       a_internal_HDB ) + "/";
String pacs_table_dir_name = 
String((a_internal_HDB = getenv("HEED_DATABASE")) == NULL ? "" : 
       a_internal_HDB) + "/henke/";
//String shelllist_dir_name = "conv/";
//String pacs_table_dir_name = "conv/henke/";

//HydrogenPhotoAbsCS Hydrogen_shell_PACS;
//SimpleAtomPhotoAbsCS Hydrogen_PACS(1, Hydrogen_shell_PACS);

PhenoPhotoAbsCS Hydrogen_for_H2_shell_PACS("Hydrogen_for_H2", 
					   1, 15.43e-6, 3.228);
//1, 16.4e-6, 3.228);
PhenoPhotoAbsCS Hydrogen_for_CH4_shell_PACS("Hydrogen_for_CH4", 
					    1, 12.65e-06, 3.228);
PhenoPhotoAbsCS Hydrogen_for_NH4_shell_PACS("Hydrogen_for_NH4", 
					    1, 10.0e-06, 3.228);

SimpleAtomPhotoAbsCS Hydrogen_for_H2_PACS(1, Hydrogen_for_H2_shell_PACS);
SimpleAtomPhotoAbsCS Hydrogen_for_CH4_PACS(1, Hydrogen_for_CH4_shell_PACS);
SimpleAtomPhotoAbsCS Hydrogen_for_NH4_PACS(1, Hydrogen_for_NH4_shell_PACS);

//ExAtomPhotoAbsCS Hydrogen_for_H2_PACS(1, 
//				      shelllist_dir_name + "shelllist.dat", 
//				      pacs_table_dir_name + "H.dat"); 
//ExAtomPhotoAbsCS Hydrogen_for_CH4_PACS(1, 
//				       shelllist_dir_name + "shelllist.dat", 
//				       pacs_table_dir_name + "H.dat",
//				       12.0e-06); 
//ExAtomPhotoAbsCS Hydrogen_for_NH4_PACS(1, 
//				       shelllist_dir_name + "shelllist.dat", 
//				       pacs_table_dir_name + "H.dat",
//				       10.0e-06); 


ExAtomPhotoAbsCS Helium_PACS(2, 
			     shelllist_dir_name + "shelllist.dat", 
			     pacs_table_dir_name + "He.dat"); 
ExAtomPhotoAbsCS Lithium_PACS(3, 
			     shelllist_dir_name + "shelllist.dat", 
			     pacs_table_dir_name + "Li.dat"); 
ExAtomPhotoAbsCS Beryllium_PACS(4, 
			     shelllist_dir_name + "shelllist.dat", 
			     pacs_table_dir_name + "Be.dat"); 
ExAtomPhotoAbsCS Boron_PACS(5, 
			    shelllist_dir_name + "shelllist.dat", 
			    pacs_table_dir_name + "B.dat"); 
ExAtomPhotoAbsCS Carbon_PACS(6, 
			     shelllist_dir_name + "shelllist.dat", 
			     pacs_table_dir_name + "C.dat"); 
// for debug, FitBT
//ExAtomPhotoAbsCS Carbon_PACS(6, "carbon",  
//			     shelllist_dir_name + "shelltscf.dat", 2, 0, 0.0);


ExAtomPhotoAbsCS Carbon_for_CH4_PACS(6, 
				     shelllist_dir_name + "shelllist.dat", 
				     pacs_table_dir_name + "C.dat",
				     "C_for_CH4",
				     12.65e-06); 
ExAtomPhotoAbsCS Carbon_for_C2H4_PACS(6, 
				     shelllist_dir_name + "shelllist.dat", 
				     pacs_table_dir_name + "C.dat",
				     "C_for_C2H4",
				     10.51e-06); 
ExAtomPhotoAbsCS Carbon_for_C2H6_PACS(6, 
				     shelllist_dir_name + "shelllist.dat", 
				     pacs_table_dir_name + "C.dat",
				     "C_for_C2H6",
				     11.52e-06); 
ExAtomPhotoAbsCS Carbon_for_C4H10_PACS(6, 
				     shelllist_dir_name + "shelllist.dat", 
				     pacs_table_dir_name + "C.dat",
				     "C_for_C4H10",
				     10.55e-06); 
ExAtomPhotoAbsCS Carbon_for_Methylal_PACS
(6, 
 shelllist_dir_name + "shelllist.dat", 
 pacs_table_dir_name + "C.dat",
 "C_for_Methylal", 10.0e-06); 
ExAtomPhotoAbsCS Carbon_for_CF4_PACS(6, 
				     shelllist_dir_name + "shelllist.dat", 
				     pacs_table_dir_name + "C.dat",
				     "C_for_CF4",
				     16.23e-06); 
ExAtomPhotoAbsCS Carbon_for_CO2_PACS(6, 
				     shelllist_dir_name + "shelllist.dat", 
				     pacs_table_dir_name + "C.dat",
				     "C_for_CO2",
				     13.79e-06); 
ExAtomPhotoAbsCS Nitrogen_PACS(7, 
			       shelllist_dir_name + "shelllist.dat", 
			       pacs_table_dir_name + "N.dat"); 
ExAtomPhotoAbsCS Oxygen_PACS(8, 
			     shelllist_dir_name + "shelllist.dat", 
			     pacs_table_dir_name + "O.dat"); 
ExAtomPhotoAbsCS Oxygen_for_CO2_PACS(8,
           shelllist_dir_name + "shelllist.dat",
           pacs_table_dir_name + "O.dat");
ExAtomPhotoAbsCS Fluorine_PACS(9, 
			     shelllist_dir_name + "shelllist.dat", 
			     pacs_table_dir_name + "F.dat"); 
ExAtomPhotoAbsCS Neon_PACS(10, 
			   shelllist_dir_name + "shelllist.dat", 
			   pacs_table_dir_name + "Ne.dat"); 
ExAtomPhotoAbsCS Sodium_PACS(11, 
			     shelllist_dir_name + "shelllist.dat", 
			     pacs_table_dir_name + "Na.dat"); 
ExAtomPhotoAbsCS Magnesium_PACS(12, 
				shelllist_dir_name + "shelllist.dat", 
				pacs_table_dir_name + "Mg.dat"); 
ExAtomPhotoAbsCS Aluminium_PACS(13, 
				shelllist_dir_name + "shelllist.dat", 
				pacs_table_dir_name + "Al.dat"); 
ExAtomPhotoAbsCS Silicon_PACS(14, 
			      shelllist_dir_name + "shelllist.dat", 
			      pacs_table_dir_name + "Si.dat"); 
ExAtomPhotoAbsCS Silicon_crystal_PACS(14, 
				     shelllist_dir_name + "shelllist.dat", 
				     pacs_table_dir_name + "Si.dat",
				     "Si_crystal",
				     1.12e-06);
ExAtomPhotoAbsCS Phosphor_PACS(15, 
			     shelllist_dir_name + "shelllist.dat", 
			     pacs_table_dir_name + "P.dat"); 
ExAtomPhotoAbsCS Sulfur_PACS(16, 
			     shelllist_dir_name + "shelllist.dat", 
			     pacs_table_dir_name + "S.dat"); 
ExAtomPhotoAbsCS Chlorine_PACS(17, 
			       shelllist_dir_name + "shelllist.dat", 
			       pacs_table_dir_name + "Cl.dat"); 
// "Standard" Argon:
//ExAtomPhotoAbsCS Argon_PACS(18, 
//			    shelllist_dir_name + "shelllist.dat", 
//			    pacs_table_dir_name + "Ar.dat"); 
// Optional variants:
//ExAtomPhotoAbsCS Argon_PACS(18, 
//			    shelllist_dir_name + "shelllist.dat", 
//			    shelllist_dir_name + "mw3.dat"); 
// Variant for debug, pointwise cross section
//ExAtomPhotoAbsCS Argon_PACS(18, "argon", 
//			    shelllist_dir_name + "ftbf18.dat", 2);
// Variant for debug, fitted cross section
//ExAtomPhotoAbsCS Argon_PACS(18, "argon", 
//			    shelllist_dir_name + "shelltscf.dat", 2, 0, 0.0);
// Variant for debug, fitted cross section with replacement from Henke
//ExAtomPhotoAbsCS Argon_PACS(18, "argon", 
//			    shelllist_dir_name + "shelltscf.dat", 
//			    pacs_table_dir_name + "Ar.dat",
//			    40.0e-6, 
//			    2, 0.0);
// Another variant for debug, fitted cross section with replacement from 
// Marr and West, should be similar to old Fortran verion
//ExAtomPhotoAbsCS Argon_PACS(18, "argon", 
//			    shelllist_dir_name + "shelltscf.dat", 
//			    shelllist_dir_name + "mw3.dat",
//			    40.0e-6, 
//			    2, 0.0);

ExAtomPhotoAbsCS generate_Argon_PACS_mod_esc(void)
{
  mfunnamep("ExAtomPhotoAbsCS generate_Argon_PACS_mod_esc(void)");
  ExAtomPhotoAbsCS Argon_PACS_mod_esc(18, 
                                      shelllist_dir_name + "shelllist.dat", 
                                      pacs_table_dir_name + "Ar.dat");
 
  // ExAtomPhotoAbsCS Argon_PACS_mod_esc(18, 
  //                                     shelllist_dir_name + "shelllist.dat", 
  //                                     shelllist_dir_name + "mw3.dat"); 

  // ExAtomPhotoAbsCS Argon_PACS_mod_esc(18, "argon", 
  //                                     shelllist_dir_name + "ftbf18.dat", 2);

  AtomicSecondaryProducts* asp =  Argon_PACS_mod_esc.get_asp(1);
  //asp->print(mcout, 2);
  DynLinArr< double > electron_energy;
  DynLinArr< double > photon_energy;
  //electron_energy.put_qel(1);
  //electron_energy[0] = 0.002670;
  electron_energy.put_qel(1);
  electron_energy[0] = 0.000200;
  asp->add_channel(0.65, electron_energy, photon_energy);
  electron_energy.put_qel(2);
  electron_energy[0] = 0.000050;
  electron_energy[1] = 0.000200;
  asp->add_channel(0.35, electron_energy, photon_energy, 1);
  //mcout<<"L1:\n";
  //asp->print(mcout, 2);

  asp =  Argon_PACS_mod_esc.get_asp(2);
  electron_energy.put_qel(1);
  electron_energy[0] = 0.000200;
  asp->add_channel(1.0, electron_energy, photon_energy, 1);
  //mcout<<"L2:\n";
  //asp->print(mcout, 2);

  asp =  Argon_PACS_mod_esc.get_asp(3);
  electron_energy.put_qel(1);
  electron_energy[0] = 0.000200;
  asp->add_channel(1.0, electron_energy, photon_energy, 1);
  //mcout<<"L3:\n";
  //asp->print(mcout, 2);
  
  return Argon_PACS_mod_esc;
}

ExAtomPhotoAbsCS Argon_PACS = generate_Argon_PACS_mod_esc();

ExAtomPhotoAbsCS Germanium_PACS(32, shelllist_dir_name + "shelllist.dat",
                                pacs_table_dir_name + "Ge.dat");
ExAtomPhotoAbsCS Germanium_crystal_PACS(32, 
                                        shelllist_dir_name + "shelllist.dat", 
                                        pacs_table_dir_name + "Ge.dat",
				                                "Si_crystal",
				                                0.67e-06);

ExAtomPhotoAbsCS Bromine_PACS(35, shelllist_dir_name + "shelllist.dat",
                              pacs_table_dir_name + "Br.dat");
ExAtomPhotoAbsCS Krypton_PACS(36, 
			      shelllist_dir_name + "shelllist.dat", 
			      pacs_table_dir_name + "Kr.dat"); 
ExAtomPhotoAbsCS Xenon_PACS(54, 
			    shelllist_dir_name + "shelllist.dat", 
			    pacs_table_dir_name + "Xe.dat");
ExAtomPhotoAbsCS Caesium_PACS(55, shelllist_dir_name + "shelllist.dat",
                              pacs_table_dir_name + "Cs.dat");
ExAtomPhotoAbsCS Mercury_PACS(80, shelllist_dir_name + "shelllist.dat",
                              pacs_table_dir_name + "Hg.dat");
ExAtomPhotoAbsCS Uranium_PACS(92, 
			      shelllist_dir_name + "shelllist.dat", 
			      pacs_table_dir_name + "U.dat"); 


MolecPhotoAbsCS H2_MPACS(Hydrogen_for_H2_PACS, 2);
MolecPhotoAbsCS He_MPACS(Helium_PACS, 1, 41.3e-6);
MolecPhotoAbsCS N2_MPACS(Nitrogen_PACS, 2, 34.8e-6);
MolecPhotoAbsCS O2_MPACS(Oxygen_PACS, 2, 30.8e-6);
MolecPhotoAbsCS Ne_MPACS(Neon_PACS, 1, 35.4e-6);
MolecPhotoAbsCS Ar_MPACS(Argon_PACS, 1, 26.4e-6);
MolecPhotoAbsCS Kr_MPACS(Krypton_PACS, 1, 24.4e-6);
MolecPhotoAbsCS Xe_MPACS(Xenon_PACS, 1, 22.1e-6);
MolecPhotoAbsCS NH3_MPACS(Nitrogen_PACS, 1, Hydrogen_for_NH4_PACS, 3, 26.6e-6);
MolecPhotoAbsCS N2O_MPACS(Nitrogen_PACS, 2, Oxygen_PACS, 1, 34.8e-6);
MolecPhotoAbsCS CO2_MPACS(Carbon_for_CO2_PACS, 1, Oxygen_for_CO2_PACS, 2, 33.0e-6);
MolecPhotoAbsCS CH4_MPACS(Carbon_for_CH4_PACS, 1, Hydrogen_for_H2_PACS, 4, 
			  27.3e-6);
//MolecPhotoAbsCS CH4_MPACS(Carbon_for_CH4_PACS, 1, Hydrogen_for_CH4_PACS, 4);
MolecPhotoAbsCS CF4_MPACS(Carbon_for_CF4_PACS, 1, Fluorine_PACS, 4);

// !!! The following line may need to be refined 
// (to adjust outer shell energies).
MolecPhotoAbsCS SF4_MPACS(Sulfur_PACS, 1, Fluorine_PACS, 4);
// !!! The following line may need to be refined 
// (to adjust outer shell energies).
MolecPhotoAbsCS SF6_MPACS(Sulfur_PACS, 1, Fluorine_PACS, 6);

MolecPhotoAbsCS C2H2_MPACS(Carbon_for_CH4_PACS, 2, Hydrogen_for_H2_PACS, 2,
			   25.8e-6);
MolecPhotoAbsCS C2H4_MPACS(Carbon_for_C2H4_PACS, 2, Hydrogen_for_H2_PACS, 4,
			   25.8e-6);
MolecPhotoAbsCS C2H6_MPACS(Carbon_for_C2H6_PACS, 2, Hydrogen_for_H2_PACS, 6,
			   25.0e-6);
MolecPhotoAbsCS C3H8_MPACS(Carbon_for_CH4_PACS, 3, Hydrogen_for_H2_PACS, 8,
			   24.0e-6);
MolecPhotoAbsCS C4H10_MPACS(Carbon_for_C4H10_PACS, 4, Hydrogen_for_H2_PACS, 10,
			    23.4e-6);

// !!! The following line may need to be refined 
// (to adjust outer shell energies).
MolecPhotoAbsCS C2F4H2_MPACS(Carbon_for_CF4_PACS, 2, 
			     Fluorine_PACS, 4,
			     Hydrogen_for_H2_PACS, 2);

//MolecPhotoAbsCS C2H2_MPACS(Carbon_for_CH4_PACS, 2, Hydrogen_for_CH4_PACS, 2);
//MolecPhotoAbsCS C2H4_MPACS(Carbon_for_CH4_PACS, 2, Hydrogen_for_CH4_PACS, 4);
//MolecPhotoAbsCS C2H6_MPACS(Carbon_for_CH4_PACS, 2, Hydrogen_for_CH4_PACS, 6);
//MolecPhotoAbsCS C3H8_MPACS(Carbon_for_CH4_PACS, 3, Hydrogen_for_CH4_PACS, 8);
//MolecPhotoAbsCS C4H10_MPACS(Carbon_for_CH4_PACS, 4, Hydrogen_for_CH4_PACS, 10);
MolecPhotoAbsCS Methylal_MPACS(Oxygen_PACS, 2,
			       Carbon_for_Methylal_PACS, 3, 
			       Hydrogen_for_H2_PACS, 8, 
			       10.0e-6 * 23.4 / 10.55);  // similar to C4H10
/*
The value of W for noble gases is slightly less than 
twice the ionization potential.
For organic gases it is very close to mean ionization potential
averaged with taking into account of atomic charges of carbon and hydrogen.
and assuming that the ionization potential of the hydrogen is the same
as in pure molecular hydrogen H2. 
*/

// Additional molecular photoabsorption-cross sections 
// for compatibility with Magboltz
// W values (where available) are taken from ICRU report 31
MolecPhotoAbsCS C5H12_MPACS(Carbon_for_C4H10_PACS, 5, 
                            Hydrogen_for_H2_PACS, 
                            12, 23.2e-6);
MolecPhotoAbsCS H2O_MPACS(Hydrogen_for_H2_PACS, 2,
                    Oxygen_PACS, 1, 
                    29.6e-6);
MolecPhotoAbsCS NO_MPACS(Nitrogen_PACS, 1,
                   Oxygen_PACS, 1);
MolecPhotoAbsCS CO_MPACS(Carbon_for_CO2_PACS, 1,
                   Oxygen_PACS, 1);
MolecPhotoAbsCS DME_MPACS(Carbon_for_Methylal_PACS, 2,
                    Hydrogen_for_H2_PACS, 6,
                    Oxygen_PACS, 1);
MolecPhotoAbsCS C2F6_MPACS(Carbon_for_C2H6_PACS, 2,
                     Fluorine_PACS, 6);
MolecPhotoAbsCS C3H6_MPACS(Carbon_for_C2H6_PACS, 3,
                     Hydrogen_for_H2_PACS, 6);
MolecPhotoAbsCS CH3OH_MPACS(Carbon_for_C2H6_PACS, 1,
                            Hydrogen_for_H2_PACS, 4,
                            Oxygen_PACS, 1, 
                            24.7e-6);
MolecPhotoAbsCS C2H5OH_MPACS(Carbon_for_C2H6_PACS, 2,
                             Hydrogen_for_H2_PACS, 6,
                             Oxygen_PACS, 1,
                             24.8e-6);
MolecPhotoAbsCS C3H7OH_MPACS(Carbon_for_C2H6_PACS, 3,
                             Hydrogen_for_H2_PACS, 8,
                             Oxygen_PACS, 1);
MolecPhotoAbsCS Cs_MPACS(Caesium_PACS, 1);
MolecPhotoAbsCS F2_MPACS(Fluorine_PACS, 2);
MolecPhotoAbsCS CS2_MPACS(Carbon_for_CO2_PACS, 1,
                          Sulfur_PACS, 2);
MolecPhotoAbsCS COS_MPACS(Carbon_for_CO2_PACS, 1,
                          Oxygen_PACS, 1,
                          Sulfur_PACS, 1);
MolecPhotoAbsCS BF3_MPACS(Boron_PACS, 1,
                          Fluorine_PACS, 3);
MolecPhotoAbsCS C2HF5_MPACS(Carbon_for_C2H6_PACS, 2,
                            Hydrogen_for_H2_PACS, 1,
                            Fluorine_PACS, 5);
MolecPhotoAbsCS CHF3_MPACS(Carbon_for_CF4_PACS, 1,
                           Hydrogen_for_H2_PACS, 1,
                           Fluorine_PACS, 3);
MolecPhotoAbsCS CF3Br_MPACS(Carbon_for_CF4_PACS, 1,
                            Fluorine_PACS, 3,
                            Bromine_PACS, 1);
MolecPhotoAbsCS C3F8_MPACS(Carbon_for_CF4_PACS, 3,
                           Fluorine_PACS, 8);
MolecPhotoAbsCS O3_MPACS(Oxygen_PACS, 3);
MolecPhotoAbsCS Hg_MPACS(Mercury_PACS, 1);
MolecPhotoAbsCS H2S_MPACS(Hydrogen_for_H2_PACS, 2,
                          Sulfur_PACS, 1);
MolecPhotoAbsCS GeH4_MPACS(Germanium_PACS, 1,
                           Hydrogen_for_H2_PACS, 4);
MolecPhotoAbsCS SiH4_MPACS(Silicon_PACS, 1,
                           Hydrogen_for_H2_PACS, 4);





                  

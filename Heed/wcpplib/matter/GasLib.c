#include "wcpplib/matter/GasLib.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"
/*
1998-2003,  I. Smirnov
*/

//AbsList< AtomDef* > AtomDef::logbook;
// This call should be before all atom_def
// otherwise they are all lost.

AtomDef Hydrogen(  "Hydrogen",   "H",   1, 1.0 * gram/mole);
//AtomDef Hydrogen(  "Hydrogen",   "H",   1, 1.00794 * gram/mole);
AtomDef Helium(    "Helium",     "He",  2, 4.002602 * gram/mole);
AtomDef Lithium(   "Lithium",    "Li",  3, 6.941 * gram/mole);
AtomDef Beryllium( "Beryllium",  "Be",  4, 9.012182 * gram/mole);
AtomDef Boron(     "Boron",      "B",   5, 10.811 * gram/mole);
AtomDef Carbon(    "Carbon",     "C",   6, 12.011 * gram/mole);
AtomDef Nitrogen(  "Nitrogen",   "N",   7, 14.00674 * gram/mole);
AtomDef Oxygen(    "Oxygen",     "O",   8, 15.9994 * gram/mole);
AtomDef Fluorine(  "Fluorine",   "F",   9, 18.9984032 * gram/mole);
AtomDef Neon(      "Neon",       "Ne", 10, 20.1797 * gram/mole);
AtomDef Sodium(    "Sodium",     "Na", 11, 22.989768 * gram/mole);
AtomDef Magnesium( "Magnesium",  "Mg", 12, 24.3050 * gram/mole);
AtomDef Aluminium( "Aluminium",  "Al", 13, 26.981539 * gram/mole);
AtomDef Silicon(   "Silicon",    "Si", 14, 28.0855 * gram/mole);
AtomDef Phosphor(  "Phosphor",   "P",  15, 30.973762 * gram/mole);
AtomDef Sulfur(    "Sulfur",     "S",  16, 32.066 * gram/mole);
AtomDef Chlorine(  "Chlorine",   "Cl", 17, 35.066 * gram/mole);
AtomDef Argon(     "Argon",      "Ar", 18, 39.948 * gram/mole);
AtomDef Argon_without_K
(             "Argon_without_K",  "Ar_without_K", 16, 39.948 * gram/mole);
AtomDef Potassium( "Potassium",  "K",  19, 39.098 * gram/mole);  // check
AtomDef Calcium(   "Calcium",    "Ca", 20, 40.08 * gram/mole);   // check
AtomDef Scandium(  "Scandium",   "Sc", 21, 44.9559 * gram/mole);   // check
AtomDef Titanium(  "Titanium",   "Ti", 22, 47.90 * gram/mole);   // check
AtomDef Vanadium(  "Vanadium",   "V",  23, 50.9414 * gram/mole);   // check
AtomDef Chromium(  "Chromium",   "Cr", 24, 51.996 * gram/mole);   // check
AtomDef Manganese( "Manganese",  "Mn", 25, 54.9380 * gram/mole);   // check
AtomDef Ferum(     "Ferum",      "Fe", 26, 55.847 * gram/mole);   // check
AtomDef Cobalt(    "Cobalt",     "Co", 27, 58.9332 * gram/mole);   // check
AtomDef Nickel(    "Niclel",     "Ni", 28, 58.70 * gram/mole);   // check
AtomDef Cuprum(    "Cuprum",     "Cu", 29, 63.546 * gram/mole);
AtomDef Zinc(      "Zinc",       "Zn", 30, 65.38 * gram/mole);   // check
AtomDef Gallium(   "Gallium",    "Ga", 31, 69.72 * gram/mole);   // check
AtomDef Germanium( "Germanium",  "Ge", 32, 72.59 * gram/mole);   // check
AtomDef Arsenic(   "Arsenic",    "As", 33, 74.9216 * gram/mole);   // check
AtomDef Selenium(  "Selenium",   "Se", 34, 78.96 * gram/mole);   // check
AtomDef Barium(    "Barium",     "Br", 35, 79.904 * gram/mole);   // check
AtomDef Krypton(   "Krypton",    "Kr", 36, 83.80 * gram/mole);
AtomDef Rubidium(  "Rubidium",   "Rb", 37, 85.4673 * gram/mole);  // check
AtomDef Strontium( "Strontium",  "Sr", 38, 87.62 * gram/mole);    // check
AtomDef Yttrium(   "Yttrium",    "Y",  39, 88.9059 * gram/mole);    // check
AtomDef Zirconium( "Zirconium",  "Zr", 40, 91.22 * gram/mole);    // check
AtomDef Niobium(   "Niobium",    "Nb", 41, 92.9064 * gram/mole);    // check
AtomDef Molibdenum("Molibdenum", "Mo", 42, 95.94 * gram/mole);    // check
// Find English name of tehnecii
AtomDef Ruthenium( "Ruthenium",  "Ru", 44, 101.07 * gram/mole);    // check
AtomDef Rhodium(   "Rhodium",    "Rh", 45, 102.9055 * gram/mole);    // check
AtomDef Palladium( "Palladium",  "Pd", 46, 106.4 * gram/mole);    // check
AtomDef Silver(    "Silver",     "Ag", 47, 107.868 * gram/mole);    // check
 
AtomDef Iodine(    "Iodine",     "I",  53, 126.9045 * gram/mole);    // check

AtomDef Xenon(     "Xenon",      "Xe", 54, 131.29 * gram/mole);
AtomDef Caesium(   "Caesium",    "Cs", 55, 132.9054519 * gram/mole);

AtomDef Tungsten(  "Tungsten",   "W",  74, 183.85 * gram/mole);    // check

AtomDef Mercury(   "Mercury",    "Hg", 80, 200.59 * gram/mole);

AtomDef Bismuth(   "Bismuth",    "Bi", 83, 208.9804 * gram/mole);    // check

AtomDef Uranium(   "Uranium",    "U",  92, 238.0289 * gram/mole);
AtomDef Plutonium(  "Plutonium", "Pu", 94, 244.0 * gram/mole);

//AbsList< MoleculeDef* > MoleculeDef::logbook;
// This call should be before all MoleculeDef
// otherwise they are all lost.


MoleculeDef Hydrogen2(  "Hydrogen",   "H2",   "H",  2);
MoleculeDef Helium_molec(  "Helium",     "He", "He", 1);
MoleculeDef Nitrogen_molec(  "Nitrogen",   "N2",   "N",  2);
MoleculeDef Oxygen_molec(  "Oxygen",   "O2",   "O",  2);
MoleculeDef Neon_molec(  "Neon",     "Ne", "Ne", 1);

VanDerVaals Argon_VanDerVaals(48.6 * bar, 150.7*kelvin);
MoleculeDef Argon_molec
(  "Argon",     "Ar", "Ar", 1, 
   ActivePtr< VanDerVaals >(&Argon_VanDerVaals, do_clone));
//MoleculeDef Argon_molec(  "Argon",     "Ar", "Ar", 1, &Argon_VanDerVaals);
//MoleculeDef Argon_without_K_molec(  "Argon_without_K",     
//				    "Ar_without_K", "Ar_without_K", 1);

VanDerVaals Krypton_VanDerVaals(55.0 * bar, 209.4*kelvin);
MoleculeDef Krypton_molec
(  "Krypton",    "Kr", "Kr", 1, 
   ActivePtr< VanDerVaals >(&Krypton_VanDerVaals, do_clone));

VanDerVaals Xenon_VanDerVaals(55.0 * bar, 209.4*kelvin);
MoleculeDef Xenon_molec
(  "Xenon",     "Xe", "Xe", 1, 
   ActivePtr< VanDerVaals >(&Xenon_VanDerVaals, do_clone));


MoleculeDef NH3( "NH3", "NH3", "N", 1, "H", 3);
MoleculeDef N2O( "N2O", "N2O", "N", 2, "O", 1);
MoleculeDef CO2( "CO2", "CO2", "C", 1, "O", 2);

VanDerVaals CH4_VanDerVaals(4.64e+6 * hep_pascal, (273.15 - 82.5)*kelvin);
MoleculeDef CH4
   ( "CH4", "CH4", "C", 1, "H", 4, 
     ActivePtr< VanDerVaals >(&CH4_VanDerVaals, do_clone));
//MoleculeDef CH4( "CH4", "CH4", "C", 1, "H", 4);

VanDerVaals CF4_VanDerVaals(42.5 * bar, 369.8*kelvin);
MoleculeDef CF4
( "CF4", "CF4", "C", 1, "F", 4, 
  ActivePtr< VanDerVaals >(&CF4_VanDerVaals, do_clone));
//MoleculeDef CF4( "CF4", "CF4", "C", 1, "F", 4);

MoleculeDef SF4( "SF4", "SF4", "S", 1, "F", 4);
MoleculeDef SF6( "SF6", "SF6", "S", 1, "F", 6);

MoleculeDef C2H2( "C2H2", "C2H2", "C", 2, "H", 2);
MoleculeDef C2H4( "C2H4", "C2H4", "C", 2, "H", 4);
MoleculeDef C2H6( "C2H6", "C2H6", "C", 2, "H", 6);

VanDerVaals C3H8_VanDerVaals(42.5 * bar, 369.8*kelvin);
MoleculeDef C3H8
( "C3H8", "C3H8", "C", 3, "H", 8, 
  ActivePtr< VanDerVaals >(&C3H8_VanDerVaals, do_clone));
//MoleculeDef C3H8( "C3H8", "C3H8", "C", 3, "H", 8);

VanDerVaals C4H10_VanDerVaals(40.0 * bar, 418.3*kelvin);
MoleculeDef C4H10
( "C4H10", "C4H10", "C", 4, "H", 10, 
  ActivePtr< VanDerVaals >(&C4H10_VanDerVaals, do_clone));
//MoleculeDef C4H10( "C4H10", "C4H10", "C", 4, "H", 10);

MoleculeDef C2F4H2( "C2F4H2", "C2F4H2", "C", 2, "F", 4, "H", 2);

VanDerVaals Water_VanDerVaals(22.9e+6 * hep_pascal, (273.15 + 374.15)*kelvin);
MoleculeDef Water_molec
( "Water", "Water", "O", 1, "H", 2, 
  ActivePtr< VanDerVaals >(&Water_VanDerVaals, do_clone));

VanDerVaals Methylal_VanDerVaals(39.5 * bar, 480.6*kelvin);
MoleculeDef Methylal_molec
( "Methylal", "Methylal", "O", 2, "C", 3, "H", 8, 
  ActivePtr< VanDerVaals >(&Methylal_VanDerVaals, do_clone));

//AbsList< MatterDef* > MatterDef::logbook;
// This call should be before all MatterDef
// otherwise they are all lost.

// Additional molecule definitions for compatibility with Magboltz
MoleculeDef C5H12_molec("C5H12", "C5H12", "C", 5, "H", 12);
MoleculeDef NO_molec("NO", "NO", "N", 1, "O", 1);
MoleculeDef CO_molec("CO", "CO", "C", 1, "O", 1);
MoleculeDef DME_molec("DME", "DME", "C", 2, "H", 6, "O", 1);
MoleculeDef C2F6_molec("C2F6", "C2F6", "C", 2, "F", 6);
MoleculeDef C3H6_molec("C3H6", "C3H6", "C", 3, "H", 6);
MoleculeDef CH3OH_molec("CH3OH", "CH3OH", "C", 1, "H", 4, "O", 1);
MoleculeDef C2H5OH_molec("C2H5OH", "C2H5OH", "C", 2, "H", 6, "O", 1);
MoleculeDef C3H7OH_molec("C3H7OH", "C3H7OH", "C", 3, "H", 8, "O", 1);
MoleculeDef Cs_molec("Cs", "Cs", "Cs", 1);
MoleculeDef F2_molec("F2", "F2", "F", 2);
MoleculeDef CS2_molec("CS2", "CS2", "C", 1, "S", 2);
MoleculeDef COS_molec("COS", "COS", "C", 1, "O", 1, "S", 1);
MoleculeDef BF3_molec("BF3", "BF3", "B", 1, "F", 3);
MoleculeDef C2HF5_molec("C2HF5", "C2HF5", "C", 2, "H", 1, "F", 5);
MoleculeDef CHF3_molec("CHF3", "CHF3", "C", 1, "H", 1, "F", 3);
MoleculeDef CF3Br_molec("CF3Br", "CF3Br", "C", 1, "F", 3, "Br", 1);
MoleculeDef C3F8_molec("C3F8", "C3F8", "C", 3, "F", 8);
MoleculeDef O3_molec("O3", "O3", "O", 3);
MoleculeDef Hg_molec("Hg", "Hg", "Hg", 1);
MoleculeDef H2S_molec("H2S", "H2S", "H", 2, "S", 1);
MoleculeDef GeH4_molec("GeH4", "GeH4", "Ge", 1, "H", 4);
MoleculeDef SiH4_molec("SiH4", "SiH4", "Si", 1, "H", 4); 

double todays_temperature=293.0*kelvin; // room temterature, 
                                         // 20 degrees Celsium
//double todays_temperature=293.15*kelvin; // room temterature, 
//                                         // 20 degrees Celsium
//double todays_temperature=296.15*kelvin; // anoter room temterature, 
                                         // 23 degrees Celsium

double todays_pressure=1.0 * atmosphere;   // sea level
//double todays_pressure=1.0 * atmosphere * 960.0/1000.0;   // level of CERN
// By different information the altitude is about 400 meters and
// the slope of pressure is about 1 bar per 10 m (somebody says - 12 m). 
// But the actual pressure depends on weather, of course! 

//double todays_pressure=730.0/760.0 * atmosphere;   
// level for one experiment (looks like in CERN as well)

/*
GasDef GasHelium("He", "He", "He",  
		todays_pressure, todays_temperature);
GasDef GasNeon("Ne", "Ne", "Ne",  
		todays_pressure, todays_temperature);
GasDef GasArgon("Ar", "Ar", "Ar",  
		todays_pressure, todays_temperature);
GasDef GasKrypton("Kr", "Kr", "Kr",  
		todays_pressure, todays_temperature);
GasDef GasXenon("Xe", "Xe", "Xe",  
		todays_pressure, todays_temperature);
GasDef GasN2("N2", "N2", "N2",  
		todays_pressure, todays_temperature);
GasDef GasO2("O2", "O2", "O2",  
		todays_pressure, todays_temperature);
GasDef GasNH3("NH3", "NH3", "NH3",  
		todays_pressure, todays_temperature);
GasDef GasN2O("N2O", "N2O", "N2O",  
		todays_pressure, todays_temperature);
GasDef GasCO2("CO2", "CO2", "CO2",  
		todays_pressure, todays_temperature);
GasDef GasCF4("CF4", "CF4", "CF4",  
		todays_pressure, todays_temperature);

GasDef GasCH4("CH4", "CH4", "CH4",  
		todays_pressure, todays_temperature);
GasDef GasC2H2("C2H2", "C2H2", "C2H2",  
		todays_pressure, todays_temperature);
GasDef GasC2H4("C2H4", "C2H4", "C2H4",  
		todays_pressure, todays_temperature);
GasDef GasC2H6("C2H6", "C2H6", "C2H6",  
		todays_pressure, todays_temperature);
GasDef GasC3H8("C3H8", "C3H8", "C3H8",  
		todays_pressure, todays_temperature);
GasDef GasC4H10("C4H10", "C4H10", "C4H10",  
		todays_pressure, todays_temperature);
//*/

//GasDef Air("Air", "Air", "N2", 0.7, "O2", 0.3, 
//	   todays_pressure, todays_temperature);
//DynLinArr< String > fmolec_not = {
// according to "short reference book of chemist", B.I.Perelman, Moskow, 1963.

// String  smolec_not[6] = {String("N2"), String("O2"), String("Ar"), 
// 			  String("CO2"), String("Ne"), String("He")}; 
// double sweight_quan_molec[6] = 
// {0.7809, 0.2095, 0.009325, 0.0003, 0.000018, 0.000005};
// DynLinArr< String > fmolec_not(6, smolec_not, ArgInterp_Arr()); 
// DynLinArr< double > fweight_quan_molec(6, sweight_quan_molec, 
// 				       ArgInterp_Arr()); 

// GasDef Air("Air", "Air", 6, fmolec_not, fweight_quan_molec,
// 	   todays_pressure, todays_temperature);

// GasDef GasHydrogen2("GasHydrogen2", "H2", "H2",  
// 		todays_pressure, todays_temperature);

/*
GasDef LiquidHydrogen("LiquidHydrogen", "LiqH2", "H2",  
		      todays_pressure, todays_temperature, 0.0708*gram/cm3);

MatterDef CoverMatterForSpes4pi("CoverMatterForSpes4pi",
				"CoverMatterForSpes4pi",
				"C", 2,
				"H", 3,
				"Cl", 1,
				0.0759*gram/cm3, todays_temperature);
*/

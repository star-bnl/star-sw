#ifndef GASLIB_H
#define GASLIB_H

/* 
There are not only gases but also atoms and molecules.
The gas is the highest class in this hierarhy.


The reason to gather all initializations in one file is the undefinite
order of initialization of the global objects in C++.
This order in definite only for objects presented in single object file.
Since each atom, molecula, and matter should be registered in the system,
the logbooks should be initialized and ready for work prior to
initialization of the first registered object. 
This is now guaranteed with the help of singleton class.
But moleculas and matters depend on atoms, and the order of initialization
should be kept strongly.
The only way to guarantee
this is to initialize all the ingredients in the same file  in correct order.
For not global objects there is no problem,
since global static logbooks are anyway initialized before running main().

Therefore the definitions "extern ..." are gathered in this file,
the corresponding initializations followed by initializations of
logbooks are gathered in GasLib.c. This latter file is not put into
a library file, but just compiled. 
If the user program need to initialize other global objects like these, or
objects depending on them, it has to gather all the additional
initializations in another single file and include
include the file GasLib.c into its beginning  by "#include" directive.
It should not be linked with GasLib.o, in this case.
If the user program does not have mantioned needs, it can just be linked
with the existing GasLib.o.

The last half of Mendeleev's (Mendel's) table is not totally included yet.


1998-2004,  I. Smirnov

*/

#include "wcpplib/matter/AtomDef.h"
#include "wcpplib/matter/MatterDef.h"
#include "wcpplib/matter/MoleculeDef.h"
#include "wcpplib/matter/GasDef.h"



extern AtomDef Hydrogen; 
extern AtomDef Helium;
extern AtomDef Lithium;
extern AtomDef Beryllium;
extern AtomDef Boron;
extern AtomDef Carbon;
extern AtomDef Nitrogen;
extern AtomDef Oxygen;
extern AtomDef Fluorine;
extern AtomDef Neon;
extern AtomDef Sodium;
extern AtomDef Magnesium;
extern AtomDef Aluminium;
extern AtomDef Silicon;
extern AtomDef Phosphor;
extern AtomDef Sulfur;
extern AtomDef Chlorine;
extern AtomDef Argon;
extern AtomDef Argon_without_K;  // without K-shell
extern AtomDef Potassium;
extern AtomDef Calcium;
extern AtomDef Scandium;
extern AtomDef Titanium;
extern AtomDef Vanadium;
extern AtomDef Chromium;
extern AtomDef Manganese;
extern AtomDef Ferum;
extern AtomDef Cobalt;
extern AtomDef Nickel;
extern AtomDef Cuprum;
extern AtomDef Zinc;
extern AtomDef Gallium;
extern AtomDef Germanium;
extern AtomDef Arsenic;
extern AtomDef Selenium;
extern AtomDef Barium;
extern AtomDef Krypton;
extern AtomDef Rubidium;
extern AtomDef Strontium;
extern AtomDef Yttrium;
extern AtomDef Zirconium;
extern AtomDef Niobium;
extern AtomDef Molibdenum;
extern AtomDef Ruthenium;
extern AtomDef Rhodium;
extern AtomDef Palladium;
extern AtomDef Silver;
extern AtomDef Iodine;
extern AtomDef Xenon;
extern AtomDef Caesium;
extern AtomDef Tungsten;
extern AtomDef Mercury;
extern AtomDef Bismuth;
extern AtomDef Uranium;
extern AtomDef Plutonium;
		  
extern MoleculeDef Hydrogen2; 
extern MoleculeDef Helium_molec;
extern MoleculeDef Nitrogen_molec;
extern MoleculeDef Oxygen_molec;
extern MoleculeDef Neon_molec;
extern VanDerVaals Argon_VanDerVaals;
extern MoleculeDef Argon_molec;
//extern MoleculeDef Argon_without_K_molec;
extern VanDerVaals Krypton_VanDerVaals;
extern MoleculeDef Krypton_molec;
extern VanDerVaals Xenon_VanDerVaals;
extern MoleculeDef Xenon_molec;
extern MoleculeDef NH3;
extern MoleculeDef N2O;
extern MoleculeDef CO2;
extern VanDerVaals CH4_VanDerVaals;
extern MoleculeDef CH4;
extern VanDerVaals CF4_VanDerVaals;
extern MoleculeDef CF4;

// The following is defined without VanDerVaals corrections
extern MoleculeDef SF4;
extern MoleculeDef SF6;

extern MoleculeDef C2H2;
extern MoleculeDef C2H4;
extern MoleculeDef C2H6;
extern VanDerVaals C3H8_VanDerVaals;
extern MoleculeDef C3H8;
extern VanDerVaals C4H10_VanDerVaals;
extern MoleculeDef C4H10;

// The following is defined without VanDerVaals corrections
extern MoleculeDef C2F4H2;

extern VanDerVaals Water_VanDerVaals;
extern MoleculeDef Water_molec;
extern VanDerVaals Methylal_VanDerVaals;
extern MoleculeDef Methylal_molec;

// Additional molecule definitions for compatibility with Magboltz
extern MoleculeDef C5H12_molec;
extern MoleculeDef NO_molec;
extern MoleculeDef CO_molec;
extern MoleculeDef DME_molec;
extern MoleculeDef C2F6_molec;
extern MoleculeDef C3H6_molec;
extern MoleculeDef CH3OH_molec;
extern MoleculeDef C2H5OH_molec;
extern MoleculeDef C3H7OH_molec;
extern MoleculeDef Cs_molec;
extern MoleculeDef F2_molec;
extern MoleculeDef CS2_molec;
extern MoleculeDef COS_molec;
extern MoleculeDef BF3_molec;
extern MoleculeDef C2HF5_molec;
extern MoleculeDef CHF3_molec;
extern MoleculeDef CF3Br_molec;
extern MoleculeDef C3F8_molec;
extern MoleculeDef O3_molec;
extern MoleculeDef Hg_molec;
extern MoleculeDef H2S_molec;
extern MoleculeDef GeH4_molec;
extern MoleculeDef SiH4_molec; 

extern double todays_temperature;  // in internal units
extern double todays_pressure;  // in internal units

extern GasDef GasHelium;
extern GasDef GasNeon;
extern GasDef GasArgon;
extern GasDef GasKrypton;
extern GasDef GasXenon;
extern GasDef GasN2;
extern GasDef GasO2;
extern GasDef GasNH3;
extern GasDef GasN2O;
extern GasDef GasCO2;
extern GasDef GasCF4;
extern GasDef GasCH4;
extern GasDef GasC2H2;
extern GasDef GasC2H4;
extern GasDef GasC2H6;
extern GasDef GasC3H8;
extern GasDef GasC4H10;
extern GasDef Air;
extern GasDef GasHydrogen2;
//extern GasDef LiquidHydrogen;

//extern MatterDef CoverMatterForSpes4pi;

#endif

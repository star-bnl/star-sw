#ifndef MYPHYSICALCONSTANTS_H
#define MYPHYSICALCONSTANTS_H
/*
Here I gathered constants appropriate for internal simulations of heed++.
These constants related to internal system of units traditionally
used in this field and applied in the old versions of HEED.
The old names are preserved.
2003, I. Smirnov.
*/
const double ELMAS =  0.51099906;        // Electron mass (MeV)  
const double FSCON =  137.0359895;       // The fine ctructure constant
const double ELRAD =  1.0/(FSCON*ELMAS); // Electron radius (1/MeV)
const double ELRADCM =  2.81794092e-13;  // Electron radius (cm)
const double C1_MEV_CM = ELRAD/ELRADCM; // Ratio r(1/MeV)/r(cm)
    // or coefficient for passing from x(cm) to x(1/MeV) = 5.07E10
const double C1_MEV2_BN = C1_MEV_CM * C1_MEV_CM / 1.0e24; 
    // coefficient for passing from x(bn) to x(1/MeV^2) = 
    // (5.07E10)^2/(1.0e24)
const double C1_MEV2_MBN = C1_MEV_CM * C1_MEV_CM / 1.0e18; 
    // coefficient for passing from x(mbn) to x(1/MeV^2) = 
    // (5.07E10)^2/(1.0e18)

const double AVOGADRO =  6.0221367e23;
const double PLANK =    6.6260755e-34;  // Plank constant (J*sec)
const double ELCHARGE =  1.60217733e-19; // Electron charge (C)
const double CLIGHT =   2.99792458e10;  // Light vel.(cm/sec)
const double PLANKCLIGHT = 197.327e-13; // crossed plank * CLIGHT (Mev* cm)
// Notes for memory:
// Crossed plank is plank divided by 2pi.
// The length of wave vector k in vacuum in units 1/[length]
// (that is 1/cm in this system)
// is the energy divided by PLANKCLIGHT

#endif

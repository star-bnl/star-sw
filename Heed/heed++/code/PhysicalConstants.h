#ifndef MYPHYSICALCONSTANTS_H
#define MYPHYSICALCONSTANTS_H

#include "wcpplib/clhep_units/WSystemOfUnits.h"

/*
Here I gathered constants appropriate for internal simulations of heed++.
These constants related to internal system of units traditionally
used in this field and applied in the old versions of HEED.
The old names are preserved.
2003, I. Smirnov.
*/

namespace Heed {

/// Electron radius (1/MeV)
constexpr double ELRAD = CLHEP::fine_structure_const / CLHEP::electron_mass_c2;
constexpr double ELRADCM = 2.81794092e-13;     // Electron radius (cm)
constexpr double C1_MEV_CM = ELRAD / ELRADCM;  // Ratio r(1/MeV)/r(cm)
// or coefficient for passing from x(cm) to x(1/MeV) = 5.07E10
constexpr double C1_MEV2_BN = C1_MEV_CM * C1_MEV_CM / 1.0e24;
// coefficient for passing from x(bn) to x(1/MeV^2) =
// (5.07E10)^2/(1.0e24)
constexpr double C1_MEV2_MBN = C1_MEV_CM * C1_MEV_CM / 1.0e18;
// coefficient for passing from x(mbn) to x(1/MeV^2) =
// (5.07E10)^2/(1.0e18)
}

#endif

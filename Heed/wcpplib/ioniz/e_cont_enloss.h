#ifndef E_CONT_ENLOSS_H
#define E_CONT_ENLOSS_H

namespace Heed {

/// Continuous energy loss of electron.
/// Similar to GDRELE from GEANT 3.21
/// 2003,  I. Smirnov
double e_cont_enloss(const double ratio_Z_to_A, const double I_eff,
                     const double density, const double Ekin, const double Ecut,
                     const double z);

// z = -1 (electron) or +1 (positron)
// returns the positive value in units
// [energy] / [length]
// (internal units of CLHEP/Units/PhysicalConstants.h)
// It means the energy transfer per unit density and per unit length passed.
// To convert it to real values it need to multiply it by real density and
// by real length.
//
}

#endif

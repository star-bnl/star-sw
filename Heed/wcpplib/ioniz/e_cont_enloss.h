#ifndef E_CONT_ENLOSS_H
#define E_CONT_ENLOSS_H

/*
Continious energy loss of electron similar to GDRELE from GEANT 3.21

2003,  I. Smirnov
*/

double e_cont_enloss(double ratio_Z_to_A, // do not forget: 
			                  // 1.0/(gram/mole) 
		     double I_eff,// in internal units
		     double density, // in internal units
		     double Ekin, // in internal units
		     double Ecut, // in internal units
		     double z);

// z = -1 (electron) or +1 (positron) 
// returns the positive value in units 
// [energy] / [length] 
// (internal units of CLHEP/Units/PhysicalConstants.h)
// It means the energy transfer per unit density and per unit length passed.
// To convert it to real values it need to multiply it by real density and 
// by real length.
//





#endif

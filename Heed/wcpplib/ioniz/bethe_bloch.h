#ifndef BETHE_H
#define BETHE_H

/*
Ordinary Bete-Bloch formula with various modifications

1998 - 2002,  I. Smirnov
*/
/*
By Russian translated book by (not sure about correct spelling)
K. Klainkneht, "Detectors of carpuscular radiation".
Mir 1990.
*/
double Bethe_Bloch_energy_loss(double ratio_Z_to_A, // do not forget: 
			                      // 1.0/(gram/mole) 
		  // No! seems it was error in comment: 1.0/(gram/mole)
       // Actually it is 1.0/(weight/mole), weight in internal units 
			 double I_eff, 
			 double betta, double z);
// z*e - the charge of ionizing particle
// sign is not important, z is squared inside the function.
// returns the positive value in units 
// [energy] / ([density][length])=[energy]*[length]^2 / [weight]
// (internal units of CLHEP/Units/PhysicalConstants.h)
// It means the energy transfer per unit density and per unit length passed.
// To convert it to real values it need to multiply it by real density and 
// by real length.
//
//       Averaging of ratio_Z_to_A by many atoms:
//       ---------------------------------------
// In 1 gram of matter there are Avogadro/A particles.
// If there are mixture of n atoms.
// If Wi - weight by weights (sum(Wi) = 1),
// in 1 gram of matter we should have 
// sum(Wi * Avogadro/Ai) = Avogadro * mean(1/Ai) particles,
// and sum(Wi * Avogadro* Zi/Ai) = Avogadro * mean(Zi/Ai) electrons.
// Here the notation mean(something) means sum(Wi*something).  
// If Wi - weight by atom numbers (sum(Wi) = 1),
// in 1 gram of matter we should have sum(Ki * Avogadro/Ai) particles,
// and sum(Ki * Avogadro* Zi/Ai) electrons;
// K1*Avogadro/A1  :  K2*Avogadro/A2 : ...  =
// W1              : W2              : ... .
// Then 
// Ki = Ai * Wi / sum( Ai * Wi ). 
// Therefore the number of particles is Avogadro / mean(Ai)
// and the number of electrons is Avogadro * mean(Zi) / mean(Ai)
// Interesting relations!


// more safe function for large energies ( and for little as well),
// gamma-1 instead of betta
double Bethe_Bloch_energy_loss_gamma_1(double ratio_Z_to_A, // do not forget: 
			                      // 1.0/(gram/mole) 
			                      // 1.0/(gram/mole) 
		  // No! seems it was error in comment: 1.0/(gram/mole)
       // Actually it is 1.0/(weight/mole), weight in internal units 
			 double I_eff, 
			 double gamma_1, double z);


// By another formula main part from PDG.

double Bethe_Bloch_restricted_energy_loss_gamma_1
(double ratio_Z_to_A, // do not forget: 
 // 1.0/(gram/mole) 
 // 1.0/(gram/mole) 
 // No! seems it was error in comment: 1.0/(gram/mole)
 // Actually it is 1.0/(weight/mole), weight in internal units 
 double I_eff, 
 double M,
 double gamma_1, 
 double Ecut, // in internal units
 double z);
// By another formula main part from PDG.

/*
double Bethe_Bloch_restricted_dens_energy_loss_gamma_1
(double ratio_Z_to_A, // do not forget: 
 // 1.0/(gram/mole) 
 // 1.0/(gram/mole) 
 // No! seems it was error in comment: 1.0/(gram/mole)
 // Actually it is 1.0/(weight/mole), weight in internal units 
 double I_eff, 
 double M,
 double gamma_1, 
 double Ecut, // in internal units
 double z);
*/
#endif

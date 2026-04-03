/***************************************************************************
 *
 * StDLambda.cc
 *
 * Author: J. Webb
 ***************************************************************************
 *
 * The design of the StParticleDefinition class and all concrete
 * classes derived from it is largely based on the design of the
 * G4ParticleDefinition class from Geant4 (RD44).
 * Although the code is in large parts different (modified or rewritten)
 * and adapted to the STAR framework the basic idea stays the same.
 *
 **************************************************************************/
#include "StDLambda.hh"
#include "PhysicalConstants.h"
#include "StarPDGEncoding.hh"

StDLambda::StDLambda(const string  &  aName,
		     double           mass,
		     double           width,
		     double           charge,
		     int              iSpin,
		     int              iParity,
		     int              iConjugation,
		     int              iIsospin,
		     int              iIsospinZ,
		     int              gParity,
		     const string  &  pType,
		     int              lepton,
		     int              baryon,
		     int              encoding,
		     bool             stable,
		     double           lifetime)
    : StBaryon(aName, mass, width, charge, iSpin, iParity,
	       iConjugation, iIsospin, iIsospinZ, gParity,
	       pType, lepton, baryon, encoding, stable,
	       lifetime) {/* noop */}

// ......................................................................
// ...                 static member definitions                      ...
// ......................................................................
//
//    Arguments for constructor are as follows
//               name             mass          width         charge
//             2*spin           parity  C-conjugation
//          2*Isospin       2*Isospin3       G-parity
//               type    lepton number  baryon number   PDG encoding
//             stable         lifetime
//
// d + Lambda bound state (pseudo-particle).
// Decays to deuteron(45) + Lambda constrained to p+pi-(98).
// Daughter mass sum: 1.876 + 1.115683 = 2.992 GeV; parent mass = 2.998 GeV.
//
StDLambda StDLambda::mDLambda(
    "dLambda",    2.998*GeV,   0.0*MeV,   +1.0*eplus,
    1,              +1,           0,
    0,               0,           0,
    "pseudo",        0,          +3,    kDLambda,
    false,       1.0e-19
);

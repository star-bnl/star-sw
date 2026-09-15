/***************************************************************************
 *
 * StDSigma0.cc
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
#include "StDSigma0.hh"
#include "PhysicalConstants.h"
#include "StarPDGEncoding.hh"

StDSigma0::StDSigma0(const string  &  aName,
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
// d + Sigma0 bound state (pseudo-particle).
// Decays to deuteron(45) + Sigma0 constrained to Lambda+gamma(94),
// with Lambda further constrained to p+pi-(98).
// Daughter mass sum: 1.876 + 1.193 = 3.069 GeV; parent mass = 3.074 GeV.
//
StDSigma0 StDSigma0::mDSigma0(
    "dSigma0",    3.074*GeV,   0.0*MeV,   +1.0*eplus,
    1,              +1,           0,
    0,               0,           0,
    "pseudo",        0,          +3,    kDSigma0,
    false,       1.0e-19
);

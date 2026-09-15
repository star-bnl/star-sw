/***************************************************************************
 *
 * StDXiMinus.cc
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
#include "StDXiMinus.hh"
#include "PhysicalConstants.h"
#include "StarPDGEncoding.hh"

StDXiMinus::StDXiMinus(const string  &  aName,
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
// d + Xi- bound state (pseudo-particle).
// Decays to deuteron(45) + Xi- constrained to Lambda+pi-(93),
// with Lambda further constrained to p+pi-(98).
// Net charge: +1 (d) + (-1) (Xi-) = 0.
// Daughter mass sum: 1.876 + 1.32171 = 3.198 GeV; parent mass = 3.204 GeV.
//
StDXiMinus StDXiMinus::mDXiMinus(
    "dXiMinus",   3.204*GeV,   0.0*MeV,    0.0,
    1,              +1,           0,
    1,              -1,           0,
    "pseudo",        0,          +3,    kDXiMinus,
    false,       1.0e-19
);

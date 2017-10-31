/***************************************************************************
 *
 * $Id: StKStarZero.cc,v 1.1 2017/10/30 15:23:54 jwebb Exp $
 *
 * Author: Thomas Ullrich, May 99 (based on Geant4 code, see below) 
 ***************************************************************************
 *
 * The design of the StParticleDefinition class and all concrete
 * classes derived from it is largely based on the design of the 
 * G4ParticleDefinition class from Geant4 (RD44).
 * Although the code is in large parts different (modified or rewritten)
 * and adapted to the STAR framework the basic idea stays the same.
 *
 ***************************************************************************
 *
 * $Log: StKStarZero.cc,v $
 * Revision 1.1  2017/10/30 15:23:54  jwebb
 * Add K*0(892) aka pdg 313 to gstar with g3id = 10013
 *
 * Revision 1.1  1999/05/14 18:47:54  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StKStarZero.hh" 
#include "PhysicalConstants.h"

StKStarZero::StKStarZero(const string  &  aName,  
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
    : StMeson(aName, mass, width, charge, iSpin, iParity,
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
StKStarZero StKStarZero::mKStarZero(
	      "k*0",    0.89166*GeV,     505.0*MeV,         0.0, 
		    0,              -1,             0,          
		    1,              -1,             0,             
	      "meson",               0,             0,         313,
		false,             1.30338987920792075e-23
);

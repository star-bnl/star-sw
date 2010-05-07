/***************************************************************************
 *
 * $Id: StKaonZeroMode0809.cc,v 1.1 2010/05/07 15:37:11 jwebb Exp $
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
 * $Log: StKaonZeroMode0809.cc,v $
 * Revision 1.1  2010/05/07 15:37:11  jwebb
 * Added StKaonZeroMode0809 to represent the k0 --> pi+ pi- w/ 100% br in
 * gstar_part.g.
 *
 * Revision 1.1  1999/05/14 18:47:54  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StKaonZeroMode0809.hh" 
#include "PhysicalConstants.h"

// Kaon --> pi+ pi- 100% branching ratio 

StKaonZeroMode0809::StKaonZeroMode0809(const string  &  aName,  
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
StKaonZeroMode0809 StKaonZeroMode0809::mKaonZero(
      "kaon0mode0809",    0.497672*GeV,       0.0*MeV,         0.0, 
		    0,              -1,             0,          
		    1,              -1,             0,             
	      "meson",               0,             0,      100311,
		false,             0.0
);

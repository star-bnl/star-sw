/***************************************************************************
 *
 * $Id: StXiZero.cc,v 1.1 1999/05/14 18:48:36 ullrich Exp $
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
 * $Log: StXiZero.cc,v $
 * Revision 1.1  1999/05/14 18:48:36  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StXiZero.hh" 
#include "PhysicalConstants.h"

StXiZero::StXiZero(const string  &  aName,  
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
StXiZero StXiZero::mXiZero(
                "xi0",      1.3149*GeV,       0.0*MeV,         0.0, 
		    1,              +1,             0,          
		    1,              +1,             0,             
	     "baryon",               0,            +1,        3322,
		false,        0.290*nanosecond
);

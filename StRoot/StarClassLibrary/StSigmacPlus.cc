/***************************************************************************
 *
 * $Id: StSigmacPlus.cc,v 1.1 1999/05/14 18:48:27 ullrich Exp $
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
 * $Log: StSigmacPlus.cc,v $
 * Revision 1.1  1999/05/14 18:48:27  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StSigmacPlus.hh" 
#include "PhysicalConstants.h"

StSigmacPlus::StSigmacPlus(const string  &  aName,  
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
StSigmacPlus StSigmacPlus::mSigmacPlus(
           "sigma_c+",      2.4535*GeV,       0.0*MeV,  +1.0*eplus, 
		    1,              +1,             0,          
		    2,              +1,             0,             
	     "baryon",               0,            +1,        4212,
		false,          0.0*nanosecond
);

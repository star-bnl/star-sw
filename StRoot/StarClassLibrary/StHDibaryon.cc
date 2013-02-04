/***************************************************************************
 *
 * $Id: StHDibaryon.cc,v 1.2 2013/02/04 20:41:54 jwebb Exp $
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
 * $Log: StHDibaryon.cc,v $
 * Revision 1.2  2013/02/04 20:41:54  jwebb
 * Typo fix, and mass increase.
 *
 * Revision 1.1  2013/01/31 18:21:46  jwebb
 * Updated StarClassLibrary and gstar_part.g to add the H Dibaryon.
 *
 * Revision 1.1  1999/05/14 18:47:57  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StHDibaryon.hh" 
#include "PhysicalConstants.h"

StHDibaryon::StHDibaryon(const string  &  aName,  
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
StHDibaryon StHDibaryon::mHDibaryon(
             "hdibaryon",    2.232*GeV,       0.0*MeV,         0.0, 
		    2,              +1,             0,          
		    0,               0,             0,             
	     "dibaryon",             0,            +2,           0,
	        false,       0.2632*nanosecond/2
);

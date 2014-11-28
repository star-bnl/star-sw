/***************************************************************************
 *
 * $Id: StZZeroBoson.cc,v 1.1 2010/01/28 19:28:00 jwebb Exp $
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
 * $Log: StZZeroBoson.cc,v $
 * Revision 1.1  2010/01/28 19:28:00  jwebb
 * Added the W and Z bosons to the particles in the StarClassLibrary.  This
 * makes the particles available by name and by PDG id from the StParticleTable.
 *
 * Revision 1.1  1999/05/14 18:47:47  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StZZeroBoson.hh" 
#include "PhysicalConstants.h"

StZZeroBoson::StZZeroBoson(const string  &  aName,  
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
    : StBoson(aName, mass, width, charge, iSpin, iParity,
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
StZZeroBoson StZZeroBoson::mZZeroBoson(
	      "zzero",         91.1876*GeV,       2.4952*GeV,         0.0, 
   	            2,              -1,            -1,          
		    0,               0,             0,             
	      "zzero",               0,             0,               23,
		false,             0.0
);

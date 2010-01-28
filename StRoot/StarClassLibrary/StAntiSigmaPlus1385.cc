/***************************************************************************
 *
 * $Id: StAntiSigmaPlus1385.cc,v 1.1 2010/01/28 21:54:20 jwebb Exp $
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
 * $Log: StAntiSigmaPlus1385.cc,v $
 * Revision 1.1  2010/01/28 21:54:20  jwebb
 * Added the Sigma(1385) baryons.
 *
 * Revision 1.1  1999/05/14 18:48:25  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StAntiSigmaPlus1385.hh" 
#include "PhysicalConstants.h"

StAntiSigmaPlus1385::StAntiSigmaPlus1385(const string  &  aName,  
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
StAntiSigmaPlus1385 StAntiSigmaPlus1385::mAntiSigmaPlus1385(
             "anti sigma(1385)+",     1.3872*GeV,       0.0*MeV,       -eplus, 
		    1,              +1,             0,          
		    2,              +2,             0,             
	     "baryon",               0,            +1,        -3114,
		false,       1.67E-14*nanosecond
);
//NAME: anti sigma(1385)+
//MASS: 1.3872
//CHRG: -1
//PDGI: -3114
//GNTI: 703
//LIFE: 1.67E-23

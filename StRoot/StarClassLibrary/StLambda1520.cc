/***************************************************************************
 *
 * $Id: StLambda1520.cc,v 1.3 2010/04/06 14:16:22 jwebb Exp $
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
 * $Log: StLambda1520.cc,v $
 * Revision 1.3  2010/04/06 14:16:22  jwebb
 * Redefined the geant ID of the lambda(1520) from 706 to 995, to make
 * consistent with usage in embedding group.
 *
 * Revision 1.2  2010/03/22 21:32:34  jwebb
 * And set the pdg ID to geant ID mapping.
 *
 * Revision 1.1  2010/03/22 21:22:40  jwebb
 * Added the Lambda 1520 and antiparticle.
 *
 * Revision 1.1  1999/05/14 18:47:57  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StLambda1520.hh" 
#include "PhysicalConstants.h"

StLambda1520::StLambda1520(const string  &  aName,  
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
StLambda1520 StLambda1520::mLambda1520(
             "lambda1520",    1.519500*GeV,      15.6*MeV,         0.0, 
		    3,              -1,             0,          
		    0,               0,             0,             
	     "baryon",               0,            +1,        20003122, 
	        false,       0.2632*nanosecond
);

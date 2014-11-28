/***************************************************************************
 *
 * $Id: StHyperTriton.cc,v 1.1 2010/01/28 20:01:00 jwebb Exp $
 * Hypertriton decaying into He3 + pion
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
 * $Log: StHyperTriton.cc,v $
 * Revision 1.1  2010/01/28 20:01:00  jwebb
 * Added two 'STAR' particle classes.  The (fake) Dalitz particle, which
 * is really just a pi0 with its Dalitz decay branch at 100%.  It is added
 * so the embedding team can access it by the geant ID defined in gstar_part.g.
 * The hypertriton is also added.
 *
 * In both cases we define a fake PDG id in the header file StarPDGEncoding.hh.
 *
 * Revision 1.1  1999/05/14 18:47:49  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StHyperTriton.hh" 
#include "PhysicalConstants.h"

#include "StarPDGEncoding.hh"

StHyperTriton::StHyperTriton(const string  &  aName,  
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
  : StIon(aName, mass, width, charge, iSpin, iParity,
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

StHyperTriton StHyperTriton::mHyperTriton(
	      "HyperTriton",     2.991  *GeV,   0.0*MeV,   +1.0*eplus, 
	      1,              +1,             0,          
	      0,               0,             0,             
	      "hypernucleus",  0,            +3,        kHyperTriton,
	      false,           2.632e-10
  );

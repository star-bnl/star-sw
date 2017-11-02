/***************************************************************************
 *
 * $Id: StKStarZero.hh,v 1.1 2017/10/30 15:23:54 jwebb Exp $
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
 * $Log: StKStarZero.hh,v $
 * Revision 1.1  2017/10/30 15:23:54  jwebb
 * Add K*0(892) aka pdg 313 to gstar with g3id = 10013
 *
 * Revision 1.1  1999/05/14 18:49:36  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StKStarZero_hh
#define StKStarZero_hh

#include "StMeson.hh"

class StKStarZero : public StMeson {
public:
    static StKStarZero* instance() {return &mKStarZero;}
    static StKStarZero* kaonZero() {return &mKStarZero;}
    
private:
    static StKStarZero mKStarZero;
    
    StKStarZero(const string  &  aName,  
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
	       double           lifetime);
};

#endif

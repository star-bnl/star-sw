/***************************************************************************
 *
 * $Id: StEtaPrime.hh,v 1.1 1999/05/14 18:49:28 ullrich Exp $
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
 * $Log: StEtaPrime.hh,v $
 * Revision 1.1  1999/05/14 18:49:28  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StEtaPrime_hh
#define StEtaPrime_hh

#include "StMeson.hh"

class StEtaPrime : public StMeson {
public:
    static StEtaPrime* instance() {return &mEtaPrime;}
    static StEtaPrime* etaPrime() {return &mEtaPrime;}
    
private:
    static StEtaPrime mEtaPrime;
    
    StEtaPrime(const string  &  aName,  
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

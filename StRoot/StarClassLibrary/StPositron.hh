/***************************************************************************
 *
 * $Id: StPositron.hh,v 1.1 1999/05/14 18:50:06 ullrich Exp $
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
 * $Log: StPositron.hh,v $
 * Revision 1.1  1999/05/14 18:50:06  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StPositron_hh
#define StPositron_hh

#include "StLepton.hh"

class StPositron : public StLepton {
public:
    static StPositron* instance() {return &mPositron;}
    static StPositron* positron() {return &mPositron;}
    
private:
    static StPositron mPositron;
    
    StPositron(const string  &  aName,  
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

/***************************************************************************
 *
 * $Id: StElectron.hh,v 1.1 1999/05/14 18:49:25 ullrich Exp $
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
 * $Log: StElectron.hh,v $
 * Revision 1.1  1999/05/14 18:49:25  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StElectron_hh
#define StElectron_hh

#include "StLepton.hh"

class StElectron : public StLepton {
public:
    static StElectron* instance() {return &mElectron;}
    static StElectron* electron() {return &mElectron;}
    
private:
    static StElectron mElectron;
    
    StElectron(const string  &  aName,  
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

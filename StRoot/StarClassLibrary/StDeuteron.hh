/***************************************************************************
 *
 * $Id: StDeuteron.hh,v 1.1 1999/05/14 18:49:22 ullrich Exp $
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
 * $Log: StDeuteron.hh,v $
 * Revision 1.1  1999/05/14 18:49:22  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StDeuteron_hh
#define StDeuteron_hh

#include "StIon.hh"

class StDeuteron : public StIon {
public:
    static StDeuteron* instance() {return &mDeuteron;}
    static StDeuteron* deuteron() {return &mDeuteron;}
    
private:
    static StDeuteron mDeuteron;
    
    StDeuteron(const string  &  aName,  
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

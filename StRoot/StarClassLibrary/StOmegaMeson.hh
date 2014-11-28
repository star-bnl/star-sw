/***************************************************************************
 *
 * $Id: StOmegaMeson.hh,v 1.1 2000/04/06 22:23:26 ullrich Exp $
 *
 * Author: Thomas Ullrich, Apr 2000 (based on Geant4 code, see below) 
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
 * $Log: StOmegaMeson.hh,v $
 * Revision 1.1  2000/04/06 22:23:26  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StOmegaMeson_hh
#define StOmegaMeson_hh

#include "StMeson.hh"

class StOmegaMeson : public StMeson {
public:
    static StOmegaMeson* instance() {return &mOmegaMeson;}
    static StOmegaMeson* omegaMeson() {return &mOmegaMeson;}
    
private:
    static StOmegaMeson mOmegaMeson;
    
    StOmegaMeson(const string  &  aName,  
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

/***************************************************************************
 *
 * $Id: StXiZero.hh,v 1.1 1999/05/14 18:50:24 ullrich Exp $
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
 * $Log: StXiZero.hh,v $
 * Revision 1.1  1999/05/14 18:50:24  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StXiZero_hh
#define StXiZero_hh

#include "StBaryon.hh"

class StXiZero : public StBaryon {
public:
    static StXiZero* instance() {return &mXiZero;}
    static StXiZero* xiZero() {return &mXiZero;}
    
private:
    static StXiZero mXiZero;
    
    StXiZero(const string  &  aName,  
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

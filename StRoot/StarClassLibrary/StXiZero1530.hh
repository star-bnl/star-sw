/***************************************************************************
 *
 * $Id: StXiZero1530.hh,v 1.1 2012/06/25 16:02:05 jwebb Exp $
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
 * $Log: StXiZero1530.hh,v $
 * Revision 1.1  2012/06/25 16:02:05  jwebb
 * Added Xi0(1530).
 *
 * Revision 1.1  1999/05/14 18:50:24  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StXiZero1530_hh
#define StXiZero1530_hh

#include "StBaryon.hh"

class StXiZero1530 : public StBaryon {
public:
    static StXiZero1530* instance() {return &mXiZero1530;}
    static StXiZero1530* xiZero() {return &mXiZero1530;}
    
private:
    static StXiZero1530 mXiZero1530;
    
    StXiZero1530(const string  &  aName,  
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

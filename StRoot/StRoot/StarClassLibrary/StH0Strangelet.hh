/***************************************************************************
 *
 * $Id: StH0Strangelet.hh,v 1.1 2015/06/23 14:53:36 jwebb Exp $
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
 *$Log: StH0Strangelet.hh,v $
 *Revision 1.1  2015/06/23 14:53:36  jwebb
 *StarClassLibrary support for H0 dibaryon.
 *
 *
 **************************************************************************/
#ifndef StH0Strangelet_hh
#define StH0Strangelet_hh

#include "StBaryon.hh"

class StH0Strangelet : public StBaryon {
public:
    static StH0Strangelet* instance() {return &mH0Strangelet;}
    
private:
    static StH0Strangelet mH0Strangelet;
    
    StH0Strangelet(const string  &  aName,  
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

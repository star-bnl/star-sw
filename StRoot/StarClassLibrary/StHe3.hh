/***************************************************************************
 *
 * $Id: StHe3.hh,v 1.1 1999/05/14 18:49:30 ullrich Exp $
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
 * $Log: StHe3.hh,v $
 * Revision 1.1  1999/05/14 18:49:30  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StHe3_hh
#define StHe3_hh

#include "StIon.hh"

class StHe3 : public StIon {
public:
    static StHe3* instance() {return &mHe3;}
    static StHe3* he3() {return &mHe3;}
    
private:
    static StHe3 mHe3;
    
    StHe3(const string  &  aName,  
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

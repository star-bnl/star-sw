/***************************************************************************
 *
 * $Id: StPhi.hh,v 1.1 2000/04/06 22:23:20 ullrich Exp $
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
 * $Log: StPhi.hh,v $
 * Revision 1.1  2000/04/06 22:23:20  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StPhi_hh
#define StPhi_hh

#include "StMeson.hh"

class StPhi : public StMeson {
public:
    static StPhi* instance() {return &mPhi;}
    static StPhi* phi() {return &mPhi;}
    
private:
    static StPhi mPhi;
    
    StPhi(const string  &  aName,  
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

/***************************************************************************
 *
 * $Id: StIon.hh,v 1.1 1999/05/14 18:49:31 ullrich Exp $
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
 * $Log: StIon.hh,v $
 * Revision 1.1  1999/05/14 18:49:31  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StIon_hh
#define StIon_hh

#include "StParticleDefinition.hh"

class StIon : public StParticleDefinition {
public:
    StIon(const string  &  aName,  
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
	  double           lifetime)
	: StParticleDefinition(aName, mass, width, charge, iSpin, iParity,
			       iConjugation, iIsospin, iIsospinZ, gParity,
			       pType, lepton, baryon, encoding, stable,
			       lifetime) {/* noop */};
    virtual ~StIon() {/* noop */};
    
private:
    const StIon & operator=(const StIon& m) {return m;}
};

#endif

/***************************************************************************
 *
 * $Id: StLepton.hh,v 1.1 1999/05/14 18:49:45 ullrich Exp $
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
 * $Log: StLepton.hh,v $
 * Revision 1.1  1999/05/14 18:49:45  ullrich
 * Initial Revision
 *
 **************************************************************************/
//  A virtual class for Leptons. It defines
//  public methods which describe the behavior of a
//  lepton.
#ifndef StLepton_hh
#define StLepton_hh

#include "StParticleDefinition.hh"

class StLepton : public StParticleDefinition {
public:
    StLepton(const string  &  aName,  
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
    virtual ~StLepton() {/* noop */};
    
private:
    const StLepton & operator=(const StLepton& m) {return m;}
};

#endif

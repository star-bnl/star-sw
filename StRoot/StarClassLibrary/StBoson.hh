/***************************************************************************
 *
 * $Id: StBoson.hh,v 1.1 1999/05/14 18:49:16 ullrich Exp $
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
 * $Log: StBoson.hh,v $
 * Revision 1.1  1999/05/14 18:49:16  ullrich
 * Initial Revision
 *
 **************************************************************************/
//  A virtual class for Bosons.
#ifndef StBoson_hh
#define StBoson_hh

#include "StParticleDefinition.hh"

class StBoson : public StParticleDefinition {
public:
    StBoson(const string  &  aName,  
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
    virtual ~StBoson() {/* noop */};
    
private:
    const StBoson & operator=(const StBoson& m) {return m;}
};

#endif

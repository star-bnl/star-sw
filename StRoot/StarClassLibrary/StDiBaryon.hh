/***************************************************************************
 *
 * $Id $
 *
 * Author: Jason Webb, based on StBaryon.hh
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
 * $Log: StDiBaryon.hh,v $
 * Revision 1.1  2013/01/31 18:21:46  jwebb
 * Updated StarClassLibrary and gstar_part.g to add the H Dibaryon.
 *
 *
 **************************************************************************/
//  A virtual class for DiBaryons. It defines
//  public methods which describe the behavior of a
//  baryon.
#ifndef StDiBaryon_hh
#define StDiBaryon_hh

#include "StParticleDefinition.hh"

class StDiBaryon : public StParticleDefinition {
public:
    StDiBaryon(const string  &  aName,  
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
    virtual ~StDiBaryon() {/* noop */};
    
private:
    const StDiBaryon & operator=(const StDiBaryon& m) {return m;}
};

#endif

/***************************************************************************
 *
 * $Id: StKaonZero.hh,v 1.1 1999/05/14 18:49:36 ullrich Exp $
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
 * $Log: StKaonZero.hh,v $
 * Revision 1.1  1999/05/14 18:49:36  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StKaonZero_hh
#define StKaonZero_hh

#include "StMeson.hh"

class StKaonZero : public StMeson {
public:
    static StKaonZero* instance() {return &mKaonZero;}
    static StKaonZero* kaonZero() {return &mKaonZero;}
    
private:
    static StKaonZero mKaonZero;
    
    StKaonZero(const string  &  aName,  
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

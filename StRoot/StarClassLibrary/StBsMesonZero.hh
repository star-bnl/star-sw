/***************************************************************************
 *
 * $Id: StBsMesonZero.hh,v 1.1 1999/05/14 18:49:17 ullrich Exp $
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
 * $Log: StBsMesonZero.hh,v $
 * Revision 1.1  1999/05/14 18:49:17  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StBsMesonZero_hh
#define StBsMesonZero_hh

#include "StMeson.hh"

class StBsMesonZero : public StMeson {
public:
    static StBsMesonZero* instance() {return &mBsMesonZero;}
    static StBsMesonZero* bsMesonZero() {return &mBsMesonZero;}
    
private:
    static StBsMesonZero mBsMesonZero;
    
    StBsMesonZero(const string  &  aName,  
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

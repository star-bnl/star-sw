/***************************************************************************
 *
 * $Id: StPsi2s.hh,v 1.1 2014/06/26 12:34:10 jwebb Exp $
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
 * $Log: StPsi2s.hh,v $
 * Revision 1.1  2014/06/26 12:34:10  jwebb
 * Added Psi(2s) state.
 *
 * Revision 1.1  1999/05/14 18:49:32  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StPsi2s_hh
#define StPsi2s_hh

#include "StMeson.hh"

class StPsi2s : public StMeson {
public:
    static StPsi2s* instance() {return &mPsi2s;}
    static StPsi2s* jPsi() {return &mPsi2s;}
    
private:
    static StPsi2s mPsi2s;
    
    StPsi2s(const string  &  aName,  
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

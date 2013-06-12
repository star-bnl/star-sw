/***************************************************************************
 *
 * $Id: StAntiTriton.hh,v 1.1 2011/03/25 18:17:45 jwebb Exp $
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
 * $Log: StAntiTriton.hh,v $
 * Revision 1.1  2011/03/25 18:17:45  jwebb
 * Updates to StParticleTable and additions to STAR Class Library
 *
 * (1) resolve ticket 2097
 * (2) include all particles/deays defined in gstar_part.g
 * (3) added few anti-nuclei in anticipation of future needs.
 * (3) added the geantino for completeness
 *
 * Revision 1.1  1999/05/14 18:50:22  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StAntiTriton_hh
#define StAntiTriton_hh

#include "StIon.hh"

class StAntiTriton : public StIon {
public:
    static StAntiTriton* instance() {return &mAntiTriton;}
    static StAntiTriton* triton() {return &mAntiTriton;}
    
private:
    static StAntiTriton mAntiTriton;
    
    StAntiTriton(const string  &  aName,  
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

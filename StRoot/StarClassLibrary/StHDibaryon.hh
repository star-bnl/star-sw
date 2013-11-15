/***************************************************************************
 *
 * $Id: StHDibaryon.hh,v 1.1 2013/01/31 18:21:46 jwebb Exp $
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
 * $Log: StHDibaryon.hh,v $
 * Revision 1.1  2013/01/31 18:21:46  jwebb
 * Updated StarClassLibrary and gstar_part.g to add the H Dibaryon.
 *
 * Revision 1.1  1999/05/14 18:49:43  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StHDibaryon_hh
#define StHDibaryon_hh

#include "StBaryon.hh"

class StHDibaryon : public StBaryon {
public:
    static StHDibaryon* instance() {return &mHDibaryon;}
    static StHDibaryon* lambda() {return &mHDibaryon;}
    
private:
    static StHDibaryon mHDibaryon;
    
    StHDibaryon(const string  &  aName,  
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

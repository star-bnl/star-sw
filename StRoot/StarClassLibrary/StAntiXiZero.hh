/***************************************************************************
 *
 * $Id: StAntiXiZero.hh,v 1.1 1999/05/14 18:49:07 ullrich Exp $
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
 * $Log: StAntiXiZero.hh,v $
 * Revision 1.1  1999/05/14 18:49:07  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StAntiXiZero_hh
#define StAntiXiZero_hh

#include "StBaryon.hh"

class StAntiXiZero : public StBaryon {
public:
    static StAntiXiZero* instance() {return &mAntiXiZero;}
    static StAntiXiZero* antiXiZero() {return &mAntiXiZero;}
    
private:
    static StAntiXiZero mAntiXiZero;
    
    StAntiXiZero(const string  &  aName,  
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

/***************************************************************************
 *
 * $Id: StAntiLambda1520.hh,v 1.1 2010/03/22 21:22:40 jwebb Exp $
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
 * $Log: StAntiLambda1520.hh,v $
 * Revision 1.1  2010/03/22 21:22:40  jwebb
 * Added the Lambda 1520 and antiparticle.
 *
 * Revision 1.1  1999/05/14 18:49:43  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StAntiLambda1520_hh
#define StAntiLambda1520_hh

#include "StBaryon.hh"

class StAntiLambda1520 : public StBaryon {
public:
    static StAntiLambda1520* instance() {return &mAntiLambda1520;}
    static StAntiLambda1520* antiLambda1520() {return &mAntiLambda1520;}
    
private:
    static StAntiLambda1520 mAntiLambda1520;
    
    StAntiLambda1520(const string  &  aName,  
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

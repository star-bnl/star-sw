/***************************************************************************
 *
 * $Id: StSigmaMinus1385.hh,v 1.1 2010/01/28 21:54:20 jwebb Exp $
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
 * $Log: StSigmaMinus1385.hh,v $
 * Revision 1.1  2010/01/28 21:54:20  jwebb
 * Added the Sigma(1385) baryons.
 *
 * Revision 1.1  1999/05/14 18:50:13  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StSigmaMinus1385_hh
#define StSigmaMinus1385_hh

#include "StBaryon.hh"

class StSigmaMinus1385 : public StBaryon {
public:
    static StSigmaMinus1385* instance() {return &mSigmaMinus1385;}
    static StSigmaMinus1385* SigmaMinus1385() {return &mSigmaMinus1385;}
    
private:
    static StSigmaMinus1385 mSigmaMinus1385;
    
    StSigmaMinus1385(const string  &  aName,  
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

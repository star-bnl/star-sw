/***************************************************************************
 *
 * $Id: StAntiSigmaMinus1385.hh,v 1.1 2010/01/28 21:54:20 jwebb Exp $
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
 * $Log: StAntiSigmaMinus1385.hh,v $
 * Revision 1.1  2010/01/28 21:54:20  jwebb
 * Added the Sigma(1385) baryons.
 *
 * Revision 1.1  1999/05/14 18:50:13  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StAntiSigmaMinus1385_hh
#define StAntiSigmaMinus1385_hh

#include "StBaryon.hh"

class StAntiSigmaMinus1385 : public StBaryon {
public:
    static StAntiSigmaMinus1385* instance() {return &mAntiSigmaMinus1385;}
    static StAntiSigmaMinus1385* AntiSigmaMinus1385() {return &mAntiSigmaMinus1385;}
    
private:
    static StAntiSigmaMinus1385 mAntiSigmaMinus1385;
    
    StAntiSigmaMinus1385(const string  &  aName,  
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

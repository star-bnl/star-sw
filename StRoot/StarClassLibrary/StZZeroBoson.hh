/***************************************************************************
 *
 * $Id: StZZeroBoson.hh,v 1.1 2010/01/28 19:28:00 jwebb Exp $
 *
 * Author: Jason C. Webb
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
 * $Log: StZZeroBoson.hh,v $
 * Revision 1.1  2010/01/28 19:28:00  jwebb
 * Added the W and Z bosons to the particles in the StarClassLibrary.  This
 * makes the particles available by name and by PDG id from the StParticleTable.
 *
 *
 **************************************************************************/
#ifndef StZZeroBoson_hh
#define StZZeroBoson_hh

#include "StBoson.hh"

class StZZeroBoson : public StBoson {
public:
    static StZZeroBoson* instance() {return &mZZeroBoson;}
    static StZZeroBoson* zzero() {return &mZZeroBoson;}
    
private:
    static StZZeroBoson mZZeroBoson;
    
    StZZeroBoson(const string  &  aName,  
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

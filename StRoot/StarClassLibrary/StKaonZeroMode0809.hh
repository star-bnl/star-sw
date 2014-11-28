/***************************************************************************
 *
 * $Id: StKaonZeroMode0809.hh,v 1.1 2010/05/07 15:37:11 jwebb Exp $
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
 * $Log: StKaonZeroMode0809.hh,v $
 * Revision 1.1  2010/05/07 15:37:11  jwebb
 * Added StKaonZeroMode0809 to represent the k0 --> pi+ pi- w/ 100% br in
 * gstar_part.g.
 *
 * Revision 1.1  1999/05/14 18:49:36  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StKaonZeroMode0809_hh
#define StKaonZeroMode0809_hh

#include "StMeson.hh"

class StKaonZeroMode0809 : public StMeson {
public:
    static StKaonZeroMode0809* instance() {return &mKaonZero;}
    static StKaonZeroMode0809* kaonZero() {return &mKaonZero;}
    
private:
    static StKaonZeroMode0809 mKaonZero;
    
    StKaonZeroMode0809(const string  &  aName,  
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

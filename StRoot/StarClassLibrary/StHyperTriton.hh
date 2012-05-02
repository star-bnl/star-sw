/***************************************************************************
 *
 * $Id: StHyperTriton.hh,v 1.1 2010/01/28 20:01:00 jwebb Exp $
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
 * $Log: StHyperTriton.hh,v $
 * Revision 1.1  2010/01/28 20:01:00  jwebb
 * Added two 'STAR' particle classes.  The (fake) Dalitz particle, which
 * is really just a pi0 with its Dalitz decay branch at 100%.  It is added
 * so the embedding team can access it by the geant ID defined in gstar_part.g.
 * The hypertriton is also added.
 *
 * In both cases we define a fake PDG id in the header file StarPDGEncoding.hh.
 *
 * Revision 1.1  1999/05/14 18:49:30  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StHyperTriton_hh
#define StHyperTriton_hh

#include "StIon.hh"

class StHyperTriton : public StIon {
public:
    static StHyperTriton* instance() {return &mHyperTriton;}
    static StHyperTriton* hypertriton() {return &mHyperTriton;}
    
private:
    static StHyperTriton mHyperTriton;
    
    StHyperTriton(const string  &  aName,  
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

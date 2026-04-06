/***************************************************************************
 *
 * StDLambda.hh
 *
 * Author: J. Webb
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
 * Pseudoparticle representing a loosely bound deuteron + Lambda state.
 * Decays via d_Lambda --> deuteron(45) + Lambda(98, constrained to p+pi-).
 * GEANT3 ID: 60100.  STAR PDG encoding: kDLambda.
 *
 **************************************************************************/
#ifndef StDLambda_hh
#define StDLambda_hh

#include "StBaryon.hh"

class StDLambda : public StBaryon {
public:
    static StDLambda* instance()  {return &mDLambda;}
    static StDLambda* dLambda()   {return &mDLambda;}

private:
    static StDLambda mDLambda;

    StDLambda(const string  &  aName,
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

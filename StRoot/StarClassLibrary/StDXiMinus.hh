/***************************************************************************
 *
 * StDXiMinus.hh
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
 * Pseudoparticle representing a loosely bound deuteron + Xi- state.
 * Decays via d_Xi_minus --> deuteron(45) + Xi-(93, constrained to Lambda+pi-),
 * with Lambda further constrained to p+pi- via GID 98.
 * Net charge = +1 (d) + (-1) (Xi-) = 0.
 * GEANT3 ID: 60102.  STAR PDG encoding: kDXiMinus.
 *
 **************************************************************************/
#ifndef StDXiMinus_hh
#define StDXiMinus_hh

#include "StBaryon.hh"

class StDXiMinus : public StBaryon {
public:
    static StDXiMinus* instance()  {return &mDXiMinus;}
    static StDXiMinus* dXiMinus()  {return &mDXiMinus;}

private:
    static StDXiMinus mDXiMinus;

    StDXiMinus(const string  &  aName,
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

/***************************************************************************
 *
 * StDXiZero.hh
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
 * Pseudoparticle representing a loosely bound deuteron + Xi0 state.
 * Decays via d_Xi_zero --> deuteron(45) + Xi0(92, constrained to Lambda+pi0),
 * with Lambda further constrained to p+pi- via GID 98.
 * Net charge = +1 (d) + 0 (Xi0) = +1.
 * GEANT3 ID: 60103.  STAR PDG encoding: kDXiZero.
 *
 **************************************************************************/
#ifndef StDXiZero_hh
#define StDXiZero_hh

#include "StBaryon.hh"

class StDXiZero : public StBaryon {
public:
    static StDXiZero* instance()  {return &mDXiZero;}
    static StDXiZero* dXiZero()   {return &mDXiZero;}

private:
    static StDXiZero mDXiZero;

    StDXiZero(const string  &  aName,
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

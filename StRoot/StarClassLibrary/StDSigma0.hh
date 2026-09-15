/***************************************************************************
 *
 * StDSigma0.hh
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
 * Pseudoparticle representing a loosely bound deuteron + Sigma0 state.
 * Decays via d_Sigma0 --> deuteron(45) + Sigma0(94, constrained to Lambda+gamma),
 * with Lambda further constrained to p+pi- via GID 98.
 * GEANT3 ID: 60101.  STAR PDG encoding: kDSigma0.
 *
 **************************************************************************/
#ifndef StDSigma0_hh
#define StDSigma0_hh

#include "StBaryon.hh"

class StDSigma0 : public StBaryon {
public:
    static StDSigma0* instance()  {return &mDSigma0;}
    static StDSigma0* dSigma0()   {return &mDSigma0;}

private:
    static StDSigma0 mDSigma0;

    StDSigma0(const string  &  aName,
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

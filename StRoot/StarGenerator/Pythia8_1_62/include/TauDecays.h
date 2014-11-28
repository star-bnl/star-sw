// TauDecays.h is a part of the PYTHIA event generator.
// Copyright (C) 2012 Philip Ilten, Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Header file for the TauDecays class.

#ifndef Pythia8_TauDecays_H
#define Pythia8_TauDecays_H

#include "Basics.h"
#include "Event.h"
#include "HelicityBasics.h"
#include "HelicityMatrixElements.h"
#include "PythiaComplex.h"
#include "PythiaStdlib.h"
#include "Settings.h"

namespace Pythia8 {

//==========================================================================

// TauDecays class.
// This class decays tau leptons, with helicity information.

class TauDecays {

public:

  // Constructor and destructor.
  TauDecays() {};
  ~TauDecays() {}
  
  // Initializer.
  void init(Info* infoPtrIn, Settings* settingsPtrIn, 
    ParticleData* particleDataPtrIn, Rndm* rndmPtrIn,
    Couplings* couplingsPtrIn);

  // Decay a tau or correlated tau pair.
  bool decay(int iDec, Event& event);

  // Choose a decay channel for a particle.
  vector<HelicityParticle> createChildren(HelicityParticle parent);

  // Perform an N-body isotropic decay.
  void isotropicDecay(vector<HelicityParticle>& p);

  // Write the decay to event record.
  void writeDecay(Event& event, vector<HelicityParticle>& p);

private: 

  // Constants: could only be changed in the code itself.
  static const int    NTRYCHANNEL, NTRYDECAY;
  static const double WTCORRECTION[11];

  // Flag whether a correlated tau decay should be performed.
  bool   correlated;

  // User selected mode and mother for tau decays.
  int    tauMode, tauMother;

  // User selected polarization for tau decays.
  double polarization;

  // Helicity matrix element pointers.
  HelicityMatrixElement* hardME;
  HelicityMatrixElement* decayME;

  // Hard process helicity matrix elements.
  HMETwoFermions2W2TwoFermions      hmeTwoFermions2W2TwoFermions;
  HMETwoFermions2Z2TwoFermions      hmeTwoFermions2Z2TwoFermions;
  HMETwoFermions2Gamma2TwoFermions  hmeTwoFermions2Gamma2TwoFermions;
  HMETwoFermions2GammaZ2TwoFermions hmeTwoFermions2GammaZ2TwoFermions;
  HMEHiggsEven2TwoFermions          hmeHiggsEven2TwoFermions;
  HMEHiggsOdd2TwoFermions           hmeHiggsOdd2TwoFermions;
  HMEHiggsCharged2TwoFermions       hmeHiggsCharged2TwoFermions;
  HMEUnpolarized                    hmeUnpolarized;

  // Tau decay helicity matrix elements.
  HMETau2Meson                    hmeTau2Meson;
  HMETau2TwoLeptons               hmeTau2TwoLeptons;
  HMETau2TwoMesonsViaVector       hmeTau2TwoMesonsViaVector;
  HMETau2TwoMesonsViaVectorScalar hmeTau2TwoMesonsViaVectorScalar;
  HMETau2ThreePions               hmeTau2ThreePions;
  HMETau2FourPions                hmeTau2FourPions;
  HMETau2PhaseSpace               hmeTau2PhaseSpace;

  // Particles of the hard process.
  HelicityParticle in1, in2, mediator, out1, out2;
  vector<HelicityParticle> particles;

  // The info pointer for the Pythia class.
  Info*         infoPtr;

  // Pointer to settings database.
  Settings*     settingsPtr;

  // Pointer to the particle data table.
  ParticleData* particleDataPtr;

  // Pointer to the random number generator.
  Rndm*         rndmPtr;

  // Pointer to SM coupling data.
  Couplings*    couplingsPtr;

};

//==========================================================================

} // end namespace Pythia8

#endif // end Pythia8_TauDecays_H

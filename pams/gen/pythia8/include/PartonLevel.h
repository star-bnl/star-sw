// PartonLevel.h is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// This file contains the main class for parton-level event generation
// PartonLevel: administrates showers, multiple interactions and remnants.

#ifndef Pythia8_PartonLevel_H
#define Pythia8_PartonLevel_H

#include "Basics.h"
#include "BeamParticle.h"
#include "BeamRemnants.h"
#include "Event.h"
#include "Info.h"
#include "MultipleInteractions.h"
#include "ParticleData.h"
#include "PythiaStdlib.h"
#include "Settings.h"
#include "SigmaTotal.h"
#include "SpaceShower.h"
#include "TimeShower.h"
#include "UserHooks.h"

namespace Pythia8 {
 
//**************************************************************************

// The PartonLevel class contains the top-level routines to generate
// the partonic activity of an event.

class PartonLevel {

public:

  // Constructor. 
  PartonLevel() : userHooksPtr(0) {} 
 
  // Initialization of all classes at the parton level.
  bool init( Info* infoPtrIn, BeamParticle* beamAPtrIn, 
    BeamParticle* beamBPtrIn, SigmaTotal* sigmaTotPtr, 
    TimeShower* timesDecPtrIn, TimeShower* timesPtrIn, 
    SpaceShower* spacePtrIn, UserHooks* userHooksPtrIn);
 
  // Generate the next parton-level process.
  bool next( Event& process, Event& event); 

  // Tell whether failure was due to vetoing.
  bool hasVetoed() const {return doVeto;}

  // Accumulate and print statistics.
  void accumulate() {multi.accumulate();}
  void statistics(bool reset = false) {if (doMI) multi.statistics(reset);}

private: 

  // Constants: could only be changed in the code itself.
  static const int NTRY;

  // Initialization data, mainly read from Settings.
  bool   doMI, doISR, doFSRduringProcess, doFSRafterProcess, 
         doFSRinResonances, doRemnants, doSecondHard, doMIinit, 
         hasLeptonBeams, hasPointLeptons, canVetoPT, canVetoStep;

  // Event generation strategy. Number of steps. Maximum pT scales.
  bool   doVeto;
  int    nMI, nISR, nFSRinProc, nFSRinRes, nISRhard, nFSRhard, 
         typeLatest, nVetoStep, typeVetoStep, iSysNow;
  double pTsaveMI, pTsaveISR, pTsaveFSR, pTvetoPT;

  // Pointer to various information on the generation.
  Info* infoPtr;

  // Pointers to the two incoming beams.
  BeamParticle* beamAPtr;
  BeamParticle* beamBPtr;

  // Pointer to userHooks object for user interaction with program.
  UserHooks* userHooksPtr;

  // Pointers to timelike showers for resonance decays and the rest.
  TimeShower* timesDecPtr;
  TimeShower* timesPtr;

  // Pointer to spacelike showers.
  SpaceShower* spacePtr;

  // The generator class for multiple interactions.
  MultipleInteractions multi;

  // The generator class to construct beam-remnant kinematics. 
  BeamRemnants remnants;

  // Set up the hard process, excluding subsequent resonance decays.
  void setupHardSys( Event& process, Event& event);
  // Keep track of how much of hard process has been handled.
  int nHardDone;

  // Set up an unresolved process, i.e. elastic or diffractive.
  bool setupUnresolvedSys( Event& process, Event& event);

  // Perform showers in resonance decay chains.
  int resonanceShowers( Event& process, Event& event); 
  // Position in main event record of hard partons before showers.
  vector<int> iPosBefShow;
  
};

//**************************************************************************

} // end namespace Pythia8

#endif // Pythia8_PartonLevel_H

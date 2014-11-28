// ProcessContainer.h is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// This file contains the collected machinery of a process.
// ProcessContainer: contains information on a particular process.
// SetupContainers: administrates the selection/creation of processes.

#ifndef Pythia8_ProcessContainer_H
#define Pythia8_ProcessContainer_H

#include "Basics.h"
#include "BeamParticle.h"
#include "Event.h"
#include "Info.h"
#include "ParticleData.h"
#include "PartonDistributions.h"
#include "PhaseSpace.h"
#include "PythiaStdlib.h"
#include "ResonanceDecays.h"
#include "Settings.h"
#include "SigmaProcess.h"
#include "SigmaTotal.h"
#include "SusyLesHouches.h"
#include "UserHooks.h"

namespace Pythia8 {

//**************************************************************************

// The ProcessContainer class combines pointers to matrix element and 
// phase space generator with general generation info. 

class ProcessContainer {

public:

  // Constructor. 
  ProcessContainer(SigmaProcess* sigmaProcessPtrIn = 0) 
    : sigmaProcessPtr(sigmaProcessPtrIn), phaseSpacePtr(0) {} 

  // Destructor.
  ~ProcessContainer() {delete phaseSpacePtr; delete sigmaProcessPtr;}
  
  // Initialize phase space and counters.
  bool init(Info* infoPtrIn, BeamParticle* beamAPtr, BeamParticle* beamBPtr, 
    AlphaStrong* alphaSPtr, AlphaEM* alphaEMPtr, SigmaTotal* sigmaTotPtr, 
    ResonanceDecays* resDecaysPtrIn, SusyLesHouches* slhaPtr,
    UserHooks* userHooksPtr); 

  // Store or replace Les Houches pointer.
  void setLHAPtr( LHAup* lhaUpPtrIn) {lhaUpPtr = lhaUpPtrIn;
    if (sigmaProcessPtr > 0) sigmaProcessPtr->setLHAPtr(lhaUpPtr); 
    if (phaseSpacePtr > 0) phaseSpacePtr->setLHAPtr(lhaUpPtr);}

  // Update the CM energy of the event.
  void newECM(double eCM) {phaseSpacePtr->newECM(eCM);}

  // Generate a trial event; accepted or not.
  bool trialProcess(); 
  
  // Give the hard subprocess (with option for a second hard subprocess).
  bool constructProcess( Event& process, bool isHardest = true); 

  // Do resonance decays.
  bool decayResonances( Event& process); 

  // Accumulate statistics after user veto.
  void accumulate() {++nAcc;}

  // Reset statistics on events generated so far.
  void reset();

  // Process name and code, and the number of final-state particles.
  string name()        const {return sigmaProcessPtr->name();}
  int    code()        const {return sigmaProcessPtr->code();}
  int    nFinal()      const {return sigmaProcessPtr->nFinal();}

  // Member functions for info on generation process.
  bool   newSigmaMax() const {return newSigmaMx;}
  double sigmaMax()    const {return sigmaMx;}
  long   nTried()      const {return nTry;}
  long   nSelected()   const {return nSel;}
  long   nAccepted()   const {return nAcc;}
  double sigmaSelMC()  {if (nTry > nTryStat) sigmaDelta(); return sigmaAvg;}
  double sigmaMC()     {if (nTry > nTryStat) sigmaDelta(); return sigmaFin;}
  double deltaMC()     {if (nTry > nTryStat) sigmaDelta(); return deltaFin;} 

  // Some kinematics quantities.
  int    id1()         const {return sigmaProcessPtr->id(1);}
  int    id2()         const {return sigmaProcessPtr->id(2);}
  double x1()          const {return phaseSpacePtr->x1();}
  double x2()          const {return phaseSpacePtr->x2();}
  double Q2Fac()       const {return sigmaProcessPtr->Q2Fac();}

  // Tell whether container is for Les Houches events.
  bool   isLHAContainer() const {return isLHA;}

  // When two hard processes set or get info whether process is matched.
  void   isSame( bool isSameIn) { isSameSave = isSameIn;}
  bool   isSame()      const {return isSameSave;}

private:

  // Constants: could only be changed in the code itself.
  static const int N12SAMPLE, N3SAMPLE;

  // Pointer to the subprocess matrix element.
  SigmaProcess*    sigmaProcessPtr;

  // Pointer to the phase space generator.
  PhaseSpace*      phaseSpacePtr;

  // Pointer to various information on the generation.
  Info*            infoPtr;

  // Pointer to ResonanceDecays object for sequential resonance decays.
  ResonanceDecays* resDecaysPtr;

  // Pointer to LHAup for generating external events.
  LHAup*           lhaUpPtr;

  // Info on process.
  bool   isMinBias, isResolved, isDiffA, isDiffB, isLHA, allowNegSig,
         hasOctetOnium, isSameSave;
  int    lhaStrat, lhaStratAbs;

  // Statistics on generation process. (Long integers just in case.)
  int    newSigmaMx;
  long   nTry, nSel, nAcc, nTryStat;  
  double sigmaMx, sigmaSgn, sigmaSum, sigma2Sum, sigmaNeg, sigmaAvg, 
         sigmaFin, deltaFin;

  // Estimate integrated cross section and its uncertainty. 
  void sigmaDelta();

};
 
//**************************************************************************

// The SetupContainers class turns the list of user-requested processes
// into a vector of ProcessContainer objects, each with a process.

class SetupContainers {

public:

  // Constructor. 
  SetupContainers() {} 
 
  // Initialization assuming all necessary data already read.
  bool init(vector<ProcessContainer*>& containerPtrs);
 
  // Initialization of a second hard process.
  bool init2(vector<ProcessContainer*>& container2Ptrs);

};

//**************************************************************************

} // end namespace Pythia8

#endif // Pythia8_ProcessContainer_H

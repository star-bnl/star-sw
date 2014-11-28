// PartonLevel.cc is a part of the PYTHIA event generator.
// Copyright (C) 2012 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Function definitions (not found in the header) for the PartonLevel class.

#include "PartonLevel.h"

namespace Pythia8 {
 
//==========================================================================

// The PartonLevel class.

//--------------------------------------------------------------------------

// Constants: could be changed here if desired, but normally should not.
// These are of technical nature, as described for each.

// Maximum number of tries to produce parton level from given input.
const int PartonLevel::NTRY = 10; 

//--------------------------------------------------------------------------

// Main routine to initialize the parton-level generation process.

bool PartonLevel::init( Info* infoPtrIn, Settings& settings, 
  ParticleData* particleDataPtrIn, Rndm* rndmPtrIn,
  BeamParticle* beamAPtrIn, BeamParticle* beamBPtrIn, 
  BeamParticle* beamPomAPtrIn, BeamParticle* beamPomBPtrIn, 
  Couplings* couplingsPtrIn, PartonSystems* partonSystemsPtrIn, 
  SigmaTotal* sigmaTotPtr, TimeShower* timesDecPtrIn, TimeShower* timesPtrIn, 
  SpaceShower* spacePtrIn, RHadrons* rHadronsPtrIn, 
  UserHooks* userHooksPtrIn,
  MergingHooks* mergingHooksPtrIn, bool useAsTrial ) {

  // Store input pointers and modes for future use. 
  infoPtr            = infoPtrIn;
  particleDataPtr    = particleDataPtrIn;  
  rndmPtr            = rndmPtrIn;
  beamAPtr           = beamAPtrIn;
  beamBPtr           = beamBPtrIn;
  beamHadAPtr        = beamAPtr;
  beamHadBPtr        = beamBPtr;
  beamPomAPtr        = beamPomAPtrIn;
  beamPomBPtr        = beamPomBPtrIn;
  couplingsPtr       = couplingsPtrIn;
  partonSystemsPtr   = partonSystemsPtrIn;
  timesDecPtr        = timesDecPtrIn;
  timesPtr           = timesPtrIn;
  spacePtr           = spacePtrIn; 
  rHadronsPtr        = rHadronsPtrIn; 
  userHooksPtr       = userHooksPtrIn;

  mergingHooksPtr = mergingHooksPtrIn;

  // Min bias and single diffraction processes need special treatment.
  doMinBias          =  settings.flag("SoftQCD:all") 
                     || settings.flag("SoftQCD:minBias");
  doDiffraction      =  settings.flag("SoftQCD:all") 
                     || settings.flag("SoftQCD:singleDiffractive")
                     || settings.flag("SoftQCD:doubleDiffractive");

  // Separate low-mass (unresolved) and high-mass (perturbative) diffraction.
  mMinDiff           = settings.parm("Diffraction:mMinPert");
  mWidthDiff         = settings.parm("Diffraction:mWidthPert");
  pMaxDiff           = settings.parm("Diffraction:probMaxPert");
  if (mMinDiff + mWidthDiff > infoPtr->eCM()) doDiffraction = false;

  // Need MPI initialization for soft QCD processes, even if only first MPI.
  // But no need to initialize MPI if never going to use it.
  doMPI              = settings.flag("PartonLevel:MPI");
  doMPIMB            = doMPI;
  doMPISDA           = doMPI;
  doMPISDB           = doMPI;
  doMPIinit          = doMPI;
  if (doMinBias || doDiffraction) doMPIinit = true;
  if (!settings.flag("PartonLevel:all")) doMPIinit = false;  

//   doTrial            = settings.flag("PartonLevel:Trial");
//   nTrialEmissions    = (doTrial) ? settings.mode("PartonLevel:nTrial") : -1;
  bool hasMergingHooks = (mergingHooksPtr > 0);
  doMerging          =  settings.flag("Merging:doUserMerging")
                     || settings.flag("Merging:doMGMerging")
                     || settings.flag("Merging:doKTMerging");
  doMerging          = doMerging && hasMergingHooks;
  doTrial            = useAsTrial;
  doMergeFirstEmm    = doMerging && !doTrial;
  nTrialEmissions    = 1;
  pTLastBranch       = 0.0;
  typeLastBranch     = 0;

  // Flags for showers: ISR and FSR.
  doISR              = settings.flag("PartonLevel:ISR");
  bool FSR           = settings.flag("PartonLevel:FSR");
  bool FSRinProcess  = settings.flag("PartonLevel:FSRinProcess");
  bool interleaveFSR = settings.flag("TimeShower:interleave");
  doFSRduringProcess = FSR && FSRinProcess &&  interleaveFSR;
  doFSRafterProcess  = FSR && FSRinProcess && !interleaveFSR;
  doFSRinResonances  = FSR && settings.flag("PartonLevel:FSRinResonances");

  // Some other flags.
  doRemnants         = settings.flag("PartonLevel:Remnants");
  doSecondHard       = settings.flag("SecondHard:generate");

  // Allow R-hadron formation.
  allowRH            = settings.flag("RHadrons:allow");

  // Flag if lepton beams, and if non-resolved ones. May change main flags.
  hasLeptonBeams     = ( beamAPtr->isLepton() || beamBPtr->isLepton() );
  hasPointLeptons    = ( hasLeptonBeams 
    && (beamAPtr->isUnresolved() || beamBPtr->isUnresolved() ) );
  if (hasLeptonBeams) {
    doMPIMB          = false;
    doMPISDA         = false;
    doMPISDB         = false;
    doMPIinit        = false;
  }
  if (hasPointLeptons) {
    doISR            = false;
    doRemnants       = false;
  }

  // Possibility to allow user veto during evolution.
  canVetoPT      = (userHooksPtr > 0) ? userHooksPtr->canVetoPT()   : false;
  pTvetoPT       = (canVetoPT)        ? userHooksPtr->scaleVetoPT() : -1.;
  canVetoStep    = (userHooksPtr > 0) ? userHooksPtr->canVetoStep() : false;
  nVetoStep      = (canVetoStep)   ? userHooksPtr->numberVetoStep() : -1;
  canVetoMPIStep = (userHooksPtr > 0) ? userHooksPtr->canVetoMPIStep() : false;
  nVetoMPIStep   = (canVetoStep)   ? userHooksPtr->numberVetoMPIStep() : -1;

  // Possibility to set maximal shower scale in resonance decays.
  canSetScale    = (userHooksPtr > 0) ? userHooksPtr->canSetResonanceScale() 
                                      : false;

  // Set info and initialize the respective program elements.
  timesPtr->init( beamAPtr, beamBPtr);
  if (doISR) spacePtr->init( beamAPtr, beamBPtr);
  doMPIMB  =  multiMB.init( doMPIinit, 0, infoPtr, settings, particleDataPtr,
    rndmPtr, beamAPtr, beamBPtr, couplingsPtr, partonSystemsPtr, sigmaTotPtr,
    userHooksPtr);
  if (doDiffraction) doMPISDA = multiSDA.init( doMPIinit, 1, infoPtr, 
    settings, particleDataPtr, rndmPtr, beamPomBPtr, beamAPtr, couplingsPtr,
    partonSystemsPtr, sigmaTotPtr, userHooksPtr);
  if (doDiffraction) doMPISDB = multiSDB.init( doMPIinit, 2, infoPtr, 
    settings, particleDataPtr, rndmPtr, beamPomAPtr, beamBPtr, couplingsPtr, 
    partonSystemsPtr, sigmaTotPtr, userHooksPtr);
  remnants.init( infoPtr, settings, rndmPtr, beamAPtr, beamBPtr, 
    partonSystemsPtr);  

  // Succeeded, or not.
  multiPtr       = &multiMB;
  if (doMPIinit && !doMPIMB) return false;
  if (doMPIinit && doDiffraction && (!doMPISDA || !doMPISDB)) return false;
  if (!doMPIMB || !doMPISDA || !doMPISDB) doMPI = false;
  return true;
}

//--------------------------------------------------------------------------

// Function to reset PartonLevel object for trial shower usage.

void PartonLevel::resetTrial() {

  // Clear input pointers
  partonSystemsPtr->clear();
  beamAPtr->clear();
  beamBPtr->clear();
  beamHadAPtr->clear();
  beamHadBPtr->clear();
  beamPomAPtr->clear();
  beamPomBPtr->clear();

  // Clear last branching return values
  pTLastBranch = 0.0;
  typeLastBranch = 0;

}

//--------------------------------------------------------------------------

// Main routine to do the parton-level evolution.

bool PartonLevel::next( Event& process, Event& event) {

  // Current event classification.
  isResolved     = infoPtr->isResolved();
  isResolvedA    = isResolved;
  isResolvedB    = isResolved;
  isDiffA        = infoPtr->isDiffractiveA();
  isDiffB        = infoPtr->isDiffractiveB();
  isDiff         = isDiffA || isDiffB;
  isDoubleDiff   = isDiffA && isDiffB;
  isSingleDiff   = isDiff && !isDoubleDiff;
  isMinBias      = infoPtr->isMinBias();

  // nHardLoop counts how many hard-scattering subsystems are to be processed.
  // Almost always 1, but elastic and low-mass diffraction gives 0, while
  // double diffraction can give up to 2. Not to be confused with SecondHard.
  int nHardLoop  = 1;
  if (!isResolved) nHardLoop = (isDiff) ? decideResolvedDiff( process) : 0;

  // Handle unresolved subsystems. Done if no resolved ones.
  sizeProcess    = 0;
  sizeEvent      = 0;
  if (!isResolvedA || !isResolvedB) {
    bool physical = setupUnresolvedSys( process, event);
    if (!physical || nHardLoop == 0) return physical;
    sizeProcess  = process.size();
    sizeEvent    = event.size();
  }

  // Number of actual branchings
  int nBranch = 0;
  // Number of desired branchings, negative value means no restriction
  int nBranchMax = (doTrial) ? nTrialEmissions : -1;

  // Big outer loop to handle up to two systems (in double diffraction),
  // but normally one. (Not indented in following, but end clearly marked.)
  for (int iHardLoop = 1; iHardLoop <= nHardLoop; ++iHardLoop) { 
    infoPtr->setCounter(20, iHardLoop);
    infoPtr->setCounter(21); 

  // Process and event records can be out of step for diffraction.
  if (iHardLoop == 2) {
    sizeProcess = process.size();
    sizeEvent   = event.size();
    partonSystemsPtr->clear();
  }

  // If you need to restore then do not throw existing diffractive system.
  if (isDiff) {
    event.saveSize();
    event.saveJunctionSize();
  }
   
  // Allow special treatment of diffractive systems.
  if (isDiff) setupResolvedDiff(iHardLoop, process);

  // Prepare to do multiparton interactions; at new mass for diffraction.
  if (doMPIinit) multiPtr->reset();

  // Special case if minimum bias: do hardest interaction.
  if (isMinBias || isDiff) {
    multiPtr->pTfirst();
    multiPtr->setupFirstSys( process);
  }

  // Allow up to ten tries; failure possible for beam remnants.
  // Main cause: inconsistent colour flow at the end of the day.
  bool physical = true;
  int  nRad     = 0;
  for (int iTry = 0; iTry < NTRY; ++ iTry) {
    infoPtr->addCounter(21); 
    for (int i = 22; i < 32; ++i) infoPtr->setCounter(i); 

    // Reset flag, counters and max scales.
    physical   = true;
    nMPI       = (doSecondHard) ? 2 : 1;
    nISR       = 0;
    nFSRinProc = 0;
    nFSRinRes  = 0;
    nISRhard   = 0;
    nFSRhard   = 0; 
    pTsaveMPI  = 0.;
    pTsaveISR  = 0.;
    pTsaveFSR  = 0.;

    // Identify hard interaction system for showers.
    setupHardSys( iHardLoop, process, event);

    // Optionally check for a veto after the hardest interaction.
    if (canVetoMPIStep) {
      doVeto = userHooksPtr->doVetoMPIStep( 1, event);
      // Abort event if vetoed.
      if (doVeto) {
        if (isDiff) leaveResolvedDiff( iHardLoop, event);
        return false;
      }
    }

    // Check matching of process scale to maximum ISR/FSR/MPI scales. 
    double Q2Fac       = infoPtr->Q2Fac(); 
    double Q2Ren       = infoPtr->Q2Ren(); 
    bool limitPTmaxISR = (doISR) 
      ? spacePtr->limitPTmax( event, Q2Fac, Q2Ren) : false;
    bool limitPTmaxFSR = (doFSRduringProcess) 
      ? timesPtr->limitPTmax( event, Q2Fac, Q2Ren) : false;
    bool limitPTmaxMPI  = (doMPI)  ? multiPtr->limitPTmax( event) : false;

    // Set hard scale, maximum for showers and multiparton interactions,
    double pTscale  = process.scale();
    if (doSecondHard) pTscale = max( pTscale, process.scaleSecond() );  
    double pTmaxMPI = (limitPTmaxMPI)  ? pTscale : infoPtr->eCM();
    double pTmaxISR = (limitPTmaxISR) ? spacePtr->enhancePTmax() * pTscale 
                                      : infoPtr->eCM();
    double pTmaxFSR = (limitPTmaxFSR) ? timesPtr->enhancePTmax() * pTscale 
                                      : infoPtr->eCM();
    double pTmax    = max( pTmaxMPI, max( pTmaxISR, pTmaxFSR) );
    pTsaveMPI       = pTmaxMPI;
    pTsaveISR       = pTmaxISR;
    pTsaveFSR       = pTmaxFSR;

    // Prepare the classes to begin the generation.
    if (doMPI) multiPtr->prepare( event, pTmaxMPI);
    if (doISR) spacePtr->prepare( 0, event, limitPTmaxISR);
    if (doFSRduringProcess) timesPtr->prepare( 0, event, limitPTmaxFSR);
    if (doSecondHard && doISR) spacePtr->prepare( 1, event, limitPTmaxISR);
    if (doSecondHard && doFSRduringProcess) 
      timesPtr->prepare( 1, event, limitPTmaxFSR);

    // Impact parameter has now been chosen, except for diffraction.
    if (!isDiff) infoPtr->setImpact( multiPtr->bMPI(), multiPtr->enhanceMPI());

    // Set up initial veto scale.
    doVeto        = false;
    double pTveto = pTvetoPT;
    typeLatest    = 0;

    // Begin evolution down in pT from hard pT scale.  
    do {
      infoPtr->addCounter(22); 
      typeVetoStep = 0;
      nRad         =  nISR + nFSRinProc;

      // Find next pT value for FSR, MPI and ISR.
      // Order calls to minimize time expenditure.
      double pTgen = 0.;
      double pTtimes = (doFSRduringProcess) 
        ? timesPtr->pTnext( event, pTmaxFSR, pTgen) : -1.;
      pTgen = max( pTgen, pTtimes);
      double pTmulti = (doMPI) 
        ? multiPtr->pTnext( pTmaxMPI, pTgen, event) : -1.;
      pTgen = max( pTgen, pTmulti);
      double pTspace = (doISR) 
        ? spacePtr->pTnext( event, pTmaxISR, pTgen, nRad) : -1.;
      double pTnow = max( pTtimes, max( pTmulti, pTspace));
      infoPtr->setPTnow( pTnow);

      // Allow a user veto. Only do it once, so remember to change pTveto.
      if (pTveto > 0. && pTveto > pTnow) {
        pTveto = -1.; 
        doVeto = userHooksPtr->doVetoPT( typeLatest, event);
        // Abort event if vetoed.
        if (doVeto) {
          if (isDiff) leaveResolvedDiff( iHardLoop, event);
          return false;
        }
      }

      // Do a multiparton interaction (if allowed). 
      if (pTmulti > 0. && pTmulti > pTspace && pTmulti > pTtimes) {
        infoPtr->addCounter(23); 
        if (multiPtr->scatter( event)) {
          typeLatest = 1;
          ++nMPI;
          if (canVetoMPIStep && nMPI <= nVetoMPIStep) typeVetoStep = 1;
 
          // Update ISR and FSR dipoles.
          if (doISR)              spacePtr->prepare( nMPI - 1, event);
          if (doFSRduringProcess) timesPtr->prepare( nMPI - 1, event);
        }

        // Set maximal scales for next pT to pick.
        pTmaxMPI = pTmulti;
        pTmaxISR = min( pTmulti, pTmaxISR);
        pTmaxFSR = min( pTmulti, pTmaxFSR);
        pTmax    = pTmulti;

        nBranch++;
        pTLastBranch = pTmulti;
        typeLastBranch = 1;
      }
   
      // Do an initial-state emission (if allowed).
      else if (pTspace > 0. && pTspace > pTtimes) { 
        infoPtr->addCounter(24); 
        if (spacePtr->branch( event)) {
          typeLatest = 2;
          iSysNow = spacePtr->system();
          ++nISR;
          if (iSysNow == 0) ++nISRhard;
          if (canVetoStep && iSysNow == 0 && nISRhard <= nVetoStep)
            typeVetoStep = 2;

          // Update FSR dipoles.
          if (doFSRduringProcess) timesPtr->update( iSysNow, event); 

          nBranch++;
          pTLastBranch = pTspace;
          typeLastBranch = 2;

        // Rescatter: it is possible for kinematics to fail, in which
        //            case we need to restart the parton level processing.
        } else if (spacePtr->doRestart()) {
          physical = false;
          break;
        }

        // Set maximal scales for next pT to pick.
        pTmaxMPI = min( pTspace, pTmaxMPI);
        pTmaxISR = pTspace;
        pTmaxFSR = min( pTspace, pTmaxFSR);
        pTmax    = pTspace;
      }

      // Do a final-state emission (if allowed).
      else if (pTtimes > 0.) {
        infoPtr->addCounter(25); 
        if (timesPtr->branch( event, true)) {
          typeLatest = 3;
          iSysNow = timesPtr->system();
          ++nFSRinProc;
          if (iSysNow == 0) ++nFSRhard;
          if (canVetoStep && iSysNow == 0 && nFSRhard <= nVetoStep)
            typeVetoStep = 3;

          // Update ISR dipoles.
          if (doISR) spacePtr->update( iSysNow, event); 

          nBranch++;
          pTLastBranch = pTtimes;
          typeLastBranch = 3;

        }

        // Set maximal scales for next pT to pick.
        pTmaxMPI = min( pTtimes, pTmaxMPI);
        pTmaxISR = min( pTtimes, pTmaxISR);
        pTmaxFSR = pTtimes;
        pTmax    = pTtimes;
      }
   
      // If no pT scales above zero then nothing to be done.
      else pTmax = 0.;

      // Optionally check for a veto after the first few interactions,
      // or after the first few emissions, ISR or FSR, in the hardest system.
      if (typeVetoStep == 1) {
        doVeto = userHooksPtr->doVetoMPIStep( nMPI, event);
      } else if (typeVetoStep > 1) {
        doVeto = userHooksPtr->doVetoStep( typeVetoStep, nISRhard, 
          nFSRhard, event);
      }

      // Abort event if vetoed.
      if (doVeto) {
        if (isDiff) leaveResolvedDiff( iHardLoop, event);
        return false;
      }

      // Keep on evolving until nothing is left to be done.
      if (typeLatest > 0 && typeLatest < 4) 
        infoPtr->addCounter(25 + typeLatest); 
      infoPtr->setPartEvolved( nMPI, nISR);

     if(doMergeFirstEmm && (nISRhard + nFSRhard == 1)){
       // Get number of clustering steps
       int nSteps  = mergingHooksPtr->getNumberOfClusteringSteps(process);
       // Get maximal number of additional jets
       int nJetMax = mergingHooksPtr->nMaxJets();
       // Get merging scale value
       double tms  = mergingHooksPtr->tms();
       // Get merging scale in current event
       double tnow = 0.;
       if(mergingHooksPtr->doKTMerging() || mergingHooksPtr->doMGMerging())
         tnow = mergingHooksPtr->kTms(event);
       else
         tnow = mergingHooksPtr->tmsDefinition(event);
       // Check veto condition
       if(nSteps < nJetMax && tnow > tms){
         mergingHooksPtr->setWeight(0.);
         doVeto = true;
       }
       // Abort event if vetoed.
       if (doVeto) {
         if (isDiff) leaveResolvedDiff( iHardLoop, event);
         return false;
       }
     }

    } while (pTmax > 0.  && (nBranchMax <= 0 || nBranch < nBranchMax) );

    // Do all final-state emissions if not already considered above.
    if (doFSRafterProcess && (nBranchMax <= 0 || nBranch < nBranchMax) ) {

      // Find largest scale for final partons.
      pTmax = 0.;
      for (int i = 0; i < event.size(); ++i) 
        if (event[i].isFinal() && event[i].scale() > pTmax)
          pTmax = event[i].scale();     
      pTsaveFSR = pTmax;

      // Prepare all subsystems for evolution.
      for (int iSys = 0; iSys < partonSystemsPtr->sizeSys(); ++iSys)
        timesPtr->prepare( iSys, event);

      // Set up initial veto scale.
      doVeto        = false;
      pTveto = pTvetoPT;

      // Begin evolution down in pT from hard pT scale. 
      do {
        infoPtr->addCounter(29);
        typeVetoStep = 0;
        double pTtimes = timesPtr->pTnext( event, pTmax, 0.);
        infoPtr->setPTnow( pTtimes);

        // Allow a user veto. Only do it once, so remember to change pTveto.
        if (pTveto > 0. && pTveto > pTtimes) {
          pTveto = -1.; 
          doVeto = userHooksPtr->doVetoPT( 4, event);
          // Abort event if vetoed.
          if (doVeto) {
            if (isDiff) leaveResolvedDiff( iHardLoop, event);
            return false;
          }
        }

        // Do a final-state emission (if allowed).
        if (pTtimes > 0.) {
          infoPtr->addCounter(30);
          if (timesPtr->branch( event, true)) {
            iSysNow = timesPtr->system();
            ++nFSRinProc; 
            if (iSysNow == 0) ++nFSRhard;
            if (canVetoStep && iSysNow == 0 && nFSRhard <= nVetoStep)
            typeVetoStep = 4;

            nBranch++;
            pTLastBranch = pTtimes;
            typeLastBranch = 4;

          }
          pTmax = pTtimes;
        }
    
        // If no pT scales above zero then nothing to be done.
        else pTmax = 0.;

        // Optionally check for a veto after the first few emissions.
        if (typeVetoStep > 0) {
          doVeto = userHooksPtr->doVetoStep( typeVetoStep, nISRhard, 
            nFSRhard, event);
          // Abort event if vetoed.
          if (doVeto) {
            if (isDiff) leaveResolvedDiff( iHardLoop, event);
            return false;
          }
        }

       if(doMergeFirstEmm && (nISRhard + nFSRhard == 1)){
         // Get number of clustering steps
         int nSteps  = mergingHooksPtr->getNumberOfClusteringSteps(process);
         // Get maximal number of additional jets
         int nJetMax = mergingHooksPtr->nMaxJets();
         // Get merging scale value
         double tms  = mergingHooksPtr->tms();
         // Get merging scale in current event
         double tnow = 0.;
         if(mergingHooksPtr->doKTMerging() || mergingHooksPtr->doMGMerging())
           tnow = mergingHooksPtr->kTms(event);
         else
           tnow = mergingHooksPtr->tmsDefinition(event);
         // Check veto condition
         if(nSteps < nJetMax && tnow > tms){
           mergingHooksPtr->setWeight(0.);
           doVeto = true;
         }
         // Abort event if vetoed.
         if (doVeto) {
           if (isDiff) leaveResolvedDiff( iHardLoop, event);
           return false;
         }
       }

      // Keep on evolving until nothing is left to be done.
        infoPtr->addCounter(31);
      } while (pTmax > 0.  && (nBranchMax <= 0 || nBranch < nBranchMax) );
    }

    // Add beam remnants, including primordial kT kick and colour tracing.
    if (physical && doRemnants && !remnants.add( event)) physical = false;

    // If no problems then done.
    if (physical) break;

    // Else restore and loop, but do not throw existing diffractive system.
    if (!isDiff) event.clear();
    else { 
      event.restoreSize();
      event.restoreJunctionSize();
    }
    beamAPtr->clear();
    beamBPtr->clear();
    partonSystemsPtr->clear();

  // End loop over ten tries. Restore from diffraction. Hopefully it worked.
  }
  if (isDiff) leaveResolvedDiff( iHardLoop, event);
  if (!physical) return false;

  // End big outer loop to handle two systems in double diffraction.
  }
  
  // Perform showers in resonance decay chains.
  if(nBranchMax <= 0 || nBranch < nBranchMax)
    doVeto = !resonanceShowers( process, event, true);
  // Abort event if vetoed.
  if (doVeto) return false;

  // Store event properties. Impact parameter not available for diffraction.
  infoPtr->setEvolution( pTsaveMPI, pTsaveISR, pTsaveFSR, 
    nMPI, nISR, nFSRinProc, nFSRinRes);
  if (isDiff) {
    multiPtr->setEmpty(); 
    infoPtr->setImpact( multiPtr->bMPI(), multiPtr->enhanceMPI());
  }
 
  // Done.
  return true;
}

//--------------------------------------------------------------------------

// Decide which diffractive subsystems are resolved (= perturbative). 

int PartonLevel::decideResolvedDiff( Event& process) {

  // Loop over two systems.
  int nHighMass = 0;
  for (int iDiffSys = 1; iDiffSys <= 2; ++iDiffSys) {
    int iDiffMot = iDiffSys + 2;

    // Only high-mass diffractive systems should be resolved.
    double mDiff = process[iDiffMot].m();
    bool isHighMass = ( mDiff > mMinDiff && rndmPtr->flat() 
      < pMaxDiff * ( 1. - exp( -(mDiff - mMinDiff) / mWidthDiff ) ) );

    // Set outcome and done.
    if (isHighMass) ++nHighMass;
    if (iDiffSys == 1) isResolvedA = isHighMass;
    if (iDiffSys == 2) isResolvedB = isHighMass;
  }
  return nHighMass;

}

//--------------------------------------------------------------------------

// Set up an unresolved process, i.e. elastic or diffractive.

bool PartonLevel::setupUnresolvedSys( Event& process, Event& event) {

  // No hard scale in event.
  process.scale( 0.);

  // Copy particles from process to event.
  for (int i = 0; i < process.size(); ++ i) event.append( process[i]);

  // Loop to find diffractively excited beams.
  for (int i = 0; i < 2; ++i)  
  if ( (i == 0 && isDiffA && !isResolvedA) 
    || (i == 1 && isDiffB && !isResolvedB) ) {
    int iBeam = i + 3;
    BeamParticle* beamPtr = (i == 0) ? beamAPtr : beamBPtr;

    // Diffractive mass. Reconstruct boost and rotation to event cm frame.
    double mDiff  = process[iBeam].m();  
    double m2Diff = mDiff*mDiff;  
    double beta   = process[iBeam].pAbs() / process[iBeam].e();
    double theta  = process[iBeam].theta();
    double phi    = process[iBeam].phi();
  
    // Pick quark or gluon kicked out and flavour subdivision.
    bool gluonIsKicked = beamPtr->pickGluon(mDiff);
    int id1 = beamPtr->pickValence();
    int id2 = beamPtr->pickRemnant();

    // Find flavour masses. Scale them down if too big.
    double m1 = particleDataPtr->constituentMass(id1);
    double m2 = particleDataPtr->constituentMass(id2);
    if (m1 + m2 > 0.5 * mDiff) { 
      double reduce = 0.5 * mDiff / (m1 + m2);
      m1 *= reduce;
      m2 *= reduce;
    }

    // If quark is kicked out, then trivial kinematics in rest frame.
    if (!gluonIsKicked) { 
      double pAbs = sqrt( pow2(m2Diff - m1*m1 - m2*m2) 
        - pow2(2. * m1 * m2) ) / (2. * mDiff);
      double e1 = (m2Diff + m1*m1 - m2*m2) / (2. * mDiff);
      double e2 = (m2Diff + m2*m2 - m1*m1) / (2. * mDiff);
      Vec4 p1(0.,0., -pAbs, e1);
      Vec4 p2(0.,0., pAbs, e2);

      // Boost and rotate to event cm frame.
      p1.bst(0., 0., beta); p1.rot(theta, phi);   
      p2.bst(0., 0., beta); p2.rot(theta, phi);   

      // Set colours.
      int col1, acol1, col2, acol2;
      if (particleDataPtr->colType(id1) == 1) {
        col1 = event.nextColTag(); acol1 = 0;
        col2 = 0; acol2 = col1;
      } else {  
        col1 = 0; acol1 = event.nextColTag();
        col2 = acol1; acol2 = 0;
      }  
     // Update process colours to stay in step.
      process.nextColTag();  
    
      // Store partons of diffractive system and mark system decayed.
      int iDauBeg = event.append( id1, 23, iBeam, 0, 0, 0, col1, acol1, 
        p1, m1);
      int iDauEnd = event.append( id2, 63, iBeam, 0, 0, 0, col2, acol2, 
        p2, m2);
      event[iBeam].statusNeg();
      event[iBeam].daughters(iDauBeg, iDauEnd);   


    // If gluon is kicked out: share momentum between two remnants.
    } else {
      double m2Sys, zSys, pxSys, pySys, mTS1, mTS2;
      zSys = beamPtr->zShare(mDiff, m1, m2);

      // Provide relative pT kick in remnant. Construct (transverse) masses.
      pxSys = beamPtr->pxShare(); 
      pySys = beamPtr->pyShare(); 
      mTS1  = m1*m1 + pxSys*pxSys + pySys*pySys;
      mTS2  = m2*m2 + pxSys*pxSys + pySys*pySys;
      m2Sys = mTS1 / zSys + mTS2 / (1. - zSys);

      // Momentum of kicked-out massless gluon in diffractive rest frame.
      double pAbs = (m2Diff - m2Sys) / (2. * mDiff);
      Vec4 pG(0., 0., -pAbs, pAbs);
      Vec4 pRem(0., 0., pAbs, mDiff - pAbs);

      // Momenta of the two beam remnant flavours. (Lightcone p+ = m_diff!)
      double e1 = 0.5 * (zSys * mDiff + mTS1 / (zSys * mDiff));    
      double pL1 = 0.5 * (zSys * mDiff - mTS1 / (zSys * mDiff));  
      Vec4 p1(pxSys, pySys, pL1, e1);
      Vec4 p2 = pRem - p1;
  
      // Boost and rotate to event cm frame.
      pG.bst(0., 0., beta); pG.rot(theta, phi);   
      p1.bst(0., 0., beta); p1.rot(theta, phi);   
      p2.bst(0., 0., beta); p2.rot(theta, phi); 

      // Set colours.
      int colG, acolG, col1, acol1, col2, acol2;
      if (particleDataPtr->colType(id1) == 1) {
        col1 = event.nextColTag(); acol1 = 0;
        colG = event.nextColTag(); acolG = col1;
        col2 = 0; acol2 = colG;
      } else {  
        col1 = 0; acol1 = event.nextColTag();
        colG = acol1; acolG = event.nextColTag();
        col2 = acolG; acol2 = 0;
      } 
      // Update process colours to stay in step.
      process.nextColTag();
      process.nextColTag();
             
      // Store partons of diffractive system and mark system decayed.
      int iDauBeg = event.append( 21, 23, iBeam, 0, 0, 0, colG, acolG, 
        pG, m1);
      event.append( id1, 63, iBeam, 0, 0, 0, col1, acol1, p1, m1);
      int iDauEnd = event.append( id2, 63, iBeam, 0, 0, 0, col2, acol2, 
        p2, m2);
      event[iBeam].statusNeg();
      event[iBeam].daughters(iDauBeg, iDauEnd);   
    }

  // End loop over beams. Done. 
  }
  return true;

}

//--------------------------------------------------------------------------

// Set up the hard process(es), excluding subsequent resonance decays.

  void PartonLevel::setupHardSys( int iHardLoop, Event& process, 
    Event& event) {

  // Incoming partons to hard process are stored in slots 3 and 4.
  int inS = 0;
  int inP = 3;
  int inM = 4;

  // Identify any diffractive system, mother, last entry. Offset.
  int iDiffSys =  (iHardLoop == 2 || !isResolvedA) ? 2 : 1; 
  int iDiffMot = iDiffSys + 2;
  int iDiffDau = process.size() - 1; 
  int nOffset  = sizeEvent - sizeProcess;

  // Resolved diffraction means more entries.
   if (isDiff) {
    inS   = iDiffMot;
    inP   = iDiffDau - 3;
    inM   = iDiffDau - 2;

    // Diffractively excited particle described as Pomeron-hadron beams.
    event[inS].statusNeg();
    event[inS].daughters( inP - 2 + nOffset, inM - 2 + nOffset);
  }

  // If two hard interactions then find where second begins.
  int iBeginSecond = process.size();
  if (doSecondHard) {
    iBeginSecond = 5;
    while (process[iBeginSecond].status() != -21) ++iBeginSecond;
  }

  // If incoming partons are massive then recalculate to put them massless.
  if (process[inP].m() != 0. || process[inM].m() != 0.) { 
    double pPos = process[inP].pPos() + process[inM].pPos();
    double pNeg = process[inP].pNeg() + process[inM].pNeg(); 
    process[inP].pz( 0.5 * pPos);
    process[inP].e(  0.5 * pPos);
    process[inP].m(  0.);
    process[inM].pz(-0.5 * pNeg);
    process[inM].e(  0.5 * pNeg);
    process[inM].m(  0.);
  }

  // Add incoming hard-scattering partons to list in beam remnants.
  double x1 = process[inP].pPos() / process[inS].m();
  beamAPtr->append( inP + nOffset, process[inP].id(), x1);
  double x2 = process[inM].pNeg() / process[inS].m();
  beamBPtr->append( inM + nOffset, process[inM].id(), x2);

  // Scale. Find whether incoming partons are valence or sea. Store.
  // When an x-dependent matter profile is used with minBias,
  // trial interactions mean that the valence/sea choice has already
  // been made and should be restored here. 
  double scale = process.scale();
  int vsc1, vsc2;
  beamAPtr->xfISR( 0, process[inP].id(), x1, scale*scale);
  if (isMinBias && (vsc1 = multiPtr->getVSC1()) != 0)
    (*beamAPtr)[0].companion(vsc1);
  else vsc1 = beamAPtr->pickValSeaComp();
  beamBPtr->xfISR( 0, process[inM].id(), x2, scale*scale);
  if (isMinBias && (vsc2 = multiPtr->getVSC2()) != 0)
    (*beamBPtr)[0].companion(vsc2);
  else vsc2 = beamBPtr->pickValSeaComp();
  bool isVal1 = (vsc1 == -3);
  bool isVal2 = (vsc2 == -3);
  infoPtr->setValence( isVal1, isVal2);

  // Initialize info needed for subsequent sequential decays + showers.
  nHardDone = sizeProcess;
  iPosBefShow.resize( process.size() );
  fill (iPosBefShow.begin(),iPosBefShow.end(),0);

  // Add the beam and hard subprocess partons to the event record.
  for (int i = sizeProcess; i < iBeginSecond; ++ i) { 
    if (process[i].mother1() > inM) break;
    int j = event.append(process[i]);
    iPosBefShow[i] = i;

    // Offset history if process and event not in step.
    if (nOffset != 0) {
      int iOrd = i - iBeginSecond + 7;
      if (iOrd == 1 || iOrd == 2) 
        event[j].offsetHistory( 0, 0, 0, nOffset);
      else if (iOrd == 3 || iOrd == 4) 
        event[j].offsetHistory( 0, nOffset, 0, nOffset);
      else if (iOrd == 5 || iOrd == 6) 
        event[j].offsetHistory( 0, nOffset, 0, 0);
    } 

    // Currently outgoing ones should not count as decayed.
    if (event[j].status() == -22) { 
      event[j].statusPos(); 
      event[j].daughters(0, 0);
    }

    // Complete task of copying hard subsystem into event record.
    ++nHardDone;
  }

  // Store participating partons as first set in list of all systems.
  partonSystemsPtr->addSys();
  partonSystemsPtr->setInA(0, inP + nOffset);
  partonSystemsPtr->setInB(0, inM + nOffset);
  for (int i = inM + 1; i < nHardDone; ++i) 
    partonSystemsPtr->addOut(0, i + nOffset);   
  partonSystemsPtr->setSHat( 0, 
    (event[inP + nOffset].p() + event[inM + nOffset].p()).m2Calc() );

  // Identify second hard process where applicable.
  // Since internally generated incoming partons are guaranteed massless. 
  if (doSecondHard) {
    int inP2 = iBeginSecond; 
    int inM2 = iBeginSecond + 1;

    // Add incoming hard-scattering partons to list in beam remnants.
    // Not valid if not in rest frame??
    x1 = process[inP2].pPos() / process[0].e();
    beamAPtr->append( inP2, process[inP2].id(), x1);
    x2 = process[inM2].pNeg() / process[0].e();
    beamBPtr->append( inM2, process[inM2].id(), x2);

    // Find whether incoming partons are valence or sea.
    scale = process.scaleSecond();
    beamAPtr->xfISR( 1, process[inP2].id(), x1, scale*scale);
    beamAPtr->pickValSeaComp(); 
    beamBPtr->xfISR( 1, process[inM2].id(), x2, scale*scale);
    beamBPtr->pickValSeaComp(); 

    // Add the beam and hard subprocess partons to the event record.
    for (int i = inP2; i < process.size(); ++ i) { 
      int mother = process[i].mother1();
      if ( (mother > 2 && mother < inP2) || mother > inM2 ) break;
      event.append(process[i]);
      iPosBefShow[i] = i;

      // Currently outgoing ones should not count as decayed.
      if (event[i].status() == -22) { 
        event[i].statusPos(); 
        event[i].daughters(0, 0);
      }

      // Complete task of copying hard subsystem into event record.
      ++nHardDone;
    }

    // Store participating partons as second set in list of all systems.
    partonSystemsPtr->addSys();
    partonSystemsPtr->setInA(1, inP2);
    partonSystemsPtr->setInB(1, inM2);
    for (int i = inM2 + 1; i < nHardDone; ++i) 
      partonSystemsPtr->addOut(1, i);   
    partonSystemsPtr->setSHat( 1, 
      (event[inP2].p() + event[inM2].p()).m2Calc() );

  // End code for second hard process.
  }

  // Update event colour tag to maximum in whole process.
  int maxColTag = 0;
  for (int i = 0; i < process.size(); ++ i) { 
    if (process[i].col() > maxColTag) maxColTag = process[i].col();
    if (process[i].acol() > maxColTag) maxColTag = process[i].acol();
  }
  event.initColTag(maxColTag); 

  // Copy junctions from process to event.
  for (int i = 0; i < process.sizeJunction(); ++i) 
    event.appendJunction( process.getJunction(i));
  
  // Done. 
}
  
//--------------------------------------------------------------------------

// Resolved diffraction: replace full event with diffractive subsystem.

void PartonLevel::setupResolvedDiff(int iHardLoop, Event& process) {

  // Identify diffractive system, mother, last entry.
  int iDiffSys =  (iHardLoop == 2 || !isResolvedA) ? 2 : 1; 
  int iDiffMot = iDiffSys + 2;
  int iDiffDau = process.size() - 1; 

  // Diffractive system mass.
  double mDiff = process[iDiffMot].m();
  double m2Diff = mDiff * mDiff;

  // Diffractively excited particle described as Pomeron-hadron beams.
  process[iDiffMot].statusNeg();
  process[iDiffMot].daughters( iDiffDau + 1, iDiffDau + 2);
   
  // Set up Pomeron-proton system as if it were the complete collision.
  int    idHad = process[iDiffSys].id();
  double mHad  = process[iDiffSys].m();
  double m2Had = mHad * mHad;
  double m2Pom = (process[2].p() - process[4].p()).m2Calc();
  double mPom  = (m2Pom >= 0.) ? sqrt(m2Pom) : -sqrt(-m2Pom); 
  double eHad  = 0.5 * (m2Diff + m2Had - m2Pom) / mDiff;
  double ePom  = 0.5 * (m2Diff + m2Pom - m2Had) / mDiff;
  double pzHP  = 0.5 * sqrtpos( pow2(m2Diff - m2Had - m2Pom) 
               - 4. * m2Had * m2Pom ) / mDiff; 
  process.append(   990, 13, iDiffMot, 0, 0, 0, 0, 0, 
    0., 0.,  pzHP, ePom, mPom);
  process.append( idHad, 13, iDiffMot, 0, 0, 0, 0, 0, 
    0., 0., -pzHP, eHad, mHad);

  // Reassign multiparton interactions pointer to right object. 
  multiPtr = (iDiffSys == 1) ? &multiSDA : &multiSDB; 

  // Reassign one beam pointer to refer to incoming Pomeron.
  if (iDiffSys == 1) {
    beamAPtr = beamPomBPtr;
    beamBPtr = beamHadAPtr;
  } else {  
    beamAPtr = beamPomAPtr;
    beamBPtr = beamHadBPtr;
  }

  // Beams not found in normal slots 1 and 2.
  int beamOffset = (sizeEvent > 0) ? sizeEvent - 1 : 4;  

  // Reassign beam pointers in other classes.
  timesPtr->reassignBeamPtrs( beamAPtr, beamBPtr, beamOffset); 
  spacePtr->reassignBeamPtrs( beamAPtr, beamBPtr);  
  remnants.reassignBeamPtrs( beamAPtr, beamBPtr);  

  // Pretend that the diffractive system is the whole collision.
  eCMsave = infoPtr->eCM();
  infoPtr->setECM( mDiff);
  beamAPtr->newPzE(  pzHP, ePom);
  beamBPtr->newPzE( -pzHP, eHad);

}

//--------------------------------------------------------------------------

// Resolved diffraction: restore to original behaviour.

void PartonLevel::leaveResolvedDiff( int iHardLoop, Event& event) {

  // Identify diffractive system.
  int iDiffSys =  (iHardLoop == 2 || !isResolvedA) ? 2 : 1; 

  // Reconstruct boost and rotation to event cm frame.
  Vec4 pPom = event[3 - iDiffSys].p() - event[5 - iDiffSys].p(); 
  Vec4 pHad =  event[iDiffSys].p(); 
  RotBstMatrix MtoCM;
  MtoCM.fromCMframe( pPom, pHad);
 
  // Perform rotation and boost on diffractive system.
  int iFirst = (iHardLoop == 1) ? 5 + sizeEvent - sizeProcess : sizeEvent;
  for (int i = iFirst; i < event.size(); ++i) 
    event[i].rotbst( MtoCM);   

  // Restore multiparton interactions pointer to default object.
  multiPtr = &multiMB;

  // Restore beam pointers to incoming hadrons.
  beamAPtr = beamHadAPtr;
  beamBPtr = beamHadBPtr; 

  // Reassign beam pointers in other classes.
  timesPtr->reassignBeamPtrs( beamAPtr, beamBPtr, 0);  
  spacePtr->reassignBeamPtrs( beamAPtr, beamBPtr);  
  remnants.reassignBeamPtrs( beamAPtr, beamBPtr);  

  // Restore cm energy.
  infoPtr->setECM( eCMsave);    
  beamAPtr->newPzE( event[1].pz(), event[1].e());
  beamBPtr->newPzE( event[2].pz(), event[2].e());

}

//--------------------------------------------------------------------------

// Handle showers in successive resonance decays.

bool PartonLevel::resonanceShowers( Event& process, Event& event, 
  bool skipForR) {

  // Prepare to start over from beginning for R-hadron decays.
  if (allowRH) {
    if (skipForR) {
      nHardDoneRHad = nHardDone;
      inRHadDecay.resize(0);
      for (int i = 0; i < process.size(); ++i) 
        inRHadDecay.push_back( false);
    } else nHardDone = nHardDoneRHad;
  }

  // Isolate next system to be processed, if anything remains.
  int nRes    = 0;
  int nFSRres = 0;
  // Number of desired branchings, negative value means no restriction
  int nBranchMax = (doTrial) ? nTrialEmissions : -1;

  while (nHardDone < process.size()) {
    ++nRes;
    int iBegin = nHardDone;

    // In first call (skipForR = true) skip over resonances 
    // that should form R-hadrons, and their daughters.
    if (allowRH) {
      if (skipForR) {
        bool comesFromR = false;
        int iTraceUp = iBegin;
        do {
          if ( rHadronsPtr->givesRHadron(process[iTraceUp].id()) )
            comesFromR = true;
          iTraceUp = process[iTraceUp].mother1();
        } while (iTraceUp > 0 && !comesFromR);
        if (comesFromR) {
          inRHadDecay[iBegin] = true;
          ++nHardDone;
          continue;
        }

      // In optional second call (skipForR = false) process decay chains
      // inside R-hdrons.  
      } else if (!inRHadDecay[iBegin]) {
        ++nHardDone;
        continue;
      }
    }

    // Mother in hard process and in complete event (after shower).
    int iHardMother      = process[iBegin].mother1();
    Particle& hardMother = process[iHardMother];
    int iBefMother       = iPosBefShow[iHardMother];
    int iAftMother       = event.iBotCopyId(iBefMother);
    // Possibly trace across intermediate R-hadron state.
    if (allowRH) {
      int iTraceRHadron    = rHadronsPtr->trace( iAftMother);
      if (iTraceRHadron > 0) iAftMother = iTraceRHadron;
    }
    Particle& aftMother  = event[iAftMother];
   
    // From now on mother counts as decayed.
    aftMother.statusNeg();

    // Mother can have been moved by showering (in any of previous steps), 
    // so prepare to update colour and momentum information for system.
    int colBef  = hardMother.col();
    int acolBef = hardMother.acol();
    int colAft  = aftMother.col();
    int acolAft = aftMother.acol();
    RotBstMatrix M;
    M.bst( hardMother.p(), aftMother.p());

    // Extract next partons from hard event into normal event record.
    for (int i = iBegin; i < process.size(); ++ i) { 
      if (process[i].mother1() != iHardMother) break;
      int iNow = event.append( process[i] );
      iPosBefShow[i] = iNow;
      Particle& now = event.back();

      // Currently outgoing ones should not count as decayed.
      if (now.status() == -22) { 
        now.statusPos(); 
        now.daughters(0, 0);
      }
      
      // Update daughter and mother information.
      if (i == iBegin) event[iAftMother].daughter1( iNow);
      else             event[iAftMother].daughter2( iNow); 
      now.mother1(iAftMother); 

      // Update colour and momentum information.
      if (now.col() == colBef) now.col( colAft);
      if (now.acol() == acolBef) now.acol( acolAft);
      now.rotbst( M);   

      // Update vertex information.
      if (now.hasVertex()) now.vProd( aftMother.vDec() );

      // Complete task of copying next subsystem into event record.
      ++nHardDone;
    }
    int iEnd = nHardDone - 1;

    // Reset pT of last branching
    pTLastBranch = 0.0;    

    // Do parton showers inside subsystem: maximum scale by mother mass.
    if (doFSRinResonances) {
      double pTmax = 0.5 * hardMother.m();
      if (canSetScale) pTmax 
        = userHooksPtr->scaleResonance( iAftMother, event);
      nFSRhard     = 0; 

      // Add new system, automatically with two empty beam slots.
      int iSys = partonSystemsPtr->addSys();
      partonSystemsPtr->setSHat(iSys, pow2(hardMother.m()) );
    
      // Loop over allowed range to find all final-state particles.
      for (int i = iPosBefShow[iBegin]; i <= iPosBefShow[iEnd]; ++i) 
      if (event[i].isFinal()) partonSystemsPtr->addOut( iSys, i);

      // Let prepare routine do the setup.    
      timesDecPtr->prepare( iSys, event);

       // Number of actual branchings
      int nBranch = 0;

      // Set up initial veto scale.
      doVeto        = false;
      double pTveto = pTvetoPT;

      // Begin evolution down in pT from hard pT scale. 
      do {
        typeVetoStep = 0;
        double pTtimes = timesDecPtr->pTnext( event, pTmax, 0.);

        // Allow a user veto. Only do it once, so remember to change pTveto.
        if (pTveto > 0. && pTveto > pTtimes) {
          pTveto = -1.; 
          doVeto = userHooksPtr->doVetoPT( 5, event);
          // Abort event if vetoed.
          if (doVeto) return false;
        }

        // Do a final-state emission (if allowed).
        if (pTtimes > 0.) {
          if (timesDecPtr->branch( event)) {
            ++nFSRres; 
            ++nFSRhard;
            if (canVetoStep && nFSRhard <= nVetoStep) typeVetoStep = 5;

            nBranch++;
            pTLastBranch = pTtimes;
            typeLastBranch = 5;

          }
          pTmax = pTtimes;
        }
    
        // If no pT scales above zero then nothing to be done.
        else pTmax = 0.;

        // Optionally check for a veto after the first few emissions.
        if (typeVetoStep > 0) {
          doVeto = userHooksPtr->doVetoStep( typeVetoStep, 0, nFSRhard, 
            event);
          // Abort event if vetoed.
          if (doVeto) return false;
        }

       if(doMergeFirstEmm && nFSRhard == 1){
         // Get number of clustering steps
         int nSteps  = mergingHooksPtr->getNumberOfClusteringSteps(process);
         // Get maximal number of additional jets
         int nJetMax = mergingHooksPtr->nMaxJets();
         // Get merging scale value
         double tms  = mergingHooksPtr->tms();
         // Get merging scale in current event
         double tnow = 0.;
         if(mergingHooksPtr->doKTMerging() || mergingHooksPtr->doMGMerging())
           tnow = mergingHooksPtr->kTms(event);
         else
           tnow = mergingHooksPtr->tmsDefinition(event);
         // Check veto condition
         if(nSteps < nJetMax && tnow > tms){
           mergingHooksPtr->setWeight(0.);
           doVeto = true;
         }
          // Abort event if vetoed.
          if (doVeto) return false;
       }

      // Keep on evolving until nothing is left to be done.
      } while (pTmax > 0.  && (nBranchMax <= 0 || nBranch < nBranchMax) );

    }    

  // No more systems to be processed. Set total number of emissions.
  }
  if (skipForR) nFSRinRes = nFSRres;
  return true;

}
 
//==========================================================================

} // end namespace Pythia8

// PartonLevel.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Function definitions (not found in the header) for the PartonLevel class.

#include "PartonLevel.h"

namespace Pythia8 {
 
//**************************************************************************

// The PartonLevel class.

//*********

// Constants: could be changed here if desired, but normally should not.
// These are of technical nature, as described for each.

// Maximum number of tries to produce parton level from given input..
const int PartonLevel::NTRY = 10; 

//*********

// Main routine to initialize the parton-level generation process.

bool PartonLevel::init( Info* infoPtrIn, BeamParticle* beamAPtrIn, 
  BeamParticle* beamBPtrIn,  SigmaTotal* sigmaTotPtr, 
  TimeShower* timesDecPtrIn, TimeShower* timesPtrIn, 
  SpaceShower* spacePtrIn, UserHooks* userHooksPtrIn) {

  // Store input pointers and modes for future use. 
  infoPtr            = infoPtrIn;
  beamAPtr           = beamAPtrIn;
  beamBPtr           = beamBPtrIn;
  timesDecPtr        = timesDecPtrIn;
  timesPtr           = timesPtrIn;
  spacePtr           = spacePtrIn;  
  userHooksPtr       = userHooksPtrIn;

  // Main flags.
  doMI               = Settings::flag("PartonLevel:MI");
  doISR              = Settings::flag("PartonLevel:ISR");
  bool FSR           = Settings::flag("PartonLevel:FSR");
  bool FSRinProcess  = Settings::flag("PartonLevel:FSRinProcess");
  bool interleaveFSR = Settings::flag("TimeShower:interleave");
  doFSRduringProcess = FSR && FSRinProcess &&  interleaveFSR;
  doFSRafterProcess  = FSR && FSRinProcess && !interleaveFSR;
  doFSRinResonances  = FSR && Settings::flag("PartonLevel:FSRinResonances");
  doRemnants         = true;
  doSecondHard       = Settings::flag("SecondHard:generate");

  // Need MI initialization for minbias processes, even if only first MI.
  // But no need to initialize MI if never going to use it.
  doMIinit           = doMI;
  if (Settings::flag("SoftQCD:minBias") || Settings::flag("SoftQCD:all"))
    doMIinit = true; 
  if (!Settings::flag("PartonLevel:all")) doMIinit = false;  

  // Flag if lepton beams, and if non-resolved ones. May change main flags.
  hasLeptonBeams  = ( beamAPtr->isLepton() || beamBPtr->isLepton() );
  hasPointLeptons = ( hasLeptonBeams 
    && (beamAPtr->isUnresolved() || beamBPtr->isUnresolved() ) );
  if (hasLeptonBeams) {
    doMI       = false;
    doMIinit   = false;
  }
  if (hasPointLeptons) {
    doISR      = false;
    doRemnants = false;
  }

  // Possibility to allow user veto during evolution.
  canVetoPT   = (userHooksPtr > 0) ? userHooksPtr->canVetoPT()   : false;
  pTvetoPT    = (canVetoPT)        ? userHooksPtr->scaleVetoPT() : -1.;
  canVetoStep = (userHooksPtr > 0) ? userHooksPtr->canVetoStep() : false;
  nVetoStep   = (canVetoStep)   ? userHooksPtr->numberVetoStep() : -1;

  // Set info and initialize the respective program elements.
  timesPtr->init( beamAPtr, beamBPtr);
  if (doISR) spacePtr->init( beamAPtr, beamBPtr);
  doMI = multi.init( doMIinit, infoPtr, beamAPtr, beamBPtr, sigmaTotPtr);
  remnants.init( infoPtr, beamAPtr, beamBPtr);  

  // Succeeded. (Check return values from other classes??)
  return true;
}

//*********

// Main routine to do the parton-level evolution.

bool PartonLevel::next( Event& process, Event& event) {

  // Special case if unresolved = elastic/diffractive event.
  if (!infoPtr->isResolved()) return setupUnresolvedSys( process, event);

  // Special case if minimum bias: do hardest interaction.
  if (doMIinit) multi.clear();
  if (infoPtr->isMinBias()) {
    multi.pTfirst();
    multi.setupFirstSys( process);
  }

  // Allow up to ten tries; failure possible for beam remnants.
  // Main cause: inconsistent colour flow at the end of the day.
  bool physical = true;
  int  nRad     = 0;
  for (int iTry = 0; iTry < NTRY; ++ iTry) {

    // Reset flag, counters and max scales.
    physical   = true;
    nMI        = (doSecondHard) ? 2 : 1;
    nISR       = 0;
    nFSRinProc = 0;
    nFSRinRes  = 0;
    nISRhard   = 0;
    nFSRhard   = 0; 
    pTsaveMI   = 0.;
    pTsaveISR  = 0.;
    pTsaveFSR  = 0.;

    // Identify hard interaction system for showers.
    setupHardSys( process, event);

    // Check matching of process scale to maximum ISR/MI scales. 
    double Q2Fac       = infoPtr->Q2Fac(); 
    double Q2Ren       = infoPtr->Q2Ren(); 
    bool limitPTmaxISR = (doISR) 
      ? spacePtr->limitPTmax( event, Q2Fac, Q2Ren) : false;
    bool limitPTmaxMI  = (doMI)  ? multi.limitPTmax( event) : false;

    // Set hard scale, maximum for showers and multiple interactions,
    double pTscale  = process.scale();
    if (doSecondHard) pTscale = max( pTscale, process.scaleSecond() );  
    double pTmaxMI  = (limitPTmaxMI)  ? pTscale : infoPtr->eCM();
    double pTmaxISR = (limitPTmaxISR) ? spacePtr->enhancePTmax() * pTscale 
                                      : infoPtr->eCM();
    double pTmaxFSR = timesPtr->enhancePTmax() * pTscale;
    double pTmax    = max( pTmaxMI, max( pTmaxISR, pTmaxFSR) );
    pTsaveMI        = pTmaxMI;
    pTsaveISR       = pTmaxISR;
    pTsaveFSR       = pTmaxFSR;

    // Prepare the classes to begin the generation.
    if (doMI)  multi.prepare( pTmaxMI);
    if (doISR) spacePtr->prepare( 0, event, limitPTmaxISR);
    if (doFSRduringProcess) timesPtr->prepare( 0, event);
    if (doSecondHard && doISR) spacePtr->prepare( 1, event, limitPTmaxISR);
    if (doSecondHard && doFSRduringProcess) timesPtr->prepare( 1, event);

    // Set up initial veto scale.
    doVeto        = false;
    double pTveto = pTvetoPT;
    typeLatest    = 0;

    // Begin evolution down in pT from hard pT scale.  
    do {
 
      typeVetoStep = 0;
      nRad         =  nISR + nFSRinProc;

      // Find next pT value for FSR, MI and ISR.
      // Order calls to minimize time expenditure.
      double pTgen = 0.;
      double pTtimes = (doFSRduringProcess) 
        ? timesPtr->pTnext( event, pTmaxFSR, pTgen) : -1.;
      pTgen = max( pTgen, pTtimes);
      double pTmulti = (doMI) 
        ? multi.pTnext( pTmaxMI, pTgen) : -1.;
      pTgen = max( pTgen, pTmulti);
      double pTspace = (doISR) 
        ? spacePtr->pTnext( event, pTmaxISR, pTgen, nRad) : -1.;

      // Allow a user veto. Only do it once, so remember to change pTveto.
      if (pTveto > 0. && pTveto > pTmulti && pTveto > pTspace 
        && pTveto > pTtimes) {
        pTveto = -1.; 
        doVeto = userHooksPtr->doVetoPT( typeLatest, event);
        // Abort event if vetoed.
        if (doVeto) return false;
      }

      // Do a multiple interaction (if allowed).
      if (pTmulti > 0. && pTmulti > pTspace && pTmulti > pTtimes) {
        multi.scatter( event);  
        typeLatest = 1;
        ++nMI;
        if (doISR)              spacePtr->prepare( nMI - 1, event);
        if (doFSRduringProcess) timesPtr->prepare( nMI - 1, event);
        pTmaxMI  = pTmulti;
        pTmaxISR = min( pTmulti, pTmaxISR);
        pTmaxFSR = min( pTmulti, pTmaxFSR);
        pTmax    = pTmulti;
      }
   
      // Do an initial-state emission (if allowed).
      else if (pTspace > 0. && pTspace > pTtimes) { 
        if (spacePtr->branch( event)) {
          typeLatest = 2;
          iSysNow = spacePtr->system();
          ++nISR;
          if (iSysNow == 0) ++nISRhard;
          if (doFSRduringProcess) timesPtr->update( iSysNow, event); 
          if (canVetoStep && iSysNow == 0 && nISRhard <= nVetoStep)
            typeVetoStep = 2;
	}
        pTmaxMI  = min( pTspace, pTmaxMI);
        pTmaxISR = pTspace;
        pTmaxFSR = min( pTspace, pTmaxFSR);
        pTmax    = pTspace;
      }

      // Do a final-state emission (if allowed).
      else if (pTtimes > 0.) {
        if (timesPtr->branch( event)) {
          typeLatest = 3;
          iSysNow = timesPtr->system();
          ++nFSRinProc;
          if (iSysNow == 0) ++nFSRhard;
          if (doISR) spacePtr->update( iSysNow, event); 
          if (canVetoStep && iSysNow == 0 && nFSRhard <= nVetoStep)
            typeVetoStep = 3;
	}
        pTmaxMI  = min( pTtimes, pTmaxMI);
        pTmaxISR = min( pTtimes, pTmaxISR);
        pTmaxFSR = pTtimes;
        pTmax    = pTtimes;
      }
   
      // If no pT scales above zero then nothing to be done.
      else pTmax = 0.;

      // Optionally check for a veto after the first few emissions.
      if (typeVetoStep > 0) {
        doVeto = userHooksPtr->doVetoStep( typeVetoStep, nISRhard, 
          nFSRhard, event);
        // Abort event if vetoed.
        if (doVeto) return false;
      }

    // Keep on evolving until nothing is left to be done.
    } while (pTmax > 0.); 

    // Do all final-state emissions if not already considered above.
    if (doFSRafterProcess) {

      // Find largest scale for final partons.
      pTmax = 0.;
      for (int i = 0; i < event.size(); ++i) 
        if (event[i].isFinal() && event[i].scale() > pTmax)
          pTmax = event[i].scale();     
      pTsaveFSR = pTmax;

      // Prepare all subsystems for evolution.
      for (int iSys = 0; iSys < event.sizeSystems(); ++iSys)
        timesPtr->prepare( iSys, event);

      // Set up initial veto scale.
      doVeto = false;
      pTveto = pTvetoPT;

      // Begin evolution down in pT from hard pT scale. 
      do {
        typeVetoStep = 0;
        double pTtimes = timesPtr->pTnext( event, pTmax, 0.);

        // Allow a user veto. Only do it once, so remember to change pTveto.
        if (pTveto > 0. && pTveto > pTtimes) {
          pTveto = -1.; 
          doVeto = userHooksPtr->doVetoPT( 4, event);
          // Abort event if vetoed.
          if (doVeto) return false;
        }

        // Do a final-state emission (if allowed).
        if (pTtimes > 0.) {
          if (timesPtr->branch( event)) {
            iSysNow = timesPtr->system();
            ++nFSRinProc; 
            if (iSysNow == 0) ++nFSRhard;
            if (canVetoStep && iSysNow == 0 && nFSRhard <= nVetoStep)
              typeVetoStep = 4;
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
          if (doVeto) return false;
        }

      // Keep on evolving until nothing is left to be done.
      } while (pTmax > 0.); 
    }
    
    // Add beam remnants, including primordial kT kick and colour tracing.
    if (doRemnants && !remnants.add( event)) physical = false;
 
    // If no problems then done, else restore and loop.
    if (physical) break;
    event.clear();
    beamAPtr->clear();
    beamBPtr->clear();

  // End loop over ten tries. Hopefully it worked
  }
  if (!physical) return false;

  // Perform showers in resonance decay chains.
  nFSRinRes = resonanceShowers( process, event); 

  // Store event properties.
  infoPtr->setImpact( multi.bMI(), multi.enhanceMI());
  infoPtr->setEvolution( pTsaveMI, pTsaveISR, pTsaveFSR, 
    nMI, nISR, nFSRinProc, nFSRinRes);
 
  // Done.
  return true;
}

//*********

// Set up the hard process(es), excluding subsequent resonance decays.

void PartonLevel::setupHardSys( Event& process, Event& event) {

  // Incoming partons to hard process are stored in slots 3 and 4.
  int inP = 3;
  int inM = 4;

  // If two hard interactions then find where second begins.
  int iBeginSecond = process.size();
  if (doSecondHard) {
    iBeginSecond = 5;
    while (process[iBeginSecond].status() != -21) ++iBeginSecond;
  }

  // If incoming partons are massive then recalculate to put them massless.
  if (process[inP].m() != 0. || process[inM].m() != 0.) { 
    double pPlus  = process[inP].pPlus() + process[inM].pPlus();
    double pMinus = process[inP].pMinus() + process[inM].pMinus(); 
    process[inP].pz( 0.5 * pPlus);
    process[inP].e(  0.5 * pPlus);
    process[inP].m(  0.);
    process[inM].pz(-0.5 * pMinus);
    process[inM].e(  0.5 * pMinus);
    process[inM].m(  0.);
  }

  // Add incoming hard-scattering partons to list in beam remnants.
  double x1 = process[inP].pPlus() / process[0].e();
  beamAPtr->append( inP, process[inP].id(), x1);
  double x2 = process[inM].pMinus() / process[0].e();
  beamBPtr->append( inM, process[inM].id(), x2);

  // Scale. Find whether incoming partons are valence or sea. Store.
  double scale = process.scale();
  beamAPtr->xfISR( 0, process[inP].id(), x1, scale*scale);
  int vsc1 = beamAPtr->pickValSeaComp(); 
  beamBPtr->xfISR( 0, process[inM].id(), x2, scale*scale);
  int vsc2 = beamBPtr->pickValSeaComp();
  bool isVal1 = (vsc1 == -3); 
  bool isVal2 = (vsc2 == -3); 
  infoPtr->setValence( isVal1, isVal2);

  // Initialize info needed for subsequent sequential decays + showers.
  nHardDone = 0;
  iPosBefShow.resize( process.size() );

  // Add the beam and hard subprocess partons to the event record.
  for (int i = 0; i < iBeginSecond; ++ i) { 
    if (process[i].mother1() > inM) break;
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

  // Store participating partons as first set in list of all systems.
  event.newSystem();
  for (int i = inP; i < nHardDone; ++i) event.addToSystem(0, i);   

  // Identify second hard process where applicable.
  // Since internally generated incoming partons are guaranteed massless. 
  if (doSecondHard) {
    int inP2 = iBeginSecond; 
    int inM2 = iBeginSecond + 1;

    // Add incoming hard-scattering partons to list in beam remnants.
    // Not valid if not in rest frame??
    x1 = process[inP2].pPlus() / process[0].e();
    beamAPtr->append( inP2, process[inP2].id(), x1);
    x2 = process[inM2].pMinus() / process[0].e();
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
    event.newSystem();
    for (int i = inP2; i < nHardDone; ++i) event.addToSystem(1, i);   

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

//*********

// Set up an unresolved process, i.e. elastic or diffractive.

bool PartonLevel::setupUnresolvedSys( Event& process, Event& event) {

  // Copy particles from process to event.
  for (int i = 0; i < process.size(); ++ i) event.append( process[i]);

  // Loop to find diffractively excited beams.
  for (int i = 0; i < 2; ++i)  
  if ( (i == 0 && infoPtr->isDiffractiveA()) 
    || (i == 1 && infoPtr->isDiffractiveB()) ) {
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
    double m1 = ParticleDataTable::constituentMass(id1);
    double m2 = ParticleDataTable::constituentMass(id2);
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
      if (ParticleDataTable::colType(id1) == 1) {
        col1 = event.nextColTag(); acol1 = 0;
        col2 = 0; acol2 = col1;
      } else {  
        col1 = 0; acol1 = event.nextColTag();
        col2 = acol1; acol2 = 0;
      }    
    
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
      if (ParticleDataTable::colType(id1) == 1) {
        col1 = event.nextColTag(); acol1 = 0;
        colG = event.nextColTag(); acolG = col1;
        col2 = 0; acol2 = colG;
      } else {  
        col1 = 0; acol1 = event.nextColTag();
        colG = acol1; acolG = event.nextColTag();
        col2 = acolG; acol2 = 0;
      } 
       
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

//*********

// Handle showers in successive resonance decays.

int PartonLevel::resonanceShowers( Event& process, Event& event) {

  // Isolate next system to be processed, if anything remains.
  int nFSRres = 0;
  while (nHardDone < process.size()) {
    int iBegin = nHardDone;

    // Mother in hard process and in complete event (after shower).
    int iHardMother      = process[iBegin].mother1();
    Particle& hardMother = process[iHardMother];
    int iBefMother       = iPosBefShow[iHardMother];
    int iAftMother       = event.iBotCopyId(iBefMother);
    Particle& aftMother  = event[iAftMother];

    // From now mother counts as decayed.
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
      if (i == iBegin) aftMother.daughter1( iNow);
      else aftMother.daughter2( iNow); 
      now.mother1(iAftMother); 

      // Update colour and momentum information.
      if (now.col() == colBef) now.col( colAft);
      if (now.acol() == acolBef) now.acol( acolAft);
      now.rotbst( M);   

      // Complete task of copying next subsystem into event record.
      ++nHardDone;
    }
    int iEnd = nHardDone - 1;

    // Do parton showers inside subsystem: maximum scale by mother mass.
    if (doFSRinResonances) {
      double pTmax = 0.5 * hardMother.m();
      nFSRhard     = 0; 

      // Add new system, with two empty beam slots.
      int iSys = event.newSystem();
      event.addToSystem( iSys, 0);
      event.addToSystem( iSys, 0);
    
      // Loop over allowed range to find all final-state particles.
      for (int i = iPosBefShow[iBegin]; i <= iPosBefShow[iEnd]; ++i) 
      if (event[i].isFinal()) event.addToSystem( iSys, i);

      // Let prepare routine do the setup.    
      timesDecPtr->prepare( iSys, event);

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

      // Keep on evolving until nothing is left to be done.
      } while (pTmax > 0.); 

    }    

  // No more systems to be processed. Return total number of emissions.
  }
  return nFSRres;

}
 
//**************************************************************************

} // end namespace Pythia8

// ProcessContainer.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Function definitions (not found in the header) for the 
// ProcessContainer and SetupContainers classes.

#include "ProcessContainer.h"

// Internal headers for special processes.
#include "SigmaCompositeness.h"
#include "SigmaEW.h"
#include "SigmaExtraDim.h"
#include "SigmaHiggs.h"
#include "SigmaLeftRightSym.h"
#include "SigmaLeptoquark.h"
#include "SigmaNewGaugeBosons.h"
#include "SigmaOnia.h"
#include "SigmaQCD.h"
#include "SigmaSUSY.h"

namespace Pythia8 {

//**************************************************************************

// ProcessContainer class.
// Information allowing the generation of a specific process.

//*********

// Constants: could be changed here if desired, but normally should not.
// These are of technical nature, as described for each.

// Number of event tries to check maximization finding reliability.
const int ProcessContainer::N12SAMPLE = 100;

// Ditto, but increased for 2 -> 3 processes.
const int ProcessContainer::N3SAMPLE  = 1000;

//*********

// Initialize phase space and counters.

bool ProcessContainer::init(Info* infoPtrIn, BeamParticle* beamAPtr, 
  BeamParticle* beamBPtr, AlphaStrong* alphaSPtr, AlphaEM* alphaEMPtr,
  SigmaTotal* sigmaTotPtr, ResonanceDecays* resDecaysPtrIn, 
  SusyLesHouches* slhaPtr, UserHooks* userHooksPtr) {

  // Extract info about current process from SigmaProcess object.
  isLHA       = sigmaProcessPtr->isLHA();
  isMinBias   = sigmaProcessPtr->isMinBias();
  isResolved  = sigmaProcessPtr->isResolved();
  isDiffA     = sigmaProcessPtr->isDiffA();
  isDiffB     = sigmaProcessPtr->isDiffB();
  int nFin    = sigmaProcessPtr->nFinal();
  lhaStrat    = (isLHA) ? lhaUpPtr->strategy() : 0;
  lhaStratAbs = abs(lhaStrat);
  allowNegSig = sigmaProcessPtr->allowNegativeSigma();

  // Pick and create phase space generator. Send pointers where required.
  if      (isLHA)       phaseSpacePtr = new PhaseSpaceLHA();
  else if (isMinBias)   phaseSpacePtr = new PhaseSpace2to2minbias();
  else if (!isResolved && !isDiffA  && !isDiffB )
                        phaseSpacePtr = new PhaseSpace2to2elastic();
  else if (!isResolved) phaseSpacePtr = new PhaseSpace2to2diffractive( 
                                        isDiffA, isDiffB);
  else if (nFin == 1)   phaseSpacePtr = new PhaseSpace2to1tauy();
  else if (nFin == 2)   phaseSpacePtr = new PhaseSpace2to2tauyz();
  else                  phaseSpacePtr = new PhaseSpace2to3tauycyl();

  // Store pointers and perform simple initialization.
  infoPtr      = infoPtrIn;
  resDecaysPtr = resDecaysPtrIn;
  if (isLHA) {
    sigmaProcessPtr->setLHAPtr(lhaUpPtr);
    phaseSpacePtr->setLHAPtr(lhaUpPtr);
  }
  sigmaProcessPtr->init(infoPtr, beamAPtr, beamBPtr, alphaSPtr,
    alphaEMPtr, sigmaTotPtr, slhaPtr);
  phaseSpacePtr->init( sigmaProcessPtr, infoPtr, beamAPtr, 
    beamBPtr, sigmaTotPtr, userHooksPtr);

  // Reset cross section statistics.
  nTry      = 0;
  nSel      = 0;
  nAcc      = 0;
  nTryStat  = 0;
  sigmaMx   = 0.;
  sigmaSum  = 0.;
  sigma2Sum = 0.;
  sigmaNeg  = 0.;
  sigmaAvg  = 0.;
  sigmaFin  = 0.;
  deltaFin  = 0.;

  // Initialize process and allowed incoming partons.
  sigmaProcessPtr->initProc();
  if (!sigmaProcessPtr->initFlux()) return false;

  // Find maximum of differential cross section * phasespace.
  bool physical       = phaseSpacePtr->setupSampling();
  sigmaMx             = phaseSpacePtr->sigmaMax();
  double sigmaHalfWay = sigmaMx;

  // Separate signed maximum needed for LHA with negative weight.
  sigmaSgn            = phaseSpacePtr->sigmaSumSigned();

  // Check maximum by a few events, and extrapolate a further increase.
  if (physical & !isLHA) {
    int nSample = (nFin < 3) ? N12SAMPLE : N3SAMPLE;
    for (int iSample = 0; iSample < nSample; ++iSample) 
    while (!phaseSpacePtr->trialKin(false)) { 
      if (iSample == nSample/2) sigmaHalfWay = phaseSpacePtr->sigmaMax();
    }   
    sigmaMx = pow2(phaseSpacePtr->sigmaMax()) / sigmaHalfWay;
    phaseSpacePtr->setSigmaMax(sigmaMx);
  }

  // Done.
  return physical;
}

//*********

// Generate a trial event; selected or not.
 
bool ProcessContainer::trialProcess() { 

  // Loop over tries only occurs for Les Houches strategy = +-2.
  for (int iTry = 0;  ; ++iTry) {

    // Generate a trial phase space point, if meaningful.
    if (sigmaMx == 0.) return false;
    infoPtr->setEndOfFile(false);
    bool repeatSame = (iTry > 0);
    bool physical = phaseSpacePtr->trialKin(true, repeatSame);

    // Possibly fail, e.g. if at end of Les Houches file, else cross section.
    if (isLHA && !physical) infoPtr->setEndOfFile(true);
    else ++nTry;
    if (!physical) return false;
    double sigmaNow = phaseSpacePtr->sigmaNow(); 

    // Tell if this event comes with negative weight, or weight at all.
    double weight = 1.;
    if ( lhaStrat < 0 && sigmaNow < 0.) weight = -1.;
    if ( lhaStratAbs == 4) weight = sigmaNow;
    infoPtr->setWeight( weight);

    // Check that not negative cross section when not allowed.
    if (!allowNegSig) {
      if (sigmaNow < sigmaNeg) {
        infoPtr->errorMsg("Warning in ProcessContainer::trialProcess: neg"
          "ative cross section set 0", "for " +  sigmaProcessPtr->name() );
        sigmaNeg = sigmaNow;
      }
      if (sigmaNow < 0.) sigmaNow = 0.;
    }

    // Update statistics. Check if maximum violated.
    double sigmaAdd = sigmaNow;
    if (lhaStratAbs == 2 || lhaStratAbs == 3) sigmaAdd = sigmaSgn;    
    sigmaSum  += sigmaAdd;
    sigma2Sum += pow2(sigmaAdd);
    newSigmaMx = phaseSpacePtr->newSigmaMax();
    if (newSigmaMx) sigmaMx = phaseSpacePtr->sigmaMax();

    // Select or reject trial point.
    bool select = true;
    if (lhaStratAbs < 3) select 
      = (newSigmaMx || Rndm::flat() * abs(sigmaMx) < abs(sigmaNow)); 
    if (select) ++nSel;
    if (select || lhaStratAbs !=2) return select;
  }
 
}

//*********
  
// Give the hard subprocess.

bool ProcessContainer::constructProcess( Event& process, bool isHardest) { 

  // Construct flavour and colours for selected event.
  if (isResolved && !isMinBias) sigmaProcessPtr->pickInState();
  sigmaProcessPtr->setIdColAcol();

  // Construct kinematics from selected phase space point.
  if (!phaseSpacePtr->finalKin()) return false;
  int nFin = sigmaProcessPtr->nFinal();

  // Basic info on process.
  if (isHardest) infoPtr->setType( name(), code(), nFin, isMinBias, 
    isResolved, isDiffA, isDiffB, isLHA);

  // Let hard process record begin with the event as a whole and
  // the two incoming beam particles.  
  process.append( 90, -11, 0, 0, 0, 0, 0, 0, 
    Vec4(0., 0., 0., infoPtr->eCM()), infoPtr->eCM(), 0. ); 
  process.append( infoPtr->idA(), -12, 0, 0, 3, 0, 0, 0, 
    Vec4(0., 0., infoPtr->pzA(), infoPtr->eA()), infoPtr->mA(), 0. ); 
  process.append( infoPtr->idB(), -12, 0, 0, 4, 0, 0, 0, 
    Vec4(0., 0., infoPtr->pzB(), infoPtr->eB()), infoPtr->mB(), 0. ); 

  // For minbias process no interaction selected so far, so done.
  if (isMinBias) return true;
  double scale = 0.;

  // Insert the subprocess partons - resolved processes.
  if (isResolved && !isLHA) {

    // Find scale from which to begin MI/ISR/FSR evolution. 
    scale = sqrt(Q2Fac());
    process.scale( scale );

    // Loop over incoming and outgoing partons.
    int colOffset = process.lastColTag();
    for (int i = 1; i <= 2 + nFin; ++i) { 

      // Read out particle info from SigmaProcess object.
      int id = sigmaProcessPtr->id(i);
      int status = (i <= 2) ? -21 : 23;
      int mother1 = (i <= 2) ? i : 3;
      int mother2 = (i <= 2) ? 0 : 4;
      int daughter1 = (i <= 2) ? 5 : 0;
      int daughter2 = (i <= 2) ? 4 + nFin : 0;
      int col = sigmaProcessPtr->col(i);
      if (col > 0) col += colOffset;
      int acol = sigmaProcessPtr->acol(i);
      if (acol > 0) acol += colOffset;

      // Append to process record.
      int iNow = process.append( id, status, mother1, mother2, 
        daughter1, daughter2, col, acol, phaseSpacePtr->p(i), 
        phaseSpacePtr->m(i), scale);
      
      // Pick lifetime where relevant, else not.
      if (process[iNow].tau0() > 0.) process[iNow].tau(
        process[iNow].tau0() * Rndm::exp() );
    }
  }

  // Insert the outgoing particles - unresolved processes.
  else if (!isLHA) { 
    process.append( sigmaProcessPtr->id(3), 23, 1, 0, 0, 0, 0, 0, 
      phaseSpacePtr->p(3), phaseSpacePtr->m(3));
    process.append( sigmaProcessPtr->id(4), 23, 2, 0, 0, 0, 0, 0, 
      phaseSpacePtr->p(4), phaseSpacePtr->m(4));
  }

  // Insert the outgoing particles - Les Houches Accord processes.
  else {

    // Since LHA partons may be out of order, determine correct one.
    // (Recall that zeroth particle is empty.) 
    vector<int> newPos;
    newPos.reserve(lhaUpPtr->sizePart());
    newPos.push_back(0);
    for (int iNew = 0; iNew < lhaUpPtr->sizePart(); ++iNew) {
      // For iNew == 0 look for the two incoming partons, then for
      // partons having them as mothers, and so on layer by layer.
      for (int i = 1; i < lhaUpPtr->sizePart(); ++i)
        if (lhaUpPtr->mother1(i) == newPos[iNew]) newPos.push_back(i);
      if (int(newPos.size()) <= iNew) break;
    } 

    // Find scale from which to begin MI/ISR/FSR evolution.
    scale = lhaUpPtr->scale();
    process.scale( scale);

    // Copy over info from LHA event to process, in proper order.
    for (int i = 1; i < lhaUpPtr->sizePart(); ++i) {
      int iOld = newPos[i];
      int id = lhaUpPtr->id(iOld);

      // Translate from LHA status codes.
      int lhaStatus =  lhaUpPtr->status(iOld);
      int status = -21;
      if (lhaStatus == 2 || lhaStatus == 3) status = -22;
      if (lhaStatus == 1) status = 23;

      // Find where mothers have been moved by reordering.
      int mother1Old = lhaUpPtr->mother1(iOld);   
      int mother2Old = lhaUpPtr->mother2(iOld);   
      int mother1 = 0;
      int mother2 = 0; 
      for (int im = 1; im < i; ++im) {
        if (mother1Old == newPos[im]) mother1 = im + 2; 
        if (mother2Old == newPos[im]) mother2 = im + 2; 
      } 
      if (i <= 2) mother1 = i;

      // Ensure that second mother = 0 except for bona fide carbon copies.
      if (mother1 > 0 && mother2 == mother1) { 
        int sister1 = process[mother1].daughter1();
        int sister2 = process[mother1].daughter2();
        if (sister2 != sister1 && sister2 != 0) mother2 = 0;
      } 

      // Find daughters and where they have been moved by reordering. 
      // (Values shifted two steps to account for inserted beams.)
      int daughter1 = 0;
      int daughter2 = 0;
      for (int im = i + 1; im < lhaUpPtr->sizePart(); ++im) { 
        if (lhaUpPtr->mother1(newPos[im]) == iOld
          || lhaUpPtr->mother2(newPos[im]) == iOld) {
          if (daughter1 == 0 || im + 2 < daughter1) daughter1 = im + 2;
          if (daughter2 == 0 || im + 2 > daughter2) daughter2 = im + 2;
        }
      }
      // For 2 -> 1 hard scatterings reset second daughter to 0.
      if (daughter2 == daughter1) daughter2 = 0;

      // Colour trivial, except reset irrelevant colour indices.
      int colType = ParticleDataTable::colType(id);
      int col1   = (colType == 1 || colType == 2) 
                 ? lhaUpPtr->col1(iOld) : 0;   
      int col2   = (colType == -1 || colType == 2) 
                 ?  lhaUpPtr->col2(iOld) : 0; 

      // Momentum trivial.
      double px  = lhaUpPtr->px(iOld);  
      double py  = lhaUpPtr->py(iOld);  
      double pz  = lhaUpPtr->pz(iOld);  
      double e   = lhaUpPtr->e(iOld);  
      double m   = lhaUpPtr->m(iOld);

      // For resonance decay products use resonance mass as scale.
      double scaleNow = scale;
      if (mother1 > 4) scaleNow = process[mother1].m();

      // Store Les Houches Accord partons.
      int iNow = process.append( id, status, mother1, mother2, daughter1, 
        daughter2, col1, col2, Vec4(px, py, pz, e), m, scaleNow);

      // Check if need to store lifetime.
      double tau = lhaUpPtr->tau(iOld);
      if (tau > 0.) process[iNow].tau(tau);
    }  
  }

  // Loop through decay chains and set secondary vertices when needed.
  for (int i = 3; i < process.size(); ++i) {
    int iMother  = process[i].mother1();
    
    // If sister to already assigned vertex then assign same.
    if ( process[i - 1].mother1() == iMother && process[i - 1].hasVertex() ) 
      process[i].vProd( process[i - 1].vProd() ); 

    // Else if mother already has vertex and/or lifetime then assign.
    else if ( process[iMother].hasVertex() || process[iMother].tau() > 0.)
      process[i].vProd( process[iMother].vDec() ); 
  }

  // Further info on process. Reset quantities that may or may not be known.
  int    id1Now  =  process[3].id(); 
  int    id2Now  =  process[4].id(); 
  double pdf1    = 0.;
  double pdf2    = 0.;
  double tHat    = 0.;
  double uHat    = 0.;
  double pTHat   = 0.;
  double m3      = 0.;
  double m4      = 0.;
  double theta   = 0.;
  double phi     = 0.;
  double Q2FacNow, alphaEM, alphaS, Q2Ren, x1Now, x2Now, sHat;

  // Internally generated and stored information.
  if (!isLHA) {
    pdf1         = sigmaProcessPtr->pdf1();
    pdf2         = sigmaProcessPtr->pdf2();
    Q2FacNow     = sigmaProcessPtr->Q2Fac();
    alphaEM      = sigmaProcessPtr->alphaEMRen();
    alphaS       = sigmaProcessPtr->alphaSRen();
    Q2Ren        = sigmaProcessPtr->Q2Ren();
    x1Now        = phaseSpacePtr->x1();
    x2Now        = phaseSpacePtr->x2();
    sHat         = phaseSpacePtr->sHat();
    tHat         = phaseSpacePtr->tHat();
    uHat         = phaseSpacePtr->uHat();
    pTHat        = phaseSpacePtr->pTHat();
    m3           = phaseSpacePtr->m(3);
    m4           = phaseSpacePtr->m(4);
    theta        = phaseSpacePtr->thetaHat();
    phi          = phaseSpacePtr->phiHat();
  }    

  // Les Houches Accord process partly available, partly to be constructed.
  else {
    Q2FacNow     = pow2(scale);
    alphaEM      = lhaUpPtr->alphaQED();
    alphaS       = lhaUpPtr->alphaQCD();
    Q2Ren        = Q2FacNow;
    x1Now        = 2. * process[3].e() / infoPtr->eCM();
    x2Now        = 2. * process[4].e() / infoPtr->eCM();
    Vec4 pSum    = process[3].p() + process[4].p();
    sHat         = pSum * pSum;

    // Read info on parton densities if provided.
    if (lhaUpPtr->pdfIsSet()) {
      pdf1       = lhaUpPtr->xpdf1();
      pdf2       = lhaUpPtr->xpdf2();
      Q2FacNow   = pow2(lhaUpPtr->scalePDF());
      x1Now      = lhaUpPtr->x1();
      x2Now      = lhaUpPtr->x2();
    }

    // Reconstruct kinematics of 2 -> 2 processes from momenta.
    if (nFin == 2) {
      Vec4 pDifT = process[3].p() - process[5].p();
      tHat       = pDifT * pDifT;    
      Vec4 pDifU = process[3].p() - process[6].p();
      uHat       = pDifU * pDifU;
      pTHat      = process[5].pT();
      m3         = process[5].m();    
      m4         = process[6].m(); 
      Vec4 p5    = process[5].p();
      p5.bstback(pSum);
      theta      = p5.theta();   
      phi        = process[5].phi();   
    }
  }

  // Store information.
  if (isHardest) {
    infoPtr->setPDFalpha( id1Now, id2Now, pdf1, pdf2, Q2FacNow, 
      alphaEM, alphaS, Q2Ren);
    infoPtr->setKin( x1Now, x2Now, sHat, tHat, uHat, pTHat, m3, m4, 
      theta, phi);
  }
  infoPtr->setTypeMI( code(), pTHat);

  // For Les Houches event store subprocess classification.
  if (isLHA) {
    int codeSub  = lhaUpPtr->idProcess();
    ostringstream nameSub;
    nameSub << "user process " << codeSub; 
    infoPtr->setSubType( nameSub.str(), codeSub, nFin);
  }

  // Done.
  return true;

}

//*********
  
// Handle resonance decays.

bool ProcessContainer::decayResonances( Event& process) {

  // Save current event-record size.
  process.saveSize();
  bool physical    = true;
  bool newFlavours = false;

  // Do sequential chain of uncorrelated isotropic decays.
  do {
    physical = resDecaysPtr->next( process);
    if (!physical) return false;

    // Check whether flavours should be correlated.
    // (Currently only relevant for f fbar -> gamma*/Z0 gamma*/Z0.)
    newFlavours = ( sigmaProcessPtr->weightDecayFlav( process) 
                  < Rndm::flat() ); 

    // Reset the decay chains if have to redo.
    if (newFlavours) {
      process.restoreSize();
      for (int i = 5; i < process.size(); ++i) process[i].statusPos();
    } 

  // Loop back where required to generate new decays with new flavours.    
  } while (newFlavours);

  // Correct to nonisotropic decays.
  phaseSpacePtr->decayKinematics( process); 

  // Done.
  return true;

}

//*********

// Reset event generation statistics; but NOT maximum of cross section.

void ProcessContainer::reset() {

  nTry      = 0;
  nSel      = 0;
  nAcc      = 0;
  nTryStat  = 0;
  sigmaSum  = 0.;
  sigma2Sum = 0.;
  sigmaNeg  = 0.;
  sigmaAvg  = 0.;
  sigmaFin  = 0.;
  deltaFin  = 0.;

}

//*********

// Estimate integrated cross section and its uncertainty.

void ProcessContainer::sigmaDelta() {

  // Initial values. No analysis meaningful unless accepted events.
  nTryStat = nTry;
  sigmaAvg = 0.;
  sigmaFin = 0.;
  deltaFin = 0.;
  if (nAcc == 0) return;

  // Average value.
  double nTryInv  = 1. / nTry;
  double nSelInv  = 1. / nSel;
  double nAccInv  = 1. / nAcc;
  sigmaAvg = sigmaSum * nTryInv ;
  double fracAcc  = nAcc * nSelInv;
  sigmaFin        = sigmaAvg * fracAcc;

  // Estimated error. Quadratic sum of cross section term and
  // binomial from accept/reject step.
  deltaFin = sigmaFin;
  if (nAcc == 1) return;
  double delta2Sig   = (sigma2Sum *nTryInv - pow2(sigmaAvg)) * nTryInv
    / pow2(sigmaAvg);
  double delta2Veto  = (nSel - nAcc) * nAccInv * nSelInv;
  double delta2Sum   = delta2Sig + delta2Veto;
  deltaFin           = sqrtpos(delta2Sum) * sigmaFin; 

}
 
//**************************************************************************

// SetupContainer class.
// Turns list of user-desired processes into a vector of containers.

//*********

// Main routine to initialize list of processes.

bool SetupContainers::init(vector<ProcessContainer*>& containerPtrs) {

  // Reset process list, if filled in previous subrun.
  if (containerPtrs.size() > 0) {
    for (int i = 0; i < int(containerPtrs.size()); ++i) 
      delete containerPtrs[i];
    containerPtrs.clear(); 
  }
  SigmaProcess* sigmaPtr;

  // Set up requested objects for soft QCD processes.
  bool softQCD = Settings::flag("SoftQCD:all");
  if (softQCD || Settings::flag("SoftQCD:minBias")) {
    sigmaPtr = new Sigma0minBias;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (softQCD || Settings::flag("SoftQCD:elastic")) {
    sigmaPtr = new Sigma0AB2AB;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (softQCD || Settings::flag("SoftQCD:singleDiffractive")) {
    sigmaPtr = new Sigma0AB2XB;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma0AB2AX;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (softQCD || Settings::flag("SoftQCD:doubleDiffractive")) {
    sigmaPtr = new Sigma0AB2XX;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  
  // Set up requested objects for hard QCD processes.
  bool hardQCD = Settings::flag("HardQCD:all");
  if (hardQCD || Settings::flag("HardQCD:gg2gg")) {
    sigmaPtr = new Sigma2gg2gg;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (hardQCD || Settings::flag("HardQCD:gg2qqbar")) {
    sigmaPtr = new Sigma2gg2qqbar;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (hardQCD || Settings::flag("HardQCD:qg2qg")) {
    sigmaPtr = new Sigma2qg2qg;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (hardQCD || Settings::flag("HardQCD:qq2qq")) {
    sigmaPtr = new Sigma2qq2qq;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (hardQCD || Settings::flag("HardQCD:qqbar2gg")) {
    sigmaPtr = new Sigma2qqbar2gg;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (hardQCD || Settings::flag("HardQCD:qqbar2qqbarNew")) {
    sigmaPtr = new Sigma2qqbar2qqbarNew;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  
  // Set up requested objects for c cbar and b bbar, also hard QCD.
  if (hardQCD || Settings::flag("HardQCD:gg2ccbar")) {
    sigmaPtr = new Sigma2gg2QQbar(4, 121);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (hardQCD || Settings::flag("HardQCD:qqbar2ccbar")) {
    sigmaPtr = new Sigma2qqbar2QQbar(4, 122);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (hardQCD || Settings::flag("HardQCD:gg2bbbar")) {
    sigmaPtr = new Sigma2gg2QQbar(5, 123);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (hardQCD || Settings::flag("HardQCD:qqbar2bbbar")) {
    sigmaPtr = new Sigma2qqbar2QQbar(5, 124);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 

  // Set up requested objects for prompt photon processes.
  bool promptPhotons = Settings::flag("PromptPhoton:all");
  if (promptPhotons
    || Settings::flag("PromptPhoton:qg2qgamma")) {
    sigmaPtr = new Sigma2qg2qgamma;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (promptPhotons 
    || Settings::flag("PromptPhoton:qqbar2ggamma")) {
    sigmaPtr = new Sigma2qqbar2ggamma;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (promptPhotons
    || Settings::flag("PromptPhoton:gg2ggamma")) {
    sigmaPtr = new Sigma2gg2ggamma;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (promptPhotons 
    || Settings::flag("PromptPhoton:ffbar2gammagamma")) {
    sigmaPtr = new Sigma2ffbar2gammagamma;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (promptPhotons
    || Settings::flag("PromptPhoton:gg2gammagamma")) {
    sigmaPtr = new Sigma2gg2gammagamma;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 

  // Set up requested objects for weak gauge boson t-channel exchange.
  bool weakBosonExchanges = Settings::flag("WeakBosonExchange:all");
  if (weakBosonExchanges
    || Settings::flag("WeakBosonExchange:ff2ff(t:gmZ)")) {
    sigmaPtr = new Sigma2ff2fftgmZ;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (weakBosonExchanges
    || Settings::flag("WeakBosonExchange:ff2ff(t:W)")) {
    sigmaPtr = new Sigma2ff2fftW;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 

  // Set up requested objects for weak gauge boson processes.
  bool weakSingleBosons = Settings::flag("WeakSingleBoson:all");
  if (weakSingleBosons
    || Settings::flag("WeakSingleBoson:ffbar2gmZ")) {
    sigmaPtr = new Sigma1ffbar2gmZ;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (weakSingleBosons
    || Settings::flag("WeakSingleBoson:ffbar2W")) {
    sigmaPtr = new Sigma1ffbar2W;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 

  // Set up requested object for s-channel gamma exchange.
  // Subset of gamma*/Z0 above, intended for Multiple interactions.
  if (Settings::flag("WeakSingleBoson:ffbar2ffbar(s:gm)")) {
    sigmaPtr = new Sigma2ffbar2ffbarsgm;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
   
  // Set up requested objects for weak gauge boson pair processes.
  bool weakDoubleBosons = Settings::flag("WeakDoubleBoson:all");
  if (weakDoubleBosons
    || Settings::flag("WeakDoubleBoson:ffbar2gmZgmZ")) {
    sigmaPtr = new Sigma2ffbar2gmZgmZ;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (weakDoubleBosons
    || Settings::flag("WeakDoubleBoson:ffbar2ZW")) {
    sigmaPtr = new Sigma2ffbar2ZW;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (weakDoubleBosons
    || Settings::flag("WeakDoubleBoson:ffbar2WW")) {
    sigmaPtr = new Sigma2ffbar2WW;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  
  // Set up requested objects for weak gauge boson + parton processes.
  bool weakBosonAndPartons = Settings::flag("WeakBosonAndParton:all");
  if (weakBosonAndPartons
    || Settings::flag("WeakBosonAndParton:qqbar2gmZg")) {
    sigmaPtr = new Sigma2qqbar2gmZg;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (weakBosonAndPartons
    || Settings::flag("WeakBosonAndParton:qg2gmZq")) {
    sigmaPtr = new Sigma2qg2gmZq;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (weakBosonAndPartons
    || Settings::flag("WeakBosonAndParton:ffbar2gmZgm")) {
    sigmaPtr = new Sigma2ffbar2gmZgm;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (weakBosonAndPartons
    || Settings::flag("WeakBosonAndParton:fgm2gmZf")) {
    sigmaPtr = new Sigma2fgm2gmZf;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (weakBosonAndPartons
    || Settings::flag("WeakBosonAndParton:qqbar2Wg")) {
    sigmaPtr = new Sigma2qqbar2Wg;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (weakBosonAndPartons
    || Settings::flag("WeakBosonAndParton:qg2Wq")) {
    sigmaPtr = new Sigma2qg2Wq;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (weakBosonAndPartons
    || Settings::flag("WeakBosonAndParton:ffbar2Wgm")) {
    sigmaPtr = new Sigma2ffbar2Wgm;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (weakBosonAndPartons
    || Settings::flag("WeakBosonAndParton:fgm2Wf")) {
    sigmaPtr = new Sigma2fgm2Wf;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  
  // Set up requested objects for charmonium production
  bool charmoniums = Settings::flag("Charmonium:all");
  if (charmoniums || Settings::flag("Charmonium:gg2QQbar[3S1(1)]g")) {
    sigmaPtr = new Sigma2gg2QQbar3S11g(4, 401);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (charmoniums || Settings::flag("Charmonium:gg2QQbar[3P0(1)]g")) {
    sigmaPtr = new Sigma2gg2QQbar3PJ1g(4, 0, 402);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (charmoniums || Settings::flag("Charmonium:gg2QQbar[3P1(1)]g")) {
    sigmaPtr = new Sigma2gg2QQbar3PJ1g(4, 1, 403);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (charmoniums || Settings::flag("Charmonium:gg2QQbar[3P2(1)]g")) {
    sigmaPtr = new Sigma2gg2QQbar3PJ1g(4, 2, 404);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (charmoniums || Settings::flag("Charmonium:qg2QQbar[3P0(1)]q")) {
    sigmaPtr = new Sigma2qg2QQbar3PJ1q(4, 0, 405);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (charmoniums || Settings::flag("Charmonium:qg2QQbar[3P1(1)]q")) {
    sigmaPtr = new Sigma2qg2QQbar3PJ1q(4, 1, 406);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (charmoniums || Settings::flag("Charmonium:qg2QQbar[3P2(1)]q")) {
    sigmaPtr = new Sigma2qg2QQbar3PJ1q(4, 2, 407);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (charmoniums || Settings::flag("Charmonium:qqbar2QQbar[3P0(1)]g")) {
    sigmaPtr = new Sigma2qqbar2QQbar3PJ1g(4, 0, 408);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (charmoniums || Settings::flag("Charmonium:qqbar2QQbar[3P1(1)]g")) {
    sigmaPtr = new Sigma2qqbar2QQbar3PJ1g(4, 1, 409);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (charmoniums || Settings::flag("Charmonium:qqbar2QQbar[3P2(1)]g")) {
    sigmaPtr = new Sigma2qqbar2QQbar3PJ1g(4, 2, 410);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (charmoniums || Settings::flag("Charmonium:gg2QQbar[3S1(8)]g")) {
    sigmaPtr = new Sigma2gg2QQbarX8g(4, 0, 411);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (charmoniums || Settings::flag("Charmonium:gg2QQbar[1S0(8)]g")) {
    sigmaPtr = new Sigma2gg2QQbarX8g(4, 1, 412);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (charmoniums || Settings::flag("Charmonium:gg2QQbar[3PJ(8)]g")) {
    sigmaPtr = new Sigma2gg2QQbarX8g(4, 2, 413);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (charmoniums || Settings::flag("Charmonium:qg2QQbar[3S1(8)]q")) {
    sigmaPtr = new Sigma2qg2QQbarX8q(4, 0, 414);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (charmoniums || Settings::flag("Charmonium:qg2QQbar[1S0(8)]q")) {
    sigmaPtr = new Sigma2qg2QQbarX8q(4, 1, 415);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (charmoniums || Settings::flag("Charmonium:qg2QQbar[3PJ(8)]q")) {
    sigmaPtr = new Sigma2qg2QQbarX8q(4, 2, 416);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (charmoniums || Settings::flag("Charmonium:qqbar2QQbar[3S1(8)]g")) {
    sigmaPtr = new Sigma2qqbar2QQbarX8g(4, 0, 417);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (charmoniums || Settings::flag("Charmonium:qqbar2QQbar[1S0(8)]g")) {
    sigmaPtr = new Sigma2qqbar2QQbarX8g(4, 1, 418);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (charmoniums || Settings::flag("Charmonium:qqbar2QQbar[3PJ(8)]g")) {
    sigmaPtr = new Sigma2qqbar2QQbarX8g(4, 2, 419);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
    
  // Set up requested objects for bottomonium production
  bool bottomoniums = Settings::flag("Bottomonium:all");
  if (bottomoniums || Settings::flag("Bottomonium:gg2QQbar[3S1(1)]g")) {
    sigmaPtr = new Sigma2gg2QQbar3S11g(5, 501);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (bottomoniums || Settings::flag("Bottomonium:gg2QQbar[3P0(1)]g")) {
    sigmaPtr = new Sigma2gg2QQbar3PJ1g(5, 0, 502);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (bottomoniums || Settings::flag("Bottomonium:gg2QQbar[3P1(1)]g")) {
    sigmaPtr = new Sigma2gg2QQbar3PJ1g(5, 1, 503);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (bottomoniums || Settings::flag("Bottomonium:gg2QQbar[3P2(1)]g")) {
    sigmaPtr = new Sigma2gg2QQbar3PJ1g(5, 2, 504);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (bottomoniums || Settings::flag("Bottomonium:qg2QQbar[3P0(1)]q")) {
    sigmaPtr = new Sigma2qg2QQbar3PJ1q(5, 0, 505);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (bottomoniums || Settings::flag("Bottomonium:qg2QQbar[3P1(1)]q")) {
    sigmaPtr = new Sigma2qg2QQbar3PJ1q(5, 1, 506);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (bottomoniums || Settings::flag("Bottomonium:qg2QQbar[3P2(1)]q")) {
    sigmaPtr = new Sigma2qg2QQbar3PJ1q(5, 2, 507);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (bottomoniums || Settings::flag("Bottomonium:qqbar2QQbar[3P0(1)]g")) {
    sigmaPtr = new Sigma2qqbar2QQbar3PJ1g(5, 0, 508);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (bottomoniums || Settings::flag("Bottomonium:qqbar2QQbar[3P1(1)]g")) {
    sigmaPtr = new Sigma2qqbar2QQbar3PJ1g(5, 1, 509);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (bottomoniums || Settings::flag("Bottomonium:qqbar2QQbar[3P2(1)]g")) {
    sigmaPtr = new Sigma2qqbar2QQbar3PJ1g(5, 2, 510);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (bottomoniums || Settings::flag("Bottomonium:gg2QQbar[3S1(8)]g")) {
    sigmaPtr = new Sigma2gg2QQbarX8g(5, 0, 511);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (bottomoniums || Settings::flag("Bottomonium:gg2QQbar[1S0(8)]g")) {
    sigmaPtr = new Sigma2gg2QQbarX8g(5, 1, 512);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (bottomoniums || Settings::flag("Bottomonium:gg2QQbar[3PJ(8)]g")) {
    sigmaPtr = new Sigma2gg2QQbarX8g(5, 2, 513);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (bottomoniums || Settings::flag("Bottomonium:qg2QQbar[3S1(8)]q")) {
    sigmaPtr = new Sigma2qg2QQbarX8q(5, 0, 514);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (bottomoniums || Settings::flag("Bottomonium:qg2QQbar[1S0(8)]q")) {
    sigmaPtr = new Sigma2qg2QQbarX8q(5, 1, 515);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (bottomoniums || Settings::flag("Bottomonium:qg2QQbar[3PJ(8)]q")) {
    sigmaPtr = new Sigma2qg2QQbarX8q(5, 2, 516);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (bottomoniums || Settings::flag("Bottomonium:qqbar2QQbar[3S1(8)]g")) {
    sigmaPtr = new Sigma2qqbar2QQbarX8g(5, 0, 517);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (bottomoniums || Settings::flag("Bottomonium:qqbar2QQbar[1S0(8)]g")) {
    sigmaPtr = new Sigma2qqbar2QQbarX8g(5, 1, 518);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (bottomoniums || Settings::flag("Bottomonium:qqbar2QQbar[3PJ(8)]g")) {
    sigmaPtr = new Sigma2qqbar2QQbarX8g(5, 2, 519);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  
  // Set up requested objects for top production
  bool tops = Settings::flag("Top:all");
  if (tops || Settings::flag("Top:gg2ttbar")) {
    sigmaPtr = new Sigma2gg2QQbar(6, 601);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (tops || Settings::flag("Top:qqbar2ttbar")) {
    sigmaPtr = new Sigma2qqbar2QQbar(6, 602);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (tops || Settings::flag("Top:qq2tq(t:W)")) {
    sigmaPtr = new Sigma2qq2QqtW(6, 603);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (tops || Settings::flag("Top:ffbar2ttbar(s:gmZ)")) {
    sigmaPtr = new Sigma2ffbar2FFbarsgmZ(6, 604);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (tops || Settings::flag("Top:ffbar2tqbar(s:W)")) {
    sigmaPtr = new Sigma2ffbar2FfbarsW(6, 0, 605);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  
  // Set up requested objects for fourth-generation b' production
  bool bPrimes = Settings::flag("FourthBottom:all");
  if (bPrimes || Settings::flag("FourthBottom:gg2bPrimebPrimebar")) {
    sigmaPtr = new Sigma2gg2QQbar(7, 801);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (bPrimes || Settings::flag("FourthBottom:qqbar2bPrimebPrimebar")) {
    sigmaPtr = new Sigma2qqbar2QQbar(7, 802);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (bPrimes || Settings::flag("FourthBottom:qq2bPrimeq(t:W)")) {
    sigmaPtr = new Sigma2qq2QqtW(7, 803);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (bPrimes || Settings::flag("FourthBottom:ffbar2bPrimebPrimebar(s:gmZ)")) {
    sigmaPtr = new Sigma2ffbar2FFbarsgmZ(7, 804);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (bPrimes || Settings::flag("FourthBottom:ffbar2bPrimeqbar(s:W)")) {
    sigmaPtr = new Sigma2ffbar2FfbarsW(7, 0, 805);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (bPrimes || Settings::flag("FourthBottom:ffbar2bPrimetbar(s:W)")) {
    sigmaPtr = new Sigma2ffbar2FfbarsW(7, 6, 806);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  
  // Set up requested objects for fourth-generation t' production
  bool tPrimes = Settings::flag("FourthTop:all");
  if (tPrimes || Settings::flag("FourthTop:gg2tPrimetPrimebar")) {
    sigmaPtr = new Sigma2gg2QQbar(8, 821);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (tPrimes || Settings::flag("FourthTop:qqbar2tPrimetPrimebar")) {
    sigmaPtr = new Sigma2qqbar2QQbar(8, 822);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (tPrimes || Settings::flag("FourthTop:qq2tPrimeq(t:W)")) {
    sigmaPtr = new Sigma2qq2QqtW(8, 823);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (tPrimes || Settings::flag("FourthTop:ffbar2tPrimetPrimebar(s:gmZ)")) {
    sigmaPtr = new Sigma2ffbar2FFbarsgmZ(8, 824);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (tPrimes || Settings::flag("FourthTop:ffbar2tPrimeqbar(s:W)")) {
    sigmaPtr = new Sigma2ffbar2FfbarsW(8, 0, 825);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 

  // Set up requested objects for two different fourth-generation fermions.
  if (bPrimes || tPrimes 
    || Settings::flag("FourthPair:ffbar2tPrimebPrimebar(s:W)")) {
    sigmaPtr = new Sigma2ffbar2FfbarsW(8, 7, 841);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (Settings::flag("FourthPair:ffbar2tauPrimenuPrimebar(s:W)")) {
    sigmaPtr = new Sigma2ffbar2FfbarsW(17, 18, 842);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 

  // Flag for global choice between SM and BSM Higgses.
  bool useBSMHiggses = Settings::flag("Higgs:useBSM");
  
  // Set up requested objects for Standard-Model Higgs production.
  if (!useBSMHiggses) {
    bool HiggsesSM = Settings::flag("HiggsSM:all");
    if (HiggsesSM || Settings::flag("HiggsSM:ffbar2H")) {
      sigmaPtr = new Sigma1ffbar2H(0);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    } 
    if (HiggsesSM || Settings::flag("HiggsSM:gg2H")) {
     sigmaPtr = new Sigma1gg2H(0);
     containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    } 
    if (HiggsesSM || Settings::flag("HiggsSM:gmgm2H")) {
      sigmaPtr = new Sigma1gmgm2H(0);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    } 
    if (HiggsesSM || Settings::flag("HiggsSM:ffbar2HZ")) {
      sigmaPtr = new Sigma2ffbar2HZ(0);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    } 
    if (HiggsesSM || Settings::flag("HiggsSM:ffbar2HW")) {
      sigmaPtr = new Sigma2ffbar2HW(0);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    } 
    if (HiggsesSM || Settings::flag("HiggsSM:ff2Hff(t:ZZ)")) {
      sigmaPtr = new Sigma3ff2HfftZZ(0);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    } 
    if (HiggsesSM || Settings::flag("HiggsSM:ff2Hff(t:WW)")) {
      sigmaPtr = new Sigma3ff2HfftWW(0);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    } 
    if (HiggsesSM || Settings::flag("HiggsSM:gg2Httbar")) {
      sigmaPtr = new Sigma3gg2HQQbar(6,0);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    } 
    if (HiggsesSM || Settings::flag("HiggsSM:qqbar2Httbar")) {
      sigmaPtr = new Sigma3qqbar2HQQbar(6,0);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    } 

    // Further Standard-Model Higgs processes, not included in "all".
    if (Settings::flag("HiggsSM:qg2Hq")) {
      sigmaPtr = new Sigma2qg2Hq(4,0);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
      sigmaPtr = new Sigma2qg2Hq(5,0);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    } 
    if (Settings::flag("HiggsSM:gg2Hbbbar")) {
      sigmaPtr = new Sigma3gg2HQQbar(5,0);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    } 
    if (Settings::flag("HiggsSM:qqbar2Hbbbar")) {
      sigmaPtr = new Sigma3qqbar2HQQbar(5,0);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    } 
    if (Settings::flag("HiggsSM:gg2Hg(l:t)")) {
      sigmaPtr = new Sigma2gg2Hglt(0);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    } 
    if (Settings::flag("HiggsSM:qg2Hq(l:t)")) {
      sigmaPtr = new Sigma2qg2Hqlt(0);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    } 
    if (Settings::flag("HiggsSM:qqbar2Hg(l:t)")) {
      sigmaPtr = new Sigma2qqbar2Hglt(0);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    } 
  }

  // Common switch for the group of Higgs production BSM.
  if (useBSMHiggses) {
    bool HiggsesBSM = Settings::flag("HiggsBSM:all");    

    // Set up requested objects for BSM H1 production.
    bool HiggsesH1 = Settings::flag("HiggsBSM:allH1"); 
    if (HiggsesBSM || HiggsesH1 || Settings::flag("HiggsBSM:ffbar2H1")) {
      sigmaPtr = new Sigma1ffbar2H(1);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );  
    }
    if (HiggsesBSM || HiggsesH1 || Settings::flag("HiggsBSM:gg2H1")) {
      sigmaPtr = new Sigma1gg2H(1);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (HiggsesBSM || HiggsesH1 || Settings::flag("HiggsBSM:gmgm2H1")) {
      sigmaPtr = new Sigma1gmgm2H(1);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (HiggsesBSM || HiggsesH1 || Settings::flag("HiggsBSM:ffbar2H1Z")) {
      sigmaPtr = new Sigma2ffbar2HZ(1);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (HiggsesBSM || HiggsesH1 || Settings::flag("HiggsBSM:ffbar2H1W")) {
      sigmaPtr = new Sigma2ffbar2HW(1);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (HiggsesBSM || HiggsesH1 || Settings::flag("HiggsBSM:ff2H1ff(t:ZZ)")) {
      sigmaPtr = new Sigma3ff2HfftZZ(1);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (HiggsesBSM || HiggsesH1 || Settings::flag("HiggsBSM:ff2H1ff(t:WW)")) {
      sigmaPtr = new Sigma3ff2HfftWW(1);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (HiggsesBSM || HiggsesH1 || Settings::flag("HiggsBSM:gg2H1ttbar")) {
      sigmaPtr = new Sigma3gg2HQQbar(6,1);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (HiggsesBSM || HiggsesH1 || Settings::flag("HiggsBSM:qqbar2H1ttbar")) {
      sigmaPtr = new Sigma3qqbar2HQQbar(6,1);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }

    // Further BSM H1 processes, not included in "all".
    if (Settings::flag("HiggsBSM:qg2H1q")) {
      sigmaPtr = new Sigma2qg2Hq(4,1);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
      sigmaPtr = new Sigma2qg2Hq(5,1);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (Settings::flag("HiggsBSM:gg2H1bbbar")) {
      sigmaPtr = new Sigma3gg2HQQbar(5,1);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (Settings::flag("HiggsBSM:qqbar2H1bbbar")) {
      sigmaPtr = new Sigma3qqbar2HQQbar(5,1);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (Settings::flag("HiggsBSM:gg2H1g(l:t)")) {
      sigmaPtr = new Sigma2gg2Hglt(1);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (Settings::flag("HiggsBSM:qg2H1q(l:t)")) {
      sigmaPtr = new Sigma2qg2Hqlt(1);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (Settings::flag("HiggsBSM:qqbar2H1g(l:t)")) {
      sigmaPtr = new Sigma2qqbar2Hglt(1);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }   

    // Set up requested objects for BSM H2 production.
    bool HiggsesH2 = Settings::flag("HiggsBSM:allH2");
    if (HiggsesBSM || HiggsesH2 || Settings::flag("HiggsBSM:ffbar2H2")) {
      sigmaPtr = new Sigma1ffbar2H(2);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    } 
    if (HiggsesBSM || HiggsesH2 || Settings::flag("HiggsBSM:gg2H2")) {
      sigmaPtr = new Sigma1gg2H(2);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (HiggsesBSM || HiggsesH2 || Settings::flag("HiggsBSM:gmgm2H2")) {
      sigmaPtr = new Sigma1gmgm2H(2);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (HiggsesBSM || HiggsesH2 || Settings::flag("HiggsBSM:ffbar2H2Z")) {
      sigmaPtr = new Sigma2ffbar2HZ(2);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (HiggsesBSM || HiggsesH2 || Settings::flag("HiggsBSM:ffbar2H2W")) {
      sigmaPtr = new Sigma2ffbar2HW(2);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (HiggsesBSM || HiggsesH2 || Settings::flag("HiggsBSM:ff2H2ff(t:ZZ)")) {
      sigmaPtr = new Sigma3ff2HfftZZ(2);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (HiggsesBSM || HiggsesH2 || Settings::flag("HiggsBSM:ff2H2ff(t:WW)")) {
      sigmaPtr = new Sigma3ff2HfftWW(2);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (HiggsesBSM || HiggsesH2 || Settings::flag("HiggsBSM:gg2H2ttbar")) {
      sigmaPtr = new Sigma3gg2HQQbar(6,2);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (HiggsesBSM || HiggsesH2 || Settings::flag("HiggsBSM:qqbar2H2ttbar")) {
      sigmaPtr = new Sigma3qqbar2HQQbar(6,2);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }

    // Further BSM H2 processes, not included in "all".
   if (Settings::flag("HiggsBSM:qg2H2q")) {
      sigmaPtr = new Sigma2qg2Hq(4,2);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
      sigmaPtr = new Sigma2qg2Hq(5,2);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (Settings::flag("HiggsBSM:gg2H2bbbar")) {
      sigmaPtr = new Sigma3gg2HQQbar(5,2);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (Settings::flag("HiggsBSM:qqbar2H2bbbar")) {
      sigmaPtr = new Sigma3qqbar2HQQbar(5,2);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (Settings::flag("HiggsBSM:gg2H2g(l:t)")) {
      sigmaPtr = new Sigma2gg2Hglt(2);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (Settings::flag("HiggsBSM:qg2H2q(l:t)")) {
      sigmaPtr = new Sigma2qg2Hqlt(2);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (Settings::flag("HiggsBSM:qqbar2H2g(l:t)")) {
      sigmaPtr = new Sigma2qqbar2Hglt(2);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }

    // Set up requested objects for BSM A3 production.
    bool HiggsesA3 = Settings::flag("HiggsBSM:allA3");
    if (HiggsesBSM || HiggsesA3 || Settings::flag("HiggsBSM:ffbar2A3")) {
      sigmaPtr = new Sigma1ffbar2H(3);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (HiggsesBSM || HiggsesA3 || Settings::flag("HiggsBSM:gg2A3")) {
      sigmaPtr = new Sigma1gg2H(3);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (HiggsesBSM || HiggsesA3 || Settings::flag("HiggsBSM:gmgm2A3")) {
      sigmaPtr = new Sigma1gmgm2H(3);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (HiggsesBSM || HiggsesA3 || Settings::flag("HiggsBSM:ffbar2A3Z")) {
      sigmaPtr = new Sigma2ffbar2HZ(3);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (HiggsesBSM || HiggsesA3 || Settings::flag("HiggsBSM:ffbar2A3W")) {
      sigmaPtr = new Sigma2ffbar2HW(3);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (HiggsesBSM || HiggsesA3 || Settings::flag("HiggsBSM:ff2A3ff(t:ZZ)")) {
      sigmaPtr = new Sigma3ff2HfftZZ(3);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (HiggsesBSM || HiggsesA3 || Settings::flag("HiggsBSM:ff2A3ff(t:WW)")) {
      sigmaPtr = new Sigma3ff2HfftWW(3);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (HiggsesBSM || HiggsesA3 || Settings::flag("HiggsBSM:gg2A3ttbar")) {
      sigmaPtr = new Sigma3gg2HQQbar(6,3);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (HiggsesBSM || HiggsesA3 || Settings::flag("HiggsBSM:qqbar2A3ttbar")) {
      sigmaPtr = new Sigma3qqbar2HQQbar(6,3);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }

    // Further BSM A3 processes, not included in "all".
    if (Settings::flag("HiggsBSM:qg2A3q")) {
      sigmaPtr = new Sigma2qg2Hq(4,3);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
      sigmaPtr = new Sigma2qg2Hq(5,3);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (Settings::flag("HiggsBSM:gg2A3bbbar")) {
      sigmaPtr = new Sigma3gg2HQQbar(5,3);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (Settings::flag("HiggsBSM:qqbar2A3bbbar")) {
      sigmaPtr = new Sigma3qqbar2HQQbar(5,3);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (Settings::flag("HiggsBSM:gg2A3g(l:t)")) {
      sigmaPtr = new Sigma2gg2Hglt(3);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (Settings::flag("HiggsBSM:qg2A3q(l:t)")) {
      sigmaPtr = new Sigma2qg2Hqlt(3);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }
    if (Settings::flag("HiggsBSM:qqbar2A3g(l:t)")) {
      sigmaPtr = new Sigma2qqbar2Hglt(3);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    }

    // Set up requested objects for Charged Higgs production
    bool HiggsesChg = Settings::flag("HiggsBSM:allH+-");
    if (HiggsesBSM || HiggsesChg || Settings::flag("HiggsBSM:ffbar2H+-")) {
      sigmaPtr = new Sigma1ffbar2Hchg;
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    } 
    if (HiggsesBSM || HiggsesChg || Settings::flag("HiggsBSM:bg2H+-t")) {
      sigmaPtr = new Sigma2qg2Hchgq(6, 1062, "b g -> H+- t");
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    } 

    // Set up requested objects for Higgs pair-production
    bool HiggsesPairs = Settings::flag("HiggsBSM:allHpair");
    if (HiggsesBSM || HiggsesPairs || Settings::flag("HiggsBSM:ffbar2A3H1")) {
      sigmaPtr = new Sigma2ffbar2A3H12(1);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    } 
    if (HiggsesBSM || HiggsesPairs || Settings::flag("HiggsBSM:ffbar2A3H2")) {
      sigmaPtr = new Sigma2ffbar2A3H12(2);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    } 
    if (HiggsesBSM || HiggsesPairs || Settings::flag("HiggsBSM:ffbar2H+-H1")) {
      sigmaPtr = new Sigma2ffbar2HchgH12(1);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    } 
    if (HiggsesBSM || HiggsesPairs || Settings::flag("HiggsBSM:ffbar2H+-H2")) {
      sigmaPtr = new Sigma2ffbar2HchgH12(2);
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    } 
    if (HiggsesBSM || HiggsesPairs || Settings::flag("HiggsBSM:ffbar2H+H-")) {
      sigmaPtr = new Sigma2ffbar2HposHneg();
      containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    } 
  }

  // Set up requested objects for neutralino pair processes.
  bool SUSYs = Settings::flag("SUSY:all");
  if (SUSYs || Settings::flag("SUSY:qqbar2chi0chi0")) {
    sigmaPtr = new Sigma2qqbar2chi0chi0(1, 1, 1201);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2qqbar2chi0chi0(1, 2, 1202); 
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2qqbar2chi0chi0(1, 3, 1203); 
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2qqbar2chi0chi0(1, 4, 1204);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2qqbar2chi0chi0(2, 2, 1205);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2qqbar2chi0chi0(2, 3, 1206);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2qqbar2chi0chi0(2, 4, 1207);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2qqbar2chi0chi0(3, 3, 1208); 
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2qqbar2chi0chi0(3, 4, 1209);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2qqbar2chi0chi0(4, 4, 1210); 
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  
  // Set up requested objects for New-Gauge-Boson processes.
  if (Settings::flag("NewGaugeBoson:ffbar2gmZZprime")) {
    sigmaPtr = new Sigma1ffbar2gmZZprime();
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (Settings::flag("NewGaugeBoson:ffbar2Wprime")) {
    sigmaPtr = new Sigma1ffbar2Wprime();
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (Settings::flag("NewGaugeBoson:ffbar2R0")) {
    sigmaPtr = new Sigma1ffbar2Rhorizontal();
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
   
  // Set up requested objects for Left-Right-Symmetry processes.
  bool leftrights = Settings::flag("LeftRightSymmmetry:all");
  if (leftrights || Settings::flag("LeftRightSymmmetry:ffbar2ZR")) {
    sigmaPtr = new Sigma1ffbar2ZRight();
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (leftrights || Settings::flag("LeftRightSymmmetry:ffbar2WR")) {
    sigmaPtr = new Sigma1ffbar2WRight();
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (leftrights || Settings::flag("LeftRightSymmmetry:ll2HL")) {
    sigmaPtr = new Sigma1ll2Hchgchg(1);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (leftrights || Settings::flag("LeftRightSymmmetry:lgm2HLe")) {
    sigmaPtr = new Sigma2lgm2Hchgchgl(1, 11);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (leftrights || Settings::flag("LeftRightSymmmetry:lgm2HLmu")) {
    sigmaPtr = new Sigma2lgm2Hchgchgl(1, 13);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (leftrights || Settings::flag("LeftRightSymmmetry:lgm2HLtau")) {
    sigmaPtr = new Sigma2lgm2Hchgchgl(1, 15);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (leftrights || Settings::flag("LeftRightSymmmetry:ff2HLff")) {
    sigmaPtr = new Sigma3ff2HchgchgfftWW(1);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (leftrights || Settings::flag("LeftRightSymmmetry:ffbar2HLHL")) {
    sigmaPtr = new Sigma2ffbar2HchgchgHchgchg(1);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (leftrights || Settings::flag("LeftRightSymmmetry:ll2HR")) {
    sigmaPtr = new Sigma1ll2Hchgchg(2);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (leftrights || Settings::flag("LeftRightSymmmetry:lgm2HRe")) {
    sigmaPtr = new Sigma2lgm2Hchgchgl(2, 11);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (leftrights || Settings::flag("LeftRightSymmmetry:lgm2HRmu")) {
    sigmaPtr = new Sigma2lgm2Hchgchgl(2, 13);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (leftrights || Settings::flag("LeftRightSymmmetry:lgm2HRtau")) {
    sigmaPtr = new Sigma2lgm2Hchgchgl(2, 15);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (leftrights || Settings::flag("LeftRightSymmmetry:ff2HRff")) {
    sigmaPtr = new Sigma3ff2HchgchgfftWW(2);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (leftrights || Settings::flag("LeftRightSymmmetry:ffbar2HRHR")) {
    sigmaPtr = new Sigma2ffbar2HchgchgHchgchg(2);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  
  // Set up requested objects for leptoquark LQ processes.
  bool leptoquarks = Settings::flag("LeptoQuark:all");
  if (leptoquarks || Settings::flag("LeptoQuark:ql2LQ")) {
    sigmaPtr = new Sigma1ql2LeptoQuark;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (leptoquarks || Settings::flag("LeptoQuark:qg2LQl")) {
    sigmaPtr = new Sigma2qg2LeptoQuarkl;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (leptoquarks || Settings::flag("LeptoQuark:gg2LQLQbar")) {
    sigmaPtr = new Sigma2gg2LQLQbar;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (leptoquarks || Settings::flag("LeptoQuark:qqbar2LQLQbar")) {
    sigmaPtr = new Sigma2qqbar2LQLQbar;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  
  // Set up requested objects for excited-fermion processes.
  bool excitedfermions = Settings::flag("ExcitedFermion:all");
  if (excitedfermions || Settings::flag("ExcitedFermion:dg2dStar")) {
    sigmaPtr = new Sigma1qg2qStar(1);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (excitedfermions || Settings::flag("ExcitedFermion:ug2uStar")) {
    sigmaPtr = new Sigma1qg2qStar(2);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (excitedfermions || Settings::flag("ExcitedFermion:sg2sStar")) {
    sigmaPtr = new Sigma1qg2qStar(3);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (excitedfermions || Settings::flag("ExcitedFermion:cg2cStar")) {
    sigmaPtr = new Sigma1qg2qStar(4);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (excitedfermions || Settings::flag("ExcitedFermion:bg2bStar")) {
    sigmaPtr = new Sigma1qg2qStar(5);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (excitedfermions || Settings::flag("ExcitedFermion:egm2eStar")) {
    sigmaPtr = new Sigma1lgm2lStar(11);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (excitedfermions || Settings::flag("ExcitedFermion:mugm2muStar")) {
    sigmaPtr = new Sigma1lgm2lStar(13);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (excitedfermions || Settings::flag("ExcitedFermion:taugm2tauStar")) {
    sigmaPtr = new Sigma1lgm2lStar(15);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (excitedfermions || Settings::flag("ExcitedFermion:qq2dStarq")) {
    sigmaPtr = new Sigma2qq2qStarq(1);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (excitedfermions || Settings::flag("ExcitedFermion:qq2uStarq")) {
    sigmaPtr = new Sigma2qq2qStarq(2);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (excitedfermions || Settings::flag("ExcitedFermion:qq2sStarq")) {
    sigmaPtr = new Sigma2qq2qStarq(3);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (excitedfermions || Settings::flag("ExcitedFermion:qq2cStarq")) {
    sigmaPtr = new Sigma2qq2qStarq(4);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (excitedfermions || Settings::flag("ExcitedFermion:qq2bStarq")) {
    sigmaPtr = new Sigma2qq2qStarq(5);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (excitedfermions || Settings::flag("ExcitedFermion:qqbar2eStare")) {
    sigmaPtr = new Sigma2qqbar2lStarlbar(11);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (excitedfermions || Settings::flag("ExcitedFermion:qqbar2nueStarnue")) {
    sigmaPtr = new Sigma2qqbar2lStarlbar(12);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (excitedfermions || Settings::flag("ExcitedFermion:qqbar2muStarmu")) {
    sigmaPtr = new Sigma2qqbar2lStarlbar(13);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (excitedfermions || Settings::flag("ExcitedFermion:qqbar2numuStarnumu")) {
    sigmaPtr = new Sigma2qqbar2lStarlbar(14);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (excitedfermions || Settings::flag("ExcitedFermion:qqbar2tauStartau")) {
    sigmaPtr = new Sigma2qqbar2lStarlbar(15);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (excitedfermions || Settings::flag("ExcitedFermion:qqbar2nutauStarnutau")) {
    sigmaPtr = new Sigma2qqbar2lStarlbar(16);
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  
  // Set up requested objects for extra-dimensional G* processes.
  bool extraDimGstars = Settings::flag("ExtraDimensionsG*:all");
  if (extraDimGstars || Settings::flag("ExtraDimensionsG*:gg2G*")) {
    sigmaPtr = new Sigma1gg2GravitonStar;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (extraDimGstars || Settings::flag("ExtraDimensionsG*:ffbar2G*")) {
    sigmaPtr = new Sigma1ffbar2GravitonStar;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (Settings::flag("ExtraDimensionsG*:gg2G*g")) {
    sigmaPtr = new Sigma2gg2GravitonStarg;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (Settings::flag("ExtraDimensionsG*:qg2G*q")) {
    sigmaPtr = new Sigma2qg2GravitonStarq;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 
  if (Settings::flag("ExtraDimensionsG*:qqbar2G*g")) {
    sigmaPtr = new Sigma2qqbar2GravitonStarg;
    containerPtrs.push_back( new ProcessContainer(sigmaPtr) );
  } 

  // Done. 
  return true;

}

//*********

// Routine to initialize list of second hard processes.

bool SetupContainers::init2(vector<ProcessContainer*>& container2Ptrs) {

  // Reset process list, if filled in previous subrun.
  if (container2Ptrs.size() > 0) {
    for (int i = 0; i < int(container2Ptrs.size()); ++i) 
      delete container2Ptrs[i];
    container2Ptrs.clear(); 
  }
  SigmaProcess* sigmaPtr;

  // Two hard QCD jets.
  if (Settings::flag("SecondHard:TwoJets")) {
    sigmaPtr = new Sigma2gg2gg;
    container2Ptrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2gg2qqbar;
    container2Ptrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2qg2qg;
    container2Ptrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2qq2qq;
    container2Ptrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2qqbar2gg;
    container2Ptrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2qqbar2qqbarNew;
    container2Ptrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2gg2QQbar(4, 121);
    container2Ptrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2qqbar2QQbar(4, 122);
    container2Ptrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2gg2QQbar(5, 123);
    container2Ptrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2qqbar2QQbar(5, 124);
    container2Ptrs.push_back( new ProcessContainer(sigmaPtr) );
  } 

  // A prompt photon and a hard jet.
  if (Settings::flag("SecondHard:PhotonAndJet")) {
    sigmaPtr = new Sigma2qg2qgamma;
    container2Ptrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2qqbar2ggamma;
    container2Ptrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2gg2ggamma;
    container2Ptrs.push_back( new ProcessContainer(sigmaPtr) );
  } 

  // Two prompt photons.
  if (Settings::flag("SecondHard:TwoPhotons")) {
    sigmaPtr = new Sigma2ffbar2gammagamma;
    container2Ptrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2gg2gammagamma;
    container2Ptrs.push_back( new ProcessContainer(sigmaPtr) );
  } 

  // A single gamma*/Z0.
  if (Settings::flag("SecondHard:SingleGmZ")) {
    sigmaPtr = new Sigma1ffbar2gmZ;
    container2Ptrs.push_back( new ProcessContainer(sigmaPtr) );
  } 

  // A single W+-.
  if (Settings::flag("SecondHard:SingleW")) {
    sigmaPtr = new Sigma1ffbar2W;
    container2Ptrs.push_back( new ProcessContainer(sigmaPtr) );
  } 

  // A gamma*/Z0 and a hard jet.
  if (Settings::flag("SecondHard:GmZAndJet")) {
    sigmaPtr = new Sigma2qqbar2gmZg;
    container2Ptrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2qg2gmZq;
    container2Ptrs.push_back( new ProcessContainer(sigmaPtr) );
  } 

  // A W+- and a hard jet.
  if (Settings::flag("SecondHard:WAndJet")) {
    sigmaPtr = new Sigma2qqbar2Wg;
    container2Ptrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2qg2Wq;
    container2Ptrs.push_back( new ProcessContainer(sigmaPtr) );
  } 

  // Two b jets - already part of TwoJets sample above.
  if (Settings::flag("SecondHard:TwoBJets")) {
    sigmaPtr = new Sigma2gg2QQbar(5, 123);
    container2Ptrs.push_back( new ProcessContainer(sigmaPtr) );
    sigmaPtr = new Sigma2qqbar2QQbar(5, 124);
    container2Ptrs.push_back( new ProcessContainer(sigmaPtr) );
  }

  // Done. 
  return true;

}

//**************************************************************************

} // end namespace Pythia8

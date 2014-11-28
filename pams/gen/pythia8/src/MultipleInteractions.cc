// MultipleInteractions.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Function definitions (not found in the header) for the
// SigmaMultiple and MultipleInteractions classes.
  
#include "MultipleInteractions.h"

// Internal headers for special processes.
#include "SigmaQCD.h"
#include "SigmaEW.h"
#include "SigmaOnia.h"

namespace Pythia8 {

//**************************************************************************

// The SigmaMultiple class.

//*********

// Constants: could be changed here if desired, but normally should not.
// These are of technical nature, as described for each.

// The sum of outgoing masses must not be too close to the cm energy.
const double SigmaMultiple::MASSMARGIN = 0.1;

// Fraction of time not the dominant "gluon t-channel" process is picked.
const double SigmaMultiple::OTHERFRAC  = 0.2;

//*********

// Initialize the generation process for given beams.

bool SigmaMultiple::init(int inState, int processLevel) {

  // Reset vector sizes (necessary in case of re-initialization).
  if (sigmaT.size() > 0) {
    for (int i = 0; i < int(sigmaT.size()); ++i) delete sigmaT[i];
    sigmaT.resize(0);
  }
  if (sigmaU.size() > 0) {
    for (int i = 0; i < int(sigmaU.size()); ++i) delete sigmaU[i];
    sigmaU.resize(0);
  }

  // Always store mimimal set of processes:QCD 2 -> 2 t-channel.

  // Gluon-gluon instate.
  if (inState == 0) {
    sigmaT.push_back( new Sigma2gg2gg() );
    sigmaU.push_back( new Sigma2gg2gg() );

  // Quark-gluon instate.
  } else if (inState == 1) { 
    sigmaT.push_back( new Sigma2qg2qg() );
    sigmaU.push_back( new Sigma2qg2qg() );

  // Quark-(anti)quark instate.
  } else { 
    sigmaT.push_back( new Sigma2qq2qq() );
    sigmaU.push_back( new Sigma2qq2qq() );
  }

  // Normally store QCD processes to new flavour.
  if (processLevel > 0) { 
    if (inState == 0) {
      sigmaT.push_back( new Sigma2gg2qqbar() );
      sigmaU.push_back( new Sigma2gg2qqbar() );   
      sigmaT.push_back( new Sigma2gg2QQbar(4, 121) );
      sigmaU.push_back( new Sigma2gg2QQbar(4, 121) );   
      sigmaT.push_back( new Sigma2gg2QQbar(5, 123) );
      sigmaU.push_back( new Sigma2gg2QQbar(5, 123) );   
    } else if (inState == 2) { 
      sigmaT.push_back( new Sigma2qqbar2gg() );
      sigmaU.push_back( new Sigma2qqbar2gg() );
      sigmaT.push_back( new Sigma2qqbar2qqbarNew() );
      sigmaU.push_back( new Sigma2qqbar2qqbarNew() );
      sigmaT.push_back( new Sigma2qqbar2QQbar(4, 122) );
      sigmaU.push_back( new Sigma2qqbar2QQbar(4, 122) );   
      sigmaT.push_back( new Sigma2qqbar2QQbar(5, 124) );
      sigmaU.push_back( new Sigma2qqbar2QQbar(5, 124) );   
    }  
  }

  // Optionally store electroweak processes, mainly photon production.
  if (processLevel > 1) { 
    if (inState == 0) {
      sigmaT.push_back( new Sigma2gg2ggamma() );
      sigmaU.push_back( new Sigma2gg2ggamma() );   
      sigmaT.push_back( new Sigma2gg2gammagamma() );
      sigmaU.push_back( new Sigma2gg2gammagamma() );   
    } else if (inState == 1) { 
      sigmaT.push_back( new Sigma2qg2qgamma() );
      sigmaU.push_back( new Sigma2qg2qgamma() );
    } else if (inState == 2) { 
      sigmaT.push_back( new Sigma2qqbar2ggamma() );
      sigmaU.push_back( new Sigma2qqbar2ggamma() );
      sigmaT.push_back( new Sigma2ffbar2gammagamma() );
      sigmaU.push_back( new Sigma2ffbar2gammagamma() );
      sigmaT.push_back( new Sigma2ffbar2ffbarsgm() );
      sigmaU.push_back( new Sigma2ffbar2ffbarsgm() );
    }
    if (inState >= 2) {
      sigmaT.push_back( new Sigma2ff2fftgmZ() );
      sigmaU.push_back( new Sigma2ff2fftgmZ() );         
      sigmaT.push_back( new Sigma2ff2fftW() );
      sigmaU.push_back( new Sigma2ff2fftW() );         
    }
  }

  // Optionally store charmonium and bottomonium production.
  if (processLevel > 2) { 
    if (inState == 0) {
      sigmaT.push_back( new Sigma2gg2QQbar3S11g(4, 401) );
      sigmaU.push_back( new Sigma2gg2QQbar3S11g(4, 401) );
      sigmaT.push_back( new Sigma2gg2QQbar3PJ1g(4, 0, 402) );
      sigmaU.push_back( new Sigma2gg2QQbar3PJ1g(4, 0, 402) );
      sigmaT.push_back( new Sigma2gg2QQbar3PJ1g(4, 1, 403) );
      sigmaU.push_back( new Sigma2gg2QQbar3PJ1g(4, 1, 403) );
      sigmaT.push_back( new Sigma2gg2QQbar3PJ1g(4, 2, 404) );
      sigmaU.push_back( new Sigma2gg2QQbar3PJ1g(4, 2, 404) );
      sigmaT.push_back( new Sigma2gg2QQbarX8g(4, 0, 411) );
      sigmaU.push_back( new Sigma2gg2QQbarX8g(4, 0, 411) );
      sigmaT.push_back( new Sigma2gg2QQbarX8g(4, 1, 412) );
      sigmaU.push_back( new Sigma2gg2QQbarX8g(4, 1, 412) );
      sigmaT.push_back( new Sigma2gg2QQbarX8g(4, 2, 413) );
      sigmaU.push_back( new Sigma2gg2QQbarX8g(4, 2, 413) );
      sigmaT.push_back( new Sigma2gg2QQbar3S11g(5, 501) );
      sigmaU.push_back( new Sigma2gg2QQbar3S11g(5, 501) );
      sigmaT.push_back( new Sigma2gg2QQbar3PJ1g(5, 0, 502) );
      sigmaU.push_back( new Sigma2gg2QQbar3PJ1g(5, 0, 502) );
      sigmaT.push_back( new Sigma2gg2QQbar3PJ1g(5, 1, 503) );
      sigmaU.push_back( new Sigma2gg2QQbar3PJ1g(5, 1, 503) );
      sigmaT.push_back( new Sigma2gg2QQbar3PJ1g(5, 2, 504) );
      sigmaU.push_back( new Sigma2gg2QQbar3PJ1g(5, 2, 504) );
      sigmaT.push_back( new Sigma2gg2QQbarX8g(5, 0, 511) );
      sigmaU.push_back( new Sigma2gg2QQbarX8g(5, 0, 511) );
      sigmaT.push_back( new Sigma2gg2QQbarX8g(5, 1, 512) );
      sigmaU.push_back( new Sigma2gg2QQbarX8g(5, 1, 512) );
      sigmaT.push_back( new Sigma2gg2QQbarX8g(5, 2, 513) );
      sigmaU.push_back( new Sigma2gg2QQbarX8g(5, 2, 513) );
    } else if (inState == 1) { 
      sigmaT.push_back( new Sigma2qg2QQbar3PJ1q(4, 0, 405) );
      sigmaU.push_back( new Sigma2qg2QQbar3PJ1q(4, 0, 405) );
      sigmaT.push_back( new Sigma2qg2QQbar3PJ1q(4, 1, 406) );
      sigmaU.push_back( new Sigma2qg2QQbar3PJ1q(4, 1, 406) );
      sigmaT.push_back( new Sigma2qg2QQbar3PJ1q(4, 2, 407) );
      sigmaU.push_back( new Sigma2qg2QQbar3PJ1q(4, 2, 407) );
      sigmaT.push_back( new Sigma2qg2QQbarX8q(4, 0, 414) );
      sigmaU.push_back( new Sigma2qg2QQbarX8q(4, 0, 414) );
      sigmaT.push_back( new Sigma2qg2QQbarX8q(4, 1, 415) );
      sigmaU.push_back( new Sigma2qg2QQbarX8q(4, 1, 415) );
      sigmaT.push_back( new Sigma2qg2QQbarX8q(4, 2, 416) );
      sigmaU.push_back( new Sigma2qg2QQbarX8q(4, 2, 416) );
      sigmaT.push_back( new Sigma2qg2QQbar3PJ1q(5, 0, 505) );
      sigmaU.push_back( new Sigma2qg2QQbar3PJ1q(5, 0, 505) );
      sigmaT.push_back( new Sigma2qg2QQbar3PJ1q(5, 1, 506) );
      sigmaU.push_back( new Sigma2qg2QQbar3PJ1q(5, 1, 506) );
      sigmaT.push_back( new Sigma2qg2QQbar3PJ1q(5, 2, 507) );
      sigmaU.push_back( new Sigma2qg2QQbar3PJ1q(5, 2, 507) );
      sigmaT.push_back( new Sigma2qg2QQbarX8q(5, 0, 514) );
      sigmaU.push_back( new Sigma2qg2QQbarX8q(5, 0, 514) );
      sigmaT.push_back( new Sigma2qg2QQbarX8q(5, 1, 515) );
      sigmaU.push_back( new Sigma2qg2QQbarX8q(5, 1, 515) );
      sigmaT.push_back( new Sigma2qg2QQbarX8q(5, 2, 516) );
      sigmaU.push_back( new Sigma2qg2QQbarX8q(5, 2, 516) );
    } else if (inState == 2) { 
      sigmaT.push_back( new Sigma2qqbar2QQbar3PJ1g(4, 0, 408) );
      sigmaU.push_back( new Sigma2qqbar2QQbar3PJ1g(4, 0, 408) );
      sigmaT.push_back( new Sigma2qqbar2QQbar3PJ1g(4, 1, 409) );
      sigmaU.push_back( new Sigma2qqbar2QQbar3PJ1g(4, 1, 409) );
      sigmaT.push_back( new Sigma2qqbar2QQbar3PJ1g(4, 2, 410) );
      sigmaU.push_back( new Sigma2qqbar2QQbar3PJ1g(4, 2, 410) );
      sigmaT.push_back( new Sigma2qqbar2QQbarX8g(4, 0, 417) );
      sigmaU.push_back( new Sigma2qqbar2QQbarX8g(4, 0, 417) );
      sigmaT.push_back( new Sigma2qqbar2QQbarX8g(4, 1, 418) );
      sigmaU.push_back( new Sigma2qqbar2QQbarX8g(4, 1, 418) );
      sigmaT.push_back( new Sigma2qqbar2QQbarX8g(4, 2, 419) );
      sigmaU.push_back( new Sigma2qqbar2QQbarX8g(4, 2, 419) );
      sigmaT.push_back( new Sigma2qqbar2QQbar3PJ1g(5, 0, 508) );
      sigmaU.push_back( new Sigma2qqbar2QQbar3PJ1g(5, 0, 508) );
      sigmaT.push_back( new Sigma2qqbar2QQbar3PJ1g(5, 1, 509) );
      sigmaU.push_back( new Sigma2qqbar2QQbar3PJ1g(5, 1, 509) );
      sigmaT.push_back( new Sigma2qqbar2QQbar3PJ1g(5, 2, 510) );
      sigmaU.push_back( new Sigma2qqbar2QQbar3PJ1g(5, 2, 510) );
      sigmaT.push_back( new Sigma2qqbar2QQbarX8g(5, 0, 517) );
      sigmaU.push_back( new Sigma2qqbar2QQbarX8g(5, 0, 517) );
      sigmaT.push_back( new Sigma2qqbar2QQbarX8g(5, 1, 518) );
      sigmaU.push_back( new Sigma2qqbar2QQbarX8g(5, 1, 518) );
      sigmaT.push_back( new Sigma2qqbar2QQbarX8g(5, 2, 519) );
      sigmaU.push_back( new Sigma2qqbar2QQbarX8g(5, 2, 519) );
    }
  }

  // Resize arrays to match sizes above.
  nChan = sigmaT.size();
  needMasses.resize(nChan);
  m3Fix.resize(nChan);
  m4Fix.resize(nChan);
  sHatMin.resize(nChan);
  sigmaTval.resize(nChan);
  sigmaUval.resize(nChan);

  // Initialize the processes.
  for (int i = 0; i < nChan; ++i) {
    sigmaT[i]->initProc();
    sigmaU[i]->initProc();

    // Prepare for massive kinematics (but fixed masses!) where required.
    needMasses[i] = false;
    int id3Mass =  sigmaT[i]->id3Mass();
    int id4Mass =  sigmaT[i]->id4Mass();
    m3Fix[i] = 0.;
    m4Fix[i] = 0.;
    if (id3Mass > 0 || id4Mass > 0) {
      needMasses[i] = true;
      m3Fix[i] =  ParticleDataTable::m0(id3Mass); 
      m4Fix[i] =  ParticleDataTable::m0(id4Mass); 
    }
    sHatMin[i] = pow2( m3Fix[i] + m4Fix[i] + MASSMARGIN); 
  }

  // Done.
  return true;

}

//*********

// Calculate cross section summed over possibilities.

double SigmaMultiple::sigma( int id1, int id2, double x1, double x2, 
  double sHat, double tHat, double uHat, double alpS, double alpEM) {

  // Choose either the dominant process (in slot 0) or the rest of them.
  bool pickOther = (Rndm::flat() < OTHERFRAC);

  // Iterate over all subprocesses.
  sigmaTsum = 0.;
  sigmaUsum = 0.;
  for (int i = 0; i < nChan; ++i) {
    sigmaTval[i] = 0.;
    sigmaUval[i] = 0.;

    // Skip the not chosen processes.
    if (i == 0 && pickOther) continue;
    if (i > 0 && !pickOther) continue; 

    // t-channel-sampling contribution. 
    if (sHat > sHatMin[i]) { 
      sigmaT[i]->set2KinMI( x1, x2, sHat, tHat, uHat, 
        alpS, alpEM, needMasses[i], m3Fix[i], m4Fix[i]);
      sigmaTval[i] = sigmaT[i]->sigmaHatWrap(id1, id2);
      sigmaT[i]->pickInState(id1, id2);
      // Correction factor for tHat rescaling in massive kinematics.
      if (needMasses[i]) sigmaTval[i] *= sigmaT[i]->sHBetaMI() / sHat;
      sigmaTsum += sigmaTval[i];
    } 
   
    // u-channel-sampling contribution.
    if (sHat > sHatMin[i]) { 
      sigmaU[i]->set2KinMI( x1, x2, sHat, uHat, tHat, 
        alpS, alpEM, needMasses[i], m3Fix[i], m4Fix[i]);
      sigmaUval[i] = sigmaU[i]->sigmaHatWrap( id1, id2);
      sigmaU[i]->pickInState(id1, id2);
      // Correction factor for tHat rescaling in massive kinematics.
      if (needMasses[i]) sigmaUval[i] *= sigmaU[i]->sHBetaMI() / sHat;
      sigmaUsum += sigmaUval[i];
    } 

  // Average of t- and u-channel sampling; corrected for not selected channels.
  }
  double sigmaAvg = 0.5 * (sigmaTsum + sigmaUsum);
  if (pickOther) sigmaAvg /= OTHERFRAC;
  if (!pickOther) sigmaAvg /= (1. - OTHERFRAC); 
  return sigmaAvg;

}

//*********

// Return one subprocess, picked according to relative cross sections.

SigmaProcess* SigmaMultiple::sigmaSel() { 

  // Decide between t- and u-channel-sampled kinematics.
  pickedU = (Rndm::flat() * (sigmaTsum + sigmaUsum) < sigmaUsum);

  // Pick one of t-channel-sampled processes.
  if (!pickedU) {
    double sigmaRndm = sigmaTsum * Rndm::flat();
    int    iPick = -1;
    do     sigmaRndm -= sigmaTval[++iPick];
    while  (sigmaRndm > 0.);
    return sigmaT[iPick];

  // Pick one of u-channel-sampled processes.
  } else {
    double sigmaRndm = sigmaUsum * Rndm::flat();
    int    iPick = -1;
    do     sigmaRndm -= sigmaUval[++iPick];
    while  (sigmaRndm > 0.);
    return sigmaU[iPick];
  }

}

//**************************************************************************

// The MultipleInteractions class.

//*********

// Constants: could be changed here if desired, but normally should not.
// These are of technical nature, as described for each.

// Factorization scale pT2 by default, but could be shifted to pT2 + pT02.
// (A priori possible, but problems for flavour threshold interpretation.)
const bool   MultipleInteractions::SHIFTFACSCALE = false;

// Naive upper estimate of cross section too pessimistic, so reduce by this.
const double MultipleInteractions::SIGMAFUDGE    = 0.7; 

// Number of bins that the dpT2 / (pT2 + r * pT20)^2 range is split into.
const int    MultipleInteractions::NBINS         = 100;

// The r value above, picked to allow a flatter correct/trial cross section.
const double MultipleInteractions::RPT20         = 0.25;

// Reduce pT0 by factor pT0STEP if sigmaInt < SIGMASTEP * sigmaND.
const double MultipleInteractions::PT0STEP       = 0.9;
const double MultipleInteractions::SIGMASTEP     = 1.1;

// Refuse too low expPow in impact parameter profile.
const double MultipleInteractions::EXPPOWMIN     = 0.4; 

// Define low-b region by interaction rate above given number.
const double MultipleInteractions::PROBATLOWB    = 0.6;

// Basic step size for b integration; sometimes modified.
const double MultipleInteractions::BSTEP         = 0.01;

// Stop b integration when integrand dropped enough.
const double MultipleInteractions::BMAX          = 1e-8;

// Do not allow too large argument to exp function.
const double MultipleInteractions::EXPMAX        = 50.;

// Convergence criterion for k iteration.
const double MultipleInteractions::KCONVERGE     = 1e-7;

// Conversion of GeV^{-2} to mb for cross section.
const double MultipleInteractions::CONVERT2MB    = 0.389380; 

//*********

// Initialize the generation process for given beams.

bool MultipleInteractions::init( bool doMIinit, Info* infoPtrIn, 
  BeamParticle* beamAPtrIn, BeamParticle* beamBPtrIn, 
  SigmaTotal* sigmaTotPtrIn, ostream& os) {

  // Store input pointers for future use. Done if no initialization. 
  infoPtr      = infoPtrIn;
  beamAPtr     = beamAPtrIn;
  beamBPtr     = beamBPtrIn;
  sigmaTotPtr  = sigmaTotPtrIn;
  if (!doMIinit) return false;

  // Matching in pT of hard interaction to further interactions.
  pTmaxMatch   = Settings::mode("MultipleInteractions:pTmaxMatch"); 

  //  Parameters of alphaStrong generation.
  alphaSvalue  = Settings::parm("MultipleInteractions:alphaSvalue");
  alphaSorder  = Settings::mode("MultipleInteractions:alphaSorder");

  // Parameters of alphaEM generation.
  alphaEMorder = Settings::mode("MultipleInteractions:alphaEMorder");

  //  Parameters of cross section generation.
  Kfactor      = Settings::parm("MultipleInteractions:Kfactor");

  // Regularization of QCD evolution for pT -> 0. 
  pT0Ref       = Settings::parm("MultipleInteractions:pT0Ref");
  ecmRef       = Settings::parm("MultipleInteractions:ecmRef");
  ecmPow       = Settings::parm("MultipleInteractions:ecmPow");
  pTmin        = Settings::parm("MultipleInteractions:pTmin");

  // Impact parameter profile.
  bProfile     = Settings::mode("MultipleInteractions:bProfile");
  coreRadius   = Settings::parm("MultipleInteractions:coreRadius");
  coreFraction = Settings::parm("MultipleInteractions:coreFraction");
  expPow       = Settings::parm("MultipleInteractions:expPow");
  expPow       = max(EXPPOWMIN, expPow);

  // Process sets to include in machinery.
  processLevel = Settings::mode("MultipleInteractions:processLevel");

  // Various other parameters. 
  nQuarkIn     = Settings::mode("MultipleInteractions:nQuarkIn");
  nSample      = Settings::mode("MultipleInteractions:nSample");

  // Some common combinations for double Gaussian, as shorthand.
  if (bProfile == 2) {
    fracA      = pow2(1. - coreFraction);
    fracB      = 2. * coreFraction * (1. - coreFraction);
    fracC      = pow2(coreFraction); 
    radius2B   = 0.5 * (1. + pow2(coreRadius));
    radius2C   = pow2(coreRadius);

  // Some common combinations for exp(b^pow), as shorthand.
  } else if (bProfile == 3) {
    hasLowPow  = (expPow < 2.);
    expRev     = 2. / expPow - 1.;
  } 

  // Initialize alpha_strong generation.
  alphaS.init( alphaSvalue, alphaSorder); 

  // Initialize alphaEM generation.
  alphaEM.init( alphaEMorder); 

  // Attach matrix-element calculation objects.
  sigma2gg.init( 0, processLevel);
  sigma2qg.init( 1, processLevel);
  sigma2qqbarSame.init( 2, processLevel);
  sigma2qq.init( 3, processLevel);

  // Calculate invariant mass of system. Set current pT0 scale.
  eCM  = infoPtr->eCM();
  sCM  = eCM * eCM;
  pT0 = pT0Ref * pow(eCM / ecmRef, ecmPow);

  // Get the total inelastic and nondiffractive cross section. Output.
  if (!sigmaTotPtr->hasSigmaTot()) return false;
  sigmaND = sigmaTotPtr->sigmaND();
  os << "\n *-------  PYTHIA Multiple Interactions Initialization  --"
     << "-----* \n"
     << " |                                                        "
     << "     | \n"
     << " |                 sigmaNonDiffractive = " << fixed 
     << setprecision(2) << setw(7) << sigmaND << " mb            | \n"
     << " |                                                        "
     << "     | \n";

  // The pT0 value may need to be decreased, if sigmaInt < sigmaND.
  double pT4dSigmaMaxBeg = 0.;
  for ( ; ; ) { 

    // Derived pT kinematics combinations.
    pT20         = pT0*pT0;
    pT2min       = pTmin*pTmin;
    pTmax        = 0.5*eCM;
    pT2max       = pTmax*pTmax;
    pT20R        = RPT20 * pT20;
    pT20minR     = pT2min + pT20R;
    pT20maxR     = pT2max + pT20R;
    pT20min0maxR = pT20minR * pT20maxR;
    pT2maxmin    = pT2max - pT2min;   

    // Provide upper estimate of interaction rate d(Prob)/d(pT2).
    upperEnvelope();

    // Integrate the parton-parton interaction cross section.
    pT4dSigmaMaxBeg = pT4dSigmaMax;
    jetCrossSection();

    // Sufficiently big SigmaInt or reduce pT0. Output.
    if (sigmaInt > SIGMASTEP * sigmaND) break; 
    os << " |  pT0 = "  << setw(5) << pT0 << " gives sigmaInteraction = " 
       << setw(7) << sigmaInt << " mb: rejected  | \n";
    pT0 *= PT0STEP;
  }
  os << " |  pT0 = " << setw(5) << pT0 << " gives sigmaInteraction = " 
     << setw(7) << sigmaInt << " mb: accepted  | \n"
     << " |                                                        "
     << "     | \n"
     << " *-------  End PYTHIA Multiple Interactions Initialization"
     << "  ---* " << endl;

  // Amount of violation from upperEnvelope to jetCrossSection.
  if (pT4dSigmaMax > pT4dSigmaMaxBeg) {  
    ostringstream osWarn;
    osWarn << "by factor " << fixed << setprecision(3) 
           << pT4dSigmaMax/pT4dSigmaMaxBeg;
    infoPtr->errorMsg("Warning in MultipleInteractions::init:"
      " maximum increased", osWarn.str());
  }

  // Calculate factor relating matter overlap and interaction rate.
  overlapInit();

  // Reset statistics.
  SigmaMultiple* dSigma;
  for (int i = 0; i < 4; ++i) {
    if      (i == 0) dSigma = &sigma2gg; 
    else if (i == 1) dSigma = &sigma2qg;
    else if (i == 2) dSigma = &sigma2qqbarSame;
    else             dSigma = &sigma2qq;
    int nProc = dSigma->nProc();
    for (int iProc = 0; iProc < nProc; ++iProc)
      nGen[ dSigma->codeProc(iProc) ] = 0;
  }

  // Done.
  return true;
}

//*********

// Select first = hardest pT in minbias process.
// Requires separate treatment at low and high b values

void MultipleInteractions::pTfirst() {  

  // Pick impact parameter and thereby interaction rate enhancement.
  overlapFirst();
  bSetInFirst = true;
  double WTacc;

  // At low b values evolve downwards with Sudakov. 
  if (isAtLowB) {
    pT2 = pT2max;
    do {

      // Pick a pT using a quick-and-dirty cross section estimate.
      pT2 = fastPT2(pT2);

      // If fallen below lower cutoff then need to restart at top.
      if (pT2 < pT2min) {
        pT2 = pT2max;
        WTacc = 0.;

      // Else pick complete kinematics and evaluate cross-section correction.
      } else WTacc = sigmaPT2(true) / dSigmaApprox;
    
    // Loop until acceptable pT and acceptable kinematics.
    } while (WTacc < Rndm::flat() || !dSigmaDtSel->final2KinMI()); 

  // At high b values make preliminary pT choice without Sudakov factor.
  } else {
    do {
      pT2 = pT20min0maxR / (pT20minR + Rndm::flat() * pT2maxmin) - pT20R; 

      // Evaluate upper estimate of cross section for this pT2 choice.  
      dSigmaApprox = pT4dSigmaMax / pow2(pT2 + pT20R);

      // Pick complete kinematics and evaluate cross-section correction.
      WTacc = sigmaPT2(true) / dSigmaApprox;

      // Evaluate and include Sudakov factor.
      WTacc *= sudakov( pT2, enhanceB);
    
    // Loop until acceptable pT and acceptable kinematics.
    } while (WTacc < Rndm::flat() || !dSigmaDtSel->final2KinMI()); 
  }
  
}

//*********

// Set up kinematics for first = hardest pT in minbias process.

void MultipleInteractions::setupFirstSys( Event& process) { 

  // Remove any partons of previous failed interactions.
  if (process.size() > 3) {
    process.popBack( process.size() - 3);
    process.initColTag();
  }

  // Loop over four partons and offset info relative to subprocess itself.
  int colOffset = process.lastColTag();
  double pTMI= 0.;
  for (int i = 1; i <= 4; ++i) {
    Particle parton = dSigmaDtSel->getParton(i);
    if (i <= 2 ) parton.mothers( i, 0);  
    else parton.mothers( 3, 4);
    if (i <= 2 ) parton.daughters( 5, 6);
    else parton.daughters( 0, 0);
    int col = parton.col();
    if (col > 0) parton.col( col + colOffset);
    int acol = parton.acol();
    if (acol > 0) parton.acol( acol + colOffset);

    // Put the partons into the event record.
    process.append(parton);
    if (i == 3) pTMI = parton.pT();
  }

  // Set scale from which to begin evolution.
  process.scale(  sqrt(pT2Fac) );

  // Info on subprocess - specific to mimimum-bias events.
  string nameSub = dSigmaDtSel->name();
  int codeSub    = dSigmaDtSel->code();
  int nFinalSub  = dSigmaDtSel->nFinal();
  infoPtr->setSubType( nameSub, codeSub, nFinalSub);
  infoPtr->setTypeMI( codeSub, pTMI);

  // Further standard info on process.
  infoPtr->setPDFalpha( id1, id2, xPDF1now, xPDF2now, pT2Fac, alpEM, alpS, 
    pT2Ren);
  double m3    = dSigmaDtSel->m(3);
  double m4    = dSigmaDtSel->m(4); 
  double theta = dSigmaDtSel->thetaMI(); 
  double phi   = dSigmaDtSel->phiMI(); 
  infoPtr->setKin( x1, x2, sHat, tHat, uHat, sqrt(pT2), m3, m4, theta, phi);

}

//*********

// Find whether to limit maximum scale of emissions.

bool MultipleInteractions::limitPTmax( Event& event) {

  // User-set cases.
  if (pTmaxMatch == 1) return true;
  if (pTmaxMatch == 2) return false;
   
  // Look if only quarks (u, d, s, c, b), gluons and photons in final state. 
  bool onlyQGP = true;
  for (int i = 5; i < event.size(); ++i) 
  if (event[i].status() != -21) {
    int idAbs = event[i].idAbs();
    if (idAbs > 5 && idAbs != 21 && idAbs != 22) onlyQGP = false;
  }
  return (onlyQGP);
 
}

//*********

// Select next pT in downwards evolution.

double MultipleInteractions::pTnext( double pTbegAll, double pTendAll) {

  // Pick a pT using a quick-and-dirty cross section estimate.
  double WTacc;
  double pT2end = pow2( max(pTmin, pTendAll) );
  pT2 = pow2(pTbegAll);
  do {
    pT2 = fastPT2(pT2);
    if (pT2 < pT2end) return 0.;

    // Pick complete kinematics and evaluate cross-section correction.
    // Note: dSigmaApprox was set in fastPT2 above. 
    WTacc = sigmaPT2(false) / dSigmaApprox;
    if (WTacc > 1.) infoPtr->errorMsg("Warning in MultipleInteractions::"
      "pTnext: weight above unity");
 
    // Decide whether to keep the event.
  } while (WTacc < Rndm::flat() || !dSigmaDtSel->final2KinMI()); 

  // Done.
  return sqrt(pT2);

}

//*********

// Set up the kinematics of the 2 -> 2 scattering process,
// and store the scattering in the event record.

void MultipleInteractions::scatter( Event& event) {

  // Loop over four partons and offset info relative to subprocess itself.
  int motherOffset = event.size();
  int colOffset = event.lastColTag();
  double pTMI= 0.;
  for (int i = 1; i <= 4; ++i) {
    Particle parton = dSigmaDtSel->getParton(i);
    if (i <= 2 ) parton.mothers( i, 0);  
    else parton.mothers( motherOffset, motherOffset + 1);
    if (i <= 2 ) parton.daughters( motherOffset + 2, motherOffset + 3);
    else parton.daughters( 0, 0);
    int col = parton.col();
    if (col > 0) parton.col( col + colOffset);
    int acol = parton.acol();
    if (acol > 0) parton.acol( acol + colOffset);

    // Put the partons into the event record.
    event.append(parton);
    if (i == 3) pTMI = parton.pT();
  }

  // Store participating partons as a new set in list of all systems.
  int iSys = event.newSystem();
  for (int i = 0; i < 4; ++i) event.addToSystem(iSys, motherOffset + i); 

  // Add scattered partons to list in beam remnants.
  int iA = beamAPtr->append( motherOffset, id1, x1);
  int iB = beamBPtr->append( motherOffset + 1, id2, x2);

  // Find whether incoming partons are valence or sea, so prepared for ISR.
  beamAPtr->xfISR( iA, id1, x1, pT2);
  beamAPtr->pickValSeaComp(); 
  beamBPtr->xfISR( iB, id2, x2, pT2);
  beamBPtr->pickValSeaComp(); 

  // Store info on subprocess code.
  int codeMI = dSigmaDtSel->code();
  infoPtr->setTypeMI( codeMI, pTMI);

  // Done.
} 

//*********

// Determine constant in d(Prob)/d(pT2) < const / (pT2 + r * pT20)^2.  

void MultipleInteractions::upperEnvelope() {  

  // Initially determine constant in jet cross section upper estimate 
  // d(sigma_approx)/d(pT2) < const / (pT2 + r * pT20)^2. 
  pT4dSigmaMax = 0.;
  
  // Loop thorough allowed pT range logarithmically evenly.
  for (int iPT = 0; iPT < NBINS; ++iPT) {
    double pT = pTmin * pow( pTmax/pTmin, (iPT + 0.5) / NBINS);
    pT2       = pT*pT;
    pT2shift  = pT2 + pT20;
    pT2Ren    = pT2shift;
    pT2Fac    = (SHIFTFACSCALE) ? pT2shift : pT2;
    xT        = 2. * pT / eCM;

    // Evaluate parton density sums at x1 = x2 = xT.
    double xPDF1sumMax = (9./4.) * beamAPtr->xf(21, xT, pT2Fac);
    for (int id = 1; id <= nQuarkIn; ++id) 
      xPDF1sumMax += beamAPtr->xf( id, xT, pT2Fac) 
                   + beamAPtr->xf(-id, xT, pT2Fac);
    double xPDF2sumMax = (9./4.) * beamBPtr->xf(21, xT, pT2Fac);
    for (int id = 1; id <= nQuarkIn; ++id)
      xPDF2sumMax += beamBPtr->xf( id, xT, pT2Fac) 
                   + beamBPtr->xf(-id, xT, pT2Fac);

    // Evaluate alpha_strong and _EM, matrix element and phase space volume.
    alpS  = alphaS.alphaS(pT2Ren);
    alpEM = alphaEM.alphaEM(pT2Ren);
    double dSigmaPartonApprox = CONVERT2MB * Kfactor * 0.5 * M_PI 
      * pow2(alpS / pT2shift);
    double yMax = log(1./xT + sqrt(1./(xT*xT) - 1.));
    double volumePhSp = pow2(2. * yMax);
  
    // Final comparison to determine upper estimate.
    double dSigmaApproxNow = SIGMAFUDGE * xPDF1sumMax * xPDF2sumMax 
      * dSigmaPartonApprox * volumePhSp;
    double pT4dSigmaNow = pow2(pT2 + pT20R) * dSigmaApproxNow;
    if ( pT4dSigmaNow > pT4dSigmaMax) pT4dSigmaMax = pT4dSigmaNow;
  } 
  
  // Get wanted constant by dividing by the nondiffractive cross section.   
  pT4dProbMax = pT4dSigmaMax / sigmaND;

}

//*********

// Integrate the parton-parton interaction cross section,
// using stratified Monte Carlo sampling.
// Store result in pT bins for use as Sudakov form factors.

void MultipleInteractions::jetCrossSection() {

  // Common factor from bin size in dpT2 / (pT2 + r * pT20)^2 and statistics.   
  double sigmaFactor = (1. / pT20minR - 1. / pT20maxR) / (NBINS * nSample);
  
  // Loop through allowed pT range evenly in dpT2 / (pT2 + r * pT20)^2.
  sigmaInt         = 0.; 
  double dSigmaMax = 0.;
  sudExpPT[NBINS]  = 0.;
  for (int iPT = NBINS - 1; iPT >= 0; --iPT) {
    double sigmaSum = 0.;

    // In each pT bin sample a number of random pT values.
    for (int iSample = 0; iSample < nSample; ++iSample) {
      double mappedPT2 = 1. - (iPT + Rndm::flat()) / NBINS;
      pT2 = pT20min0maxR / (pT20minR + mappedPT2 * pT2maxmin) - pT20R;

      // Evaluate cross section dSigma/dpT2 in phase space point.
      double dSigma = sigmaPT2(true);

     // Multiply by (pT2 + r * pT20)^2 to compensate for pT sampling. Sum.
      dSigma   *= pow2(pT2 + pT20R);
      sigmaSum += dSigma; 
      if (dSigma > dSigmaMax) dSigmaMax = dSigma;      
    }

    // Store total cross section and exponent of Sudakov.
    sigmaSum *= sigmaFactor;
    sigmaInt += sigmaSum;
    sudExpPT[iPT] =  sudExpPT[iPT + 1] + sigmaSum / sigmaND;

  // End of loop over pT values.
  } 

  // Update upper estimate of differential cross section. Done.
  if (dSigmaMax  > pT4dSigmaMax) {
    pT4dSigmaMax = dSigmaMax;
    pT4dProbMax  = dSigmaMax / sigmaND;
  }

}  

//*********

// Evaluate "Sudakov form factor" for not having a harder interaction
// at the selected b value, given the pT scale of the event.

double MultipleInteractions::sudakov(double pT2sud, double enhance) {

  // Find bin the pT2 scale falls in.
  double xBin = (pT2sud - pT2min) * pT20maxR 
    / (pT2maxmin * (pT2sud + pT20R)); 
  xBin = max(1e-6, min(NBINS - 1e-6, NBINS * xBin) );
  int iBin = int(xBin);

  // Interpolate inside bin. Optionally include enhancement factor.
  double sudExp = sudExpPT[iBin] + (xBin - iBin) 
    * (sudExpPT[iBin + 1] - sudExpPT[iBin]);
  return exp( -enhance * sudExp);
  
} 

//*********

// Pick a trial next pT, based on a simple upper estimate of the
// d(sigma)/d(pT2) spectrum.

double MultipleInteractions::fastPT2( double pT2beg) {

  // Use d(Prob)/d(pT2) < pT4dProbMax / (pT2 + r * pT20)^2. 
  double pT20begR       = pT2beg + pT20R;
  double pT4dProbMaxNow = pT4dProbMax * enhanceB; 
  double pT2try         = pT4dProbMaxNow * pT20begR 
    / (pT4dProbMaxNow - pT20begR * log(Rndm::flat())) - pT20R;

  // Save cross section associated with ansatz above. Done.
  dSigmaApprox = pT4dSigmaMax / pow2(pT2try + pT20R);
  return pT2try;

}

//*********

// Calculate the actual cross section to decide whether fast choice is OK.
// Select flavours and kinematics for interaction at given pT.
// Slightly different treatment for first interaction and subsequent ones.

double MultipleInteractions::sigmaPT2(bool isFirst) {
 
  // Derive shifted pT2 and rapidity limits from chosen pT2.
  pT2shift = pT2 + pT20;
  pT2Ren   = pT2shift;
  pT2Fac   = (SHIFTFACSCALE) ? pT2shift : pT2;
  xT       = 2. * sqrt(pT2) / eCM;
  if (xT >= 1.) return 0.;
  xT2      = xT*xT;   
  double yMax = log(1./xT + sqrt(1./xT2 - 1.));

  // Select rapidities y3 and y4 of the two produced partons.
  double y3 = yMax * (2. * Rndm::flat() - 1.);
  double y4 = yMax * (2. * Rndm::flat() - 1.);
  y = 0.5 * (y3 + y4);

  // Reject some events at large rapidities to improve efficiency.
  // (Don't have to evaluate PDF's and ME's.)
  double WTy = (1. - pow2(y3/yMax)) * (1. - pow2(y4/yMax));
  if (WTy < Rndm::flat()) return 0.; 

  // Failure if x1 or x2 exceed what is left in respective beam.
  x1 = 0.5 * xT * (exp(y3) + exp(y4));
  x2 = 0.5 * xT * (exp(-y3) + exp(-y4));
  if (isFirst) {
    if (x1 > 1. || x2 > 1.) return 0.; 
  } else {
    if (x1 > beamAPtr->xMax() || x2 > beamBPtr->xMax()) return 0.; 
  }
  tau = x1 * x2;

  // Begin evaluate parton densities at actual x1 and x2.
  double xPDF1[21];
  double xPDF1sum = 0.;
  double xPDF2[21];
  double xPDF2sum = 0.;

  // For first interaction use normal densities.
  if (isFirst) {
    for (int id = -nQuarkIn; id <= nQuarkIn; ++id) {
      if (id == 0) xPDF1[10] = (9./4.) * beamAPtr->xf(21, x1, pT2Fac);
      else xPDF1[id+10] = beamAPtr->xf(id, x1, pT2Fac);
      xPDF1sum += xPDF1[id+10];
    }
    for (int id = -nQuarkIn; id <= nQuarkIn; ++id) {
      if (id == 0) xPDF2[10] = (9./4.) * beamBPtr->xf(21, x2, pT2Fac);
      else xPDF2[id+10] = beamBPtr->xf(id, x2, pT2Fac);
      xPDF2sum += xPDF2[id+10];
    }
  
  // For subsequent interactions use rescaled densities.
  } else {
    for (int id = -nQuarkIn; id <= nQuarkIn; ++id) {
      if (id == 0) xPDF1[10] = (9./4.) * beamAPtr->xfMI(21, x1, pT2Fac);
      else xPDF1[id+10] = beamAPtr->xfMI(id, x1, pT2Fac);
      xPDF1sum += xPDF1[id+10];
    }
    for (int id = -nQuarkIn; id <= nQuarkIn; ++id) {
      if (id == 0) xPDF2[10] = (9./4.) * beamBPtr->xfMI(21, x2, pT2Fac);
      else xPDF2[id+10] = beamBPtr->xfMI(id, x2, pT2Fac);
      xPDF2sum += xPDF2[id+10];
    }
  }

  // Select incoming flavours according to actual PDF's.
  id1 = -nQuarkIn - 1;
  double temp = xPDF1sum * Rndm::flat();
  do { xPDF1now = xPDF1[(++id1) + 10]; temp -= xPDF1now; } 
  while (temp > 0. && id1 < nQuarkIn);
  if (id1 == 0) id1 = 21; 
  id2 = -nQuarkIn-1;
  temp = xPDF2sum * Rndm::flat();
  do { xPDF2now = xPDF2[(++id2) + 10]; temp -= xPDF2now;} 
  while (temp > 0. && id2 < nQuarkIn);  
  if (id2 == 0) id2 = 21; 

  // Assign pointers to processes relevant for incoming flavour choice:
  // g + g, q + g, q + qbar (same flavour), q + q(bar) (the rest).  
  // Factor 4./9. per incoming gluon to compensate for preweighting.  
  SigmaMultiple* dSigma;
  double gluFac = 1.;
  if (id1 == 21 && id2 == 21) { 
    dSigma = &sigma2gg; 
    gluFac = 16. / 81.;
  } else if (id1 == 21 || id2 == 21) { 
    dSigma = &sigma2qg; 
    gluFac = 4. / 9.;
  } else if (id1 == -id2) dSigma = &sigma2qqbarSame;
  else dSigma = &sigma2qq;

  // Prepare to generate differential cross sections.
  alpS  = alphaS.alphaS(pT2Ren);
  alpEM = alphaEM.alphaEM(pT2Ren);
  sHat  = tau * sCM;
  double root = sqrtpos(1. - xT2 / tau);
  tHat  = -0.5 * sHat * (1. - root);
  uHat  = -0.5 * sHat * (1. + root);

  // Evaluate cross sections, include possibility of K factor.
  // Use kinematics for two symmetrical configurations (tHat <-> uHat).
  // (Not necessary, but makes for better MC efficiency.)
  double dSigmaPartonCorr = Kfactor * gluFac 
    * dSigma->sigma( id1, id2, x1, x2, sHat, tHat, uHat, alpS, alpEM);

  // Pick one of the possible channels summed above.
  dSigmaDtSel = dSigma->sigmaSel();
  if (dSigma->swapTU()) swap( tHat, uHat);

  // Combine cross section, pdf's and phase space integral.
  double volumePhSp = pow2(2. * yMax) / WTy;
  double dSigmaCorr = dSigmaPartonCorr * xPDF1sum * xPDF2sum * volumePhSp;

  // Dampen cross section at small pT values; part of formalism.
  // Use pT2 corrected for massive kinematics at this step.
  double pT2massive = dSigmaDtSel->pT2MI();
  dSigmaCorr *= pow2( pT2massive / (pT2massive + pT20) );

  // Done.
  return dSigmaCorr;
}


//*********

// Calculate factor relating matter overlap and interaction rate,
// i.e. k in <n_interaction(b)> = k * overlap(b) (neglecting
// n_int = 0 corrections and energy-momentum conservation effects).

void MultipleInteractions::overlapInit() {

  // Initial values for iteration. Step size of b integration.
  nAvg = sigmaInt / sigmaND;
  kNow = 0.5;
  int stepDir = 1;
  double deltaB = BSTEP;
  if (bProfile == 2) deltaB *= min( 0.5, 2.5 * coreRadius); 
  if (bProfile == 3) deltaB *= max(1., pow(2. / expPow, 1. / expPow)); 
  
  // Further variables, with dummy initial values.
  double nNow           = 0.;
  double kLow           = 0.;
  double nLow           = 0.;
  double kHigh          = 0.;
  double nHigh          = 0.;
  double overlapNow     = 0.;
  double probNow        = 0.; 
  double overlapInt     = 0.5;
  double probInt        = 0.; 
  double probOverlapInt = 0.;
  double bProbInt       = 0.;
  normPi                = 1. / (2. * M_PI);

  // Subdivision into low-b and high-b region by interaction rate.
  bool pastBDiv = false;  
  double overlapHighB = 0.;

  // First close k into an interval by binary steps,
  // then find k by successive interpolation.  
  do {
    if (stepDir == 1) kNow *= 2.;
    else if (stepDir == -1) kNow *= 0.5;
    else kNow = kLow + (nAvg - nLow) * (kHigh - kLow) / (nHigh - nLow);

    // Overlap trivial if no impact parameter dependence.
    if (bProfile <= 0 || bProfile > 3) {
      probInt        = 0.5 * M_PI * (1. - exp(-kNow));
      probOverlapInt = probInt / M_PI;
      bProbInt       = probInt;

    // Else integrate overlap over impact parameter.
    } else { 

      // Reset integrals.
      overlapInt     = (bProfile == 3) ? 0. : 0.5;
      probInt        = 0.; 
      probOverlapInt = 0.;
      bProbInt       = 0.;
      pastBDiv       = false;
      overlapHighB   = 0.;

      // Step in b space.
      double b       = -0.5 * deltaB;
      double bArea   = 0.;
      do {
        b           += deltaB;
        bArea        = 2. * M_PI * b * deltaB;

        // Evaluate overlap at current b value.
        if (bProfile == 1) { 
          overlapNow = normPi * exp( -b*b);
	} else if (bProfile == 2) {          
          overlapNow = normPi * ( fracA * exp( -min(EXPMAX, b*b))
            + fracB * exp( -min(EXPMAX, b*b / radius2B)) / radius2B
            + fracC * exp( -min(EXPMAX, b*b / radius2C)) / radius2C );
	} else {
          overlapNow  = normPi * exp( -pow( b, expPow));  
          overlapInt += bArea * overlapNow;
	}
        if (pastBDiv) overlapHighB += bArea * overlapNow;

        // Calculate interaction probability and integrate.
        probNow         = 1. - exp( -min(EXPMAX, M_PI * kNow * overlapNow));
        probInt        += bArea * probNow;
        probOverlapInt += bArea * overlapNow * probNow;
        bProbInt       += b * bArea * probNow;

        // Check when interaction probability has dropped sufficiently.
        if (!pastBDiv && probNow < PROBATLOWB) {
          bDiv     = b + 0.5 * deltaB;
          pastBDiv = true;
        }

      // Continue out in b until overlap too small.
      } while (b < 1. || b * probNow > BMAX); 
    }      
 
    // Ratio of b-integrated k * overlap / (1 - exp( - k * overlap)).
    nNow = M_PI * kNow * overlapInt / probInt;

    // Replace lower or upper limit of k.
    if (nNow < nAvg) {
      kLow = kNow;
      nLow = nNow;
      if (stepDir == -1) stepDir = 0;
    } else {
      kHigh = kNow;
      nHigh = nNow;
      if (stepDir == 1) stepDir = -1;
    } 

  // Continue iteration until convergence.
  } while (abs(nNow - nAvg) > KCONVERGE * nAvg);

  // Save relevant final numbers for overlap values.
  double avgOverlap  = probOverlapInt / probInt; 
  zeroIntCorr = probOverlapInt / overlapInt; 
  normOverlap = normPi * zeroIntCorr / avgOverlap;
  bAvg = bProbInt / probInt;

  // Relative rates for preselection of low-b and high-b region.
  // Other useful combinations for subsequent selection.
  if (bProfile > 0 && bProfile <= 3) {
    probLowB = M_PI * bDiv*bDiv;
    double probHighB = M_PI * kNow * overlapHighB;
    if (bProfile == 1) probHighB = M_PI * kNow * 0.5 * exp( -bDiv*bDiv);
    else if (bProfile == 2) {
      fracAhigh   = fracA * exp( -bDiv*bDiv);
      fracBhigh   = fracB * exp( -bDiv*bDiv / radius2B);
      fracChigh   = fracC * exp( -bDiv*bDiv / radius2C);
      fracABChigh = fracAhigh + fracBhigh + fracChigh;
      probHighB   = M_PI * kNow * 0.5 * fracABChigh;
    } else { 
      cDiv = pow( bDiv, expPow);
      cMax = max(2. * expRev, cDiv); 
    } 
    probLowB /= (probLowB + probHighB);
  }

}

//*********

// Pick impact parameter and interaction rate enhancement beforehand,
// i.e. before even the hardest interaction for minimum-bias events. 

void MultipleInteractions::overlapFirst() {

  // Trivial values if no impact parameter dependence.
  if (bProfile <= 0 || bProfile > 3) {
    bNow     = bAvg;
    enhanceB = zeroIntCorr;
    bIsSet   = true;
    isAtLowB = true;
    return;
  }

  // Preliminary choice between and inside low-b and high-b regions.
  double overlapNow = 0.;
  double probAccept = 0.;
  do {

    // Treatment in low-b region: pick b flat in area.
    if (Rndm::flat() < probLowB) {
      isAtLowB = true;
      bNow = bDiv * sqrt(Rndm::flat());
      if (bProfile == 1) overlapNow = normPi * exp( -bNow*bNow);
      else if (bProfile == 2) overlapNow = normPi * 
        ( fracA * exp( -bNow*bNow)
        + fracB * exp( -bNow*bNow / radius2B) / radius2B
        + fracC * exp( -bNow*bNow / radius2C) / radius2C );
      else overlapNow = normPi * exp( -pow( bNow, expPow));
      probAccept = 1. - exp( -min(EXPMAX, M_PI * kNow * overlapNow));

    // Treatment in high-b region: pick b according to overlap.
    } else {

      // For simple and double Gaussian pick b according to exp(-b^2 / r^2).
      if (bProfile == 1) {
        bNow = sqrt(bDiv*bDiv - log(Rndm::flat()));
        overlapNow = normPi * exp( -min(EXPMAX, bNow*bNow));
      } else if (bProfile == 2) {
        double pickFrac = Rndm::flat() * fracABChigh; 
        if (pickFrac < fracAhigh) bNow = sqrt(bDiv*bDiv - log(Rndm::flat()));
        else if (pickFrac < fracAhigh + fracBhigh) 
          bNow = sqrt(bDiv*bDiv - radius2B * log(Rndm::flat()));
        else bNow = sqrt(bDiv*bDiv - radius2C * log(Rndm::flat()));
        overlapNow = normPi * ( fracA * exp( -min(EXPMAX, bNow*bNow))
          + fracB * exp( -min(EXPMAX, bNow*bNow / radius2B)) / radius2B
          + fracC * exp( -min(EXPMAX, bNow*bNow / radius2C)) / radius2C );

      // For exp( - b^expPow) transform to variable c = b^expPow so that
      // f(b) = b * exp( - b^expPow) -> f(c) = c^r * exp(-c) with r = expRev. 
      // case hasLowPow: expPow < 2 <=> r > 0: preselect according to
      // f(c) < N exp(-c/2) and then accept with N' * c^r * exp(-c/2). 
      } else if (hasLowPow) {
        double cNow, acceptC;
        do {      
          cNow = cDiv - 2. * log(Rndm::flat());
          acceptC = pow(cNow / cMax, expRev) * exp( -0.5 * (cNow - cMax));
        } while (acceptC < Rndm::flat());
        bNow = pow( cNow, 1. / expPow);
        overlapNow = normPi * exp( -cNow);

      // case !hasLowPow: expPow >= 2 <=> - 1 < r < 0: preselect according to
      // f(c) < N exp(-c) and then accept with N' * c^r. 
      } else {
        double cNow, acceptC;
        do {      
          cNow = cDiv - log(Rndm::flat());
          acceptC = pow(cNow / cDiv, expRev);
        } while (acceptC < Rndm::flat());
        bNow = pow( cNow, 1. / expPow);
        overlapNow = normPi * exp( -cNow);    
      }
      double temp = M_PI * kNow *overlapNow;
      probAccept = (1. - exp( -min(EXPMAX, temp))) / temp;    
    }

  // Confirm choice of b value. Derive enhancement factor.
  } while (probAccept < Rndm::flat());
  enhanceB = (normOverlap / normPi) * overlapNow ; 

  // Done. 
  bIsSet = true;

}

//*********

// Pick impact parameter and interaction rate enhancement afterwards,
// i.e. after a hard interaction is known but before rest of MI treatment.

void MultipleInteractions::overlapNext(double pTscale) {

  // Default, valid for bProfile = 0. Also initial Sudakov.
  enhanceB = zeroIntCorr;
  if (bProfile <= 0 || bProfile > 3) return; 
  double pT2scale = pTscale*pTscale;

  // Begin loop over pT-dependent rejection of b value.
  do {

    // Flat enhancement distribution for simple Gaussian.
    if (bProfile == 1) {
      double expb2 = Rndm::flat();
      enhanceB = normOverlap * expb2;  
      bNow = sqrt( -log(expb2));

    // For double Gaussian go the way via b, according to each Gaussian.
    } else if (bProfile == 2) {
      double bType = Rndm::flat();  
      double b2 = -log( Rndm::flat() );
      if (bType < fracA) ;
      else if (bType < fracA + fracB) b2 *= radius2B;
      else b2 *= radius2C; 
      enhanceB = normOverlap * ( fracA * exp( -min(EXPMAX, b2))
        + fracB * exp( -min(EXPMAX, b2 / radius2B)) / radius2B
        + fracC * exp( -min(EXPMAX, b2 / radius2C)) / radius2C ); 
      bNow = sqrt(b2);

    // For exp( - b^expPow) transform to variable c = b^expPow so that
    // f(b) = b * exp( - b^expPow) -> f(c) = c^r * exp(-c) with r = expRev. 
    // case hasLowPow: expPow < 2 <=> r > 0: 
    // f(c) < r^r exp(-r) for c < 2r, < (2r)^r exp(-r-c/2) for c > 2r.
    } else if (hasLowPow) {
      double cNow, acceptC;
      double probLowC = expRev / (expRev + pow(2., expRev) * exp( - expRev));
      do {
        if (Rndm::flat() < probLowC) {
          cNow = 2. * expRev * Rndm::flat();
          acceptC = pow( cNow / expRev, expRev) * exp(expRev - cNow);
        } else {
          cNow = 2. * (expRev - log( Rndm::flat() )); 
          acceptC = pow(0.5 * cNow / expRev, expRev) * exp(expRev - 0.5 * cNow);
        }
      } while (acceptC < Rndm::flat()); 
      enhanceB = normOverlap *exp(-cNow);  
      bNow = pow( cNow, 1. / expPow);

    // case !hasLowPow: expPow >= 2 <=> - 1 < r < 0: 
    // f(c) < c^r for c < 1,  f(c) < exp(-c) for c > 1.  
    } else {
      double cNow, acceptC;
      double probLowC = expPow / (2. * exp(-1.) + expPow);
      do { 
        if (Rndm::flat() < probLowC) {
          cNow = pow( Rndm::flat(), 0.5 * expPow);
          acceptC = exp(-cNow);
        } else {
          cNow = 1. - log( Rndm::flat() );
          acceptC = pow( cNow, expRev);    
        } 
      } while (acceptC < Rndm::flat());
      enhanceB = normOverlap * exp(-cNow);  
      bNow = pow( cNow, 1. / expPow);
    }

    // Evaluate "Sudakov form factor" for not having a harder interaction.
  } while (sudakov(pT2scale, enhanceB) < Rndm::flat());

  // Done.
  bIsSet = true;
}

//*********

// Printe statistics on number of multiple-interactions processes.

void MultipleInteractions::statistics(bool reset, ostream& os) {
    
  // Header.
  os << "\n *-------  PYTHIA Multiple Interactions Statistics  --------"
     << "---*\n"
     << " |                                                            "
     << " |\n" 
     << " |  Note: excludes hardest subprocess if already listed above " 
     << " |\n" 
     << " |                                                            "
     << " |\n" 
     << " | Subprocess                               Code |       Times"
     << " |\n" 
     << " |                                               |            "
     << " |\n"
     << " |------------------------------------------------------------"
     << "-|\n"
     << " |                                               |            "
     << " |\n";

  // Loop over existing processes. Sum of all subprocesses.
  int numberSum = 0;
  for ( map<int, int>::iterator iter = nGen.begin(); iter != nGen.end(); 
    ++iter) {  
    int code = iter -> first;
    int number = iter->second;
    numberSum += number;
  
    // Find process name that matches code.
    string name = " ";
    bool foundName = false;
    SigmaMultiple* dSigma;
    for (int i = 0; i < 4; ++i) {
      if      (i == 0) dSigma = &sigma2gg; 
      else if (i == 1) dSigma = &sigma2qg;
      else if (i == 2) dSigma = &sigma2qqbarSame;
      else             dSigma = &sigma2qq;
      int nProc = dSigma->nProc();
      for (int iProc = 0; iProc < nProc; ++iProc)
      if (dSigma->codeProc(iProc) == code) {
        name = dSigma->nameProc(iProc);
        foundName = true;
      } 
      if (foundName) break;      
    }

    // Print individual process info.
    os << " | " << left << setw(40) << name << right << setw(5) << code 
       << " | " << setw(11) << number << " |\n";
  }

  // Print summed process info.
  os << " |                                                            "
     << " |\n" 
     << " | " << left << setw(45) << "sum" << right << " | " << setw(11) 
       << numberSum  << " |\n";

    // Listing finished.
  os << " |                                               |            "
     << " |\n" 
     << " *-------  End PYTHIA Multiple Interactions Statistics -------"
     << "-*" << endl;

  // Optionally reset statistics contants.
  if (reset) for ( map<int, int>::iterator iter = nGen.begin(); 
    iter != nGen.end(); ++iter) iter->second = 0;  

}

//**************************************************************************

} // end namespace Pythia8

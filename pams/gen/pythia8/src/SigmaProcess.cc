// SigmaProcess.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Function definitions (not found in the header) for the 
// SigmaProcess class, and classes derived from it.

#include "SigmaProcess.h"

namespace Pythia8 {

//**************************************************************************

// The SigmaProcess class.
// Base class for cross sections.

//*********

// Constants: could be changed here if desired, but normally should not.
// These are of technical nature, as described for each.

// Conversion of GeV^{-2} to mb for cross section.
const double SigmaProcess::CONVERT2MB    = 0.389380; 

// The sum of outgoing masses must not be too close to the cm energy.
const double SigmaProcess::MASSMARGIN    = 0.1;

//*********

// Perform simple initialization and store pointers.

void SigmaProcess::init(Info* infoPtrIn, BeamParticle* beamAPtrIn, 
  BeamParticle* beamBPtrIn, AlphaStrong* alphaSPtrIn, 
  AlphaEM* alphaEMPtrIn,SigmaTotal* sigmaTotPtrIn,
  SusyLesHouches* slhaPtrIn) {

  // Store pointers.
  infoPtr     = infoPtrIn;
  beamAPtr    = beamAPtrIn;
  beamBPtr    = beamBPtrIn;
  alphaSPtr   = alphaSPtrIn;
  alphaEMPtr  = alphaEMPtrIn;
  sigmaTotPtr = sigmaTotPtrIn;
  slhaPtr     = slhaPtrIn;

  // Read out some properties of beams to allow shorthand.
  idA         = beamAPtr->id();
  idB         = beamBPtr->id();
  mA          = beamAPtr->m();
  mB          = beamBPtr->m();
  isLeptonA   = beamAPtr->isLepton();
  isLeptonB   = beamBPtr->isLepton();
  hasLeptonBeams = isLeptonA || isLeptonB;

  // K factor, multiplying resolved processes. (But not here for MI.)
  Kfactor         = Settings::parm("SigmaProcess:Kfactor");

  // Maximum incoming quark flavour.
  nQuarkIn        = Settings::mode("PDFinProcess:nQuarkIn");

  // Renormalization scale choice.
  renormScale1    = Settings::mode("SigmaProcess:renormScale1"); 
  renormScale2    = Settings::mode("SigmaProcess:renormScale2"); 
  renormScale3    = Settings::mode("SigmaProcess:renormScale3"); 
  renormScale3VV  = Settings::mode("SigmaProcess:renormScale3VV"); 
  renormMultFac   = Settings::parm("SigmaProcess:renormMultFac"); 
  renormFixScale  = Settings::parm("SigmaProcess:renormFixScale"); 

  // Factorization scale choice.
  factorScale1    = Settings::mode("SigmaProcess:factorScale1"); 
  factorScale2    = Settings::mode("SigmaProcess:factorScale2"); 
  factorScale3    = Settings::mode("SigmaProcess:factorScale3"); 
  factorScale3VV  = Settings::mode("SigmaProcess:factorScale3VV"); 
  factorMultFac   = Settings::parm("SigmaProcess:factorMultFac"); 
  factorFixScale  = Settings::parm("SigmaProcess:factorFixScale"); 

  // CP violation parameters for the BSM Higgs sector.
  higgsH1parity   = Settings::mode("HiggsH1:parity");
  higgsH1eta      = Settings::parm("HiggsH1:etaParity");
  higgsH2parity   = Settings::mode("HiggsH2:parity");
  higgsH2eta      = Settings::parm("HiggsH2:etaParity");
  higgsA3parity   = Settings::mode("HiggsA3:parity");
  higgsA3eta      = Settings::parm("HiggsA3:etaParity");

  // If BSM not switched on then H1 should have SM properties.
  if (!Settings::flag("Higgs:useBSM")){
    higgsH1parity = 1;
    higgsH1eta    = 0.;
  }

}

//*********

// Set up allowed flux of incoming partons.
// addBeam: set up PDF's that need to be evaluated for the two beams.
// addPair: set up pairs of incoming partons from the two beams.

bool SigmaProcess::initFlux() {

  // Read in process-specific channel information.
  string fluxType = inFlux();

  // Case with g g incoming state.
  if (fluxType == "gg") {
    addBeamA(21);
    addBeamB(21);
    addPair(21, 21);
  }

  // Case with q g incoming state.
  else if (fluxType == "qg") {
    for (int i = -nQuarkIn; i <= nQuarkIn; ++i) {
      int idNow = (i == 0) ? 21 : i;
      addBeamA(idNow);
      addBeamB(idNow);
    }
    for (int idNow = -nQuarkIn; idNow <= nQuarkIn; ++idNow) 
    if (idNow != 0) {
      addPair(idNow, 21);
      addPair(21, idNow);
    }
  }

  // Case with q q', q qbar' or qbar qbar' incoming state.
  else if (fluxType == "qq") {
    for (int idNow = -nQuarkIn; idNow <= nQuarkIn; ++idNow) 
    if (idNow != 0) {
      addBeamA(idNow);
      addBeamB(idNow);
    }
    for (int id1Now = -nQuarkIn; id1Now <= nQuarkIn; ++id1Now) 
    if (id1Now != 0) 
    for (int id2Now = -nQuarkIn; id2Now <= nQuarkIn; ++id2Now) 
    if (id2Now != 0) 
      addPair(id1Now, id2Now);
  }

  // Case with q qbar incoming state.
  else if (fluxType == "qqbarSame") {
    for (int idNow = -nQuarkIn; idNow <= nQuarkIn; ++idNow) 
    if (idNow != 0) {
      addBeamA(idNow);
      addBeamB(idNow);
    }
    for (int idNow = -nQuarkIn; idNow <= nQuarkIn; ++idNow) 
    if (idNow != 0) 
      addPair(idNow, -idNow);
  }

  // Case with f f', f fbar', fbar fbar' incoming state.
  else if (fluxType == "ff") {
    // If beams are leptons then they are also the colliding partons.
    if ( isLeptonA && isLeptonB ) {
      addBeamA(idA);
      addBeamB(idB);
      addPair(idA, idB);
    // First beam is lepton and second is hadron.
    } else if ( isLeptonA ) {
      addBeamA(idA);
      for (int idNow = -nQuarkIn; idNow <= nQuarkIn; ++idNow) 
      if (idNow != 0) {
        addBeamB(idNow);
        addPair(idA, idNow);
      }
    // First beam is hadron and second is lepton.
    } else if ( isLeptonB ) {
      addBeamB(idB);
      for (int idNow = -nQuarkIn; idNow <= nQuarkIn; ++idNow) 
      if (idNow != 0) {
        addBeamA(idNow);
        addPair(idNow, idB);
      }
    // Hadron beams gives quarks.
    } else {
      for (int idNow = -nQuarkIn; idNow <= nQuarkIn; ++idNow) 
      if (idNow != 0) {
        addBeamA(idNow);
        addBeamB(idNow);
      }
      for (int id1Now = -nQuarkIn; id1Now <= nQuarkIn; ++id1Now) 
      if (id1Now != 0) 
      for (int id2Now = -nQuarkIn; id2Now <= nQuarkIn; ++id2Now) 
      if (id2Now != 0) 
        addPair(id1Now, id2Now);
    }
  }

  // Case with f fbar incoming state.
  else if (fluxType == "ffbarSame") {
    // If beams are antiparticle pair and leptons then also colliding partons.
    if ( idA + idB == 0 && isLeptonA ) {
      addBeamA(idA);
      addBeamB(idB);
      addPair(idA, idB);
    // Else assume both to be hadrons, for better or worse.
    } else {
      for (int idNow = -nQuarkIn; idNow <= nQuarkIn; ++idNow) 
      if (idNow != 0) {
        addBeamA(idNow);
        addBeamB(idNow);
      }
      for (int idNow = -nQuarkIn; idNow <= nQuarkIn; ++idNow) 
      if (idNow != 0) 
        addPair(idNow, -idNow);
    }
  }

  // Case with f fbar' charged(+-1) incoming state.
  else if (fluxType == "ffbarChg") {
    // If beams are leptons then also colliding partons.
    if ( isLeptonA && isLeptonB && abs( ParticleDataTable::chargeType(idA) 
             + ParticleDataTable::chargeType(idB) ) == 3 ) {
      addBeamA(idA);
      addBeamB(idB);
      addPair(idA, idB);
    // Hadron beams gives quarks.
    } else {
      for (int idNow = -nQuarkIn; idNow <= nQuarkIn; ++idNow) 
      if (idNow != 0) {
        addBeamA(idNow);
        addBeamB(idNow);
      }
      for (int id1Now = -nQuarkIn; id1Now <= nQuarkIn; ++id1Now) 
      if (id1Now != 0) 
      for (int id2Now = -nQuarkIn; id2Now <= nQuarkIn; ++id2Now) 
      if (id2Now != 0 && id1Now * id2Now < 0 
        && (abs(id1Now) + abs(id2Now))%2 == 1) addPair(id1Now, id2Now);
    }
  }

  // Case with f fbar' generic incoming state.
  else if (fluxType == "ffbar") {
    // If beams are leptons then also colliding partons.
    if (isLeptonA && isLeptonB && idA * idB < 0) {
      addBeamA(idA);
      addBeamB(idB);
      addPair(idA, idB);
    // Hadron beams gives quarks.
    } else {
      for (int idNow = -nQuarkIn; idNow <= nQuarkIn; ++idNow) 
      if (idNow != 0) {
        addBeamA(idNow);
        addBeamB(idNow);
      }
      for (int id1Now = -nQuarkIn; id1Now <= nQuarkIn; ++id1Now) 
      if (id1Now != 0) 
      for (int id2Now = -nQuarkIn; id2Now <= nQuarkIn; ++id2Now) 
      if (id2Now != 0 && id1Now * id2Now < 0) 
        addPair(id1Now, id2Now);
    }
  }

  // Case with f gamma incoming state.
  else if (fluxType == "fgm") {
    // Fermion from incoming side A.
    if ( isLeptonA ) {
      addBeamA(idA);
      addPair(idA, 22);
    } else {  
      for (int idNow = -nQuarkIn; idNow <= nQuarkIn; ++idNow) 
      if (idNow != 0) {
        addBeamA(idNow);
        addPair(idNow, 22);
      }
    }
    // Fermion from incoming side B.
    if ( isLeptonB ) {
      addBeamB( idB);
      addPair(22, idB);
    } else {  
      for (int idNow = -nQuarkIn; idNow <= nQuarkIn; ++idNow) 
      if (idNow != 0) {
        addBeamB(idNow);
        addPair(22, idNow);
      }
    }
    // Photons in the beams.
    addBeamA(22);
    addBeamB(22);
  }

  // Case with gamma gamma incoming state.
  else if (fluxType == "ggm") {
    addBeamA(21);
    addBeamA(22);
    addBeamB(21);
    addBeamB(22);
    addPair(21, 22);
    addPair(22, 21);
  }

  // Case with gamma gamma incoming state.
  else if (fluxType == "gmgm") {
    addBeamA(22);
    addBeamB(22);
    addPair(22, 22);
  }

  // Unrecognized fluxType is bad sign. Else done.
  else {
    infoPtr->errorMsg("Error in SigmaProcess::initFlux: "
    "unrecognized inFlux type", fluxType);
    return false;
  }
  return true;

}

//*********

// Convolute matrix-element expression(s) with parton flux and K factor.

double SigmaProcess::sigmaPDF() {

  // Evaluate and store the required parton densities.
  for (int j = 0; j < sizeBeamA(); ++j) 
    inBeamA[j].pdf = beamAPtr->xfHard( inBeamA[j].id, x1Save, Q2FacSave); 
  for (int j = 0; j < sizeBeamB(); ++j) 
    inBeamB[j].pdf = beamBPtr->xfHard( inBeamB[j].id, x2Save, Q2FacSave); 

  // Loop over allowed incoming channels.
  sigmaSumSave = 0.;
  for (int i = 0; i < sizePair(); ++i) {
    
    // Evaluate hard-scattering cross section. Include K factor.
    inPair[i].pdfSigma = Kfactor 
                       * sigmaHatWrap(inPair[i].idA, inPair[i].idB);
    
    // Multiply by respective parton densities.
    for (int j = 0; j < sizeBeamA(); ++j) 
    if (inPair[i].idA == inBeamA[j].id) {
      inPair[i].pdfA      = inBeamA[j].pdf;
      inPair[i].pdfSigma *= inBeamA[j].pdf;
      break;
    }
    for (int j = 0; j < sizeBeamB(); ++j) 
    if (inPair[i].idB == inBeamB[j].id) {
      inPair[i].pdfB      = inBeamB[j].pdf;
      inPair[i].pdfSigma *= inBeamB[j].pdf;
      break;
    }

    // Sum for all channels.
    sigmaSumSave += inPair[i].pdfSigma;
  }
 
  // Done.
  return sigmaSumSave;

}

//*********

// Select incoming parton channel and extract parton densities (resolved).

void SigmaProcess::pickInState(int id1in, int id2in) {

  // Multiple interactions: partons already selected.
  if (id1in != 0 && id2in != 0) {
    id1 = id1in;
    id2 = id2in;
  }

  // Pick channel. Extract channel flavours and pdf's.
  double sigmaRand =  sigmaSumSave * Rndm::flat();
  for (int i = 0; i < sizePair(); ++i) {
    sigmaRand -= inPair[i].pdfSigma;
    if (sigmaRand <= 0.) {
      id1      = inPair[i].idA;
      id2      = inPair[i].idB;
      pdf1Save = inPair[i].pdfA; 
      pdf2Save = inPair[i].pdfB; 
      break;
    }
  }

}

//*********

// Evaluate weight for W decay distribution in t -> W b -> f fbar b.

double SigmaProcess::weightTopDecay( Event& process, int iResBeg,
  int iResEnd) {

  // If not pair W d/s/b and mother t then return unit weight.
  if (iResEnd - iResBeg != 1) return 1.;
  int iW1  = iResBeg;
  int iB2  = iResBeg + 1;
  int idW1 = process[iW1].idAbs();
  int idB2 = process[iB2].idAbs();
  if (idW1 != 24) {
    swap(iW1, iB2); 
    swap(idW1, idB2);
  } 
  if (idW1 != 24 || (idB2 != 1 && idB2 != 3 && idB2 != 5)) return 1.;
  int iT   = process[iW1].mother1(); 
  if (iT <= 0 || process[iT].idAbs() != 6) return 1.;

  // Find sign-matched order of W decay products. 
  int iF    = process[iW1].daughter1(); 
  int iFbar = process[iW1].daughter2();
  if (iFbar - iF != 1) return 1.; 
  if (process[iT].id() * process[iF].id() < 0) swap(iF, iFbar);

  // Weight and maximum weight.
  double wt    = (process[iT].p() * process[iFbar].p()) 
               * (process[iF].p() * process[iB2].p());
  double wtMax = ( pow4(process[iT].m()) - pow4(process[iW1].m()) ) / 8.;  

  // Done.
  return wt / wtMax;

}


//*********

// Evaluate weight for Z0/W+- decay distributions in H -> Z0/W+ Z0/W- -> 4f.

double SigmaProcess::weightHiggsDecay( Event& process, int iResBeg, 
  int iResEnd) {

  // If not pair Z0 Z0 or W+ W- then return unit weight.
  if (iResEnd - iResBeg != 1) return 1.;
  int iZW1  = iResBeg;
  int iZW2  = iResBeg + 1;
  int idZW1 = process[iZW1].id();
  int idZW2 = process[iZW2].id();
  if (idZW1 < 0) {
    swap(iZW1, iZW2); 
    swap(idZW1, idZW2);
  } 
  if ( (idZW1 != 23 || idZW2 != 23) && (idZW1 != 24 || idZW2 != -24) )
    return 1.;

  // If mother is not Higgs then return unit weight.
  int iH  = process[iZW1].mother1(); 
  if (iH <= 0) return 1.;
  int idH = process[iH].id();
  if (idH != 25 && idH != 35 && idH !=36) return 1.;

  // Parameters depend on Higgs type: H0(H_1), H^0(H_2) or A^0(H_3).
  int    higgsParity = higgsH1parity; 
  double higgsEta    = higgsH1eta;
  if (idH == 35) {
    higgsParity      = higgsH2parity;
    higgsEta         = higgsH2eta;
  } else if (idH == 36) {
    higgsParity      = higgsA3parity;
    higgsEta         = higgsA3eta;
  }

  // Option with isotropic decays.
  if (higgsParity == 0) return 1.;

  // Maximum and initial weight. 
  double wtMax = pow4(process[iH].m());
  double wt    = wtMax; 

  // Find sign-matched order of Z0/W+- decay products. 
  int i3 = process[iZW1].daughter1();
  int i4 = process[iZW1].daughter2();
  if (process[i3].id() < 0) swap( i3, i4); 
  int i5 = process[iZW2].daughter1();
  int i6 = process[iZW2].daughter2();
  if (process[i5].id() < 0) swap( i5, i6); 

  // Evaluate four-vector products and find masses..
  double p35  = 2. * process[i3].p() * process[i5].p(); 
  double p36  = 2. * process[i3].p() * process[i6].p(); 
  double p45  = 2. * process[i4].p() * process[i5].p(); 
  double p46  = 2. * process[i4].p() * process[i6].p(); 
  double p34  = 2. * process[i3].p() * process[i4].p(); 
  double p56  = 2. * process[i5].p() * process[i6].p(); 
  double mZW1 = process[iZW1].m();
  double mZW2 = process[iZW2].m();

  // For mixed CP states need epsilon product and gauge boson masses.
  double epsilonProd = 0.;
  if (higgsParity == 3) {
    double p[4][4];
    for (int i = 0; i < 4; ++i) {
      int         ii = i3;
      if (i == 1) ii = i4;
      if (i == 2) ii = i5;
      if (i == 3) ii = i6;
      p[i][0] = process[ii].e();
      p[i][1] = process[ii].px();
      p[i][2] = process[ii].py();
      p[i][3] = process[ii].pz();
    }     
    epsilonProd 
      = p[0][0]*p[1][1]*p[2][2]*p[3][3] - p[0][0]*p[1][1]*p[2][3]*p[3][2] 
      - p[0][0]*p[1][2]*p[2][1]*p[3][3] + p[0][0]*p[1][2]*p[2][3]*p[3][1]
      + p[0][0]*p[1][3]*p[2][1]*p[3][2] - p[0][0]*p[1][3]*p[2][2]*p[3][1]
      - p[0][1]*p[1][0]*p[2][2]*p[3][3] + p[0][1]*p[1][0]*p[2][3]*p[3][2]
      + p[0][1]*p[1][2]*p[2][0]*p[3][3] - p[0][1]*p[1][2]*p[2][3]*p[3][0]
      - p[0][1]*p[1][3]*p[2][0]*p[3][2] + p[0][1]*p[1][3]*p[2][2]*p[3][0]
      + p[0][2]*p[1][0]*p[2][1]*p[3][3] - p[0][2]*p[1][0]*p[2][3]*p[3][1]
      - p[0][2]*p[1][1]*p[2][0]*p[3][3] + p[0][2]*p[1][1]*p[2][3]*p[3][0] 
      + p[0][2]*p[1][3]*p[2][0]*p[3][1] - p[0][2]*p[1][3]*p[2][1]*p[3][0]
      - p[0][3]*p[1][0]*p[2][1]*p[3][2] + p[0][3]*p[1][0]*p[2][2]*p[3][1] 
      + p[0][3]*p[1][1]*p[2][0]*p[3][2] - p[0][3]*p[1][1]*p[2][2]*p[3][0] 
      - p[0][3]*p[1][2]*p[2][0]*p[3][1] + p[0][3]*p[1][2]*p[2][1]*p[3][0];
  }

  // Z0 Z0 decay: vector and axial couplings of two fermion pairs.
  if (idZW1 == 23) {
    double vf1 = CoupEW::vf(process[i3].idAbs());
    double af1 = CoupEW::af(process[i3].idAbs());
    double vf2 = CoupEW::vf(process[i5].idAbs());
    double af2 = CoupEW::af(process[i5].idAbs());
    double va12asym = 4. * vf1 * af1 * vf2 * af2 
      / ( (vf1*vf1 + af1*af1) * (vf2*vf2 + af2*af2) );
    double etaMod = higgsEta / pow2( ParticleDataTable::m0(23) );
    
    // Normal CP-even decay.
    if (higgsParity == 1) wt = 8. * (1. + va12asym) * p35 * p46 
      + 8. * (1. - va12asym) * p36 * p45;

    // CP-odd decay (normal for A0(H_3)).
    else if (higgsParity == 2) wt = ( pow2(p35 + p46) 
      + pow2(p36 + p45) - 2. * p34 * p56
      - 2. * pow2(p35 * p46 - p36 * p45) / (p34 * p56) 
      + va12asym * (p35 + p36 - p45 - p46) * (p35 + p45 - p36 - p46) )
      / (1. +  va12asym);

    // Mixed CP states. 
    else wt = 32. * ( 0.25 * ( (1. + va12asym) * p35 * p46 
      + (1. - va12asym) * p36 * p45 ) - 0.5 * etaMod * epsilonProd
      * ( (1. + va12asym) * (p35 + p46) - (1. - va12asym) * (p36 + p45) )
      + 0.0625 * etaMod * etaMod * (-2. * pow2(p34 * p56) 
      - 2. * pow2(p35 * p46 - p36 * p45) 
      + p34 * p56 * (pow2(p35 + p46) + pow2(p36 + p45)) 
      + va12asym * p34 * p56 * (p35 + p36 - p45 - p46) 
      * (p35 + p45 - p36 - p46) ) ) / ( 1. + 2. * etaMod * mZW1 * mZW2 
      + 2. * pow2(etaMod * mZW1 * mZW2) * (1. + va12asym) );

  // W+ W- decay.
  } else if (idZW1 == 24) {
    double etaMod = higgsEta / pow2( ParticleDataTable::m0(24) );
    
    // Normal CP-even decay.
    if (higgsParity == 1) wt = 16. * p35 * p46; 

    // CP-odd decay (normal for A0(H_3)).
    else if (higgsParity == 2) wt = 0.5 * ( pow2(p35 + p46) 
      + pow2(p36 + p45) - 2. * p34 * p56  
      - 2. * pow2(p35 * p46 - p36 * p45) / (p34 * p56) 
      + (p35 + p36 - p45 - p46) * (p35 + p45 - p36 - p46) );

    // Mixed CP states. 
    else wt = 32. * ( 0.25 * 2. * p35 * p46 
      - 0.5 * etaMod * epsilonProd * 2. * (p35 + p46)
      + 0.0625 * etaMod * etaMod * (-2. * pow2(p34 * p56) 
      - 2. * pow2(p35 * p46 - p36 * p45) 
      + p34 * p56 * (pow2(p35 + p46) + pow2(p36 + p45)) 
      + p34 * p56 * (p35 + p36 - p45 - p46) * (p35 + p45 - p36 - p46) ) ) 
      / ( 1. * 2. * etaMod * mZW1 * mZW2 + 2. * pow2(etaMod * mZW1 * mZW2) );
  }

  // Done.
  return wt / wtMax;

}

//**************************************************************************

// The Sigma1Process class.
// Base class for resolved 2 -> 1 cross sections; derived from SigmaProcess.

//*********

// Input and complement kinematics for resolved 2 -> 1 process. 

void Sigma1Process::store1Kin( double x1in, double x2in, double sHin) {

  // Default value only sensible for these processes.
  swapTU = false;

  // Incoming parton momentum fractions and sHat.
  x1Save = x1in;
  x2Save = x2in;
  sH     = sHin;
  mH     = sqrt(sH);
  sH2    = sH * sH;

  // Different options for renormalization scale, but normally sHat.
  Q2RenSave = renormMultFac * sH;
  if (renormScale1 == 2) Q2RenSave = renormFixScale; 

  // Different options for factorization scale, but normally sHat.
  Q2FacSave = factorMultFac * sH;
  if (factorScale1 == 2) Q2RenSave = factorFixScale; 

  // Evaluate alpha_strong and alpha_EM.
  alpS   = alphaSPtr->alphaS(Q2RenSave);  
  alpEM  = alphaEMPtr->alphaEM(Q2RenSave);  

}

//**************************************************************************

// The Sigma2Process class.
// Base class for resolved 2 -> 2 cross sections; derived from SigmaProcess.

//*********

// Input and complement kinematics for resolved 2 -> 2 process. 

void Sigma2Process::store2Kin( double x1in, double x2in, double sHin, 
  double tHin, double m3in, double m4in, double runBW3in, double runBW4in) {

  // Default ordering of particles 3 and 4.
  swapTU   = false;

  // Incoming parton momentum fractions.
  x1Save   = x1in;
  x2Save   = x2in;

  // Incoming masses and their squares.
  bool masslessKin = (id3Mass() == 0) && (id4Mass() == 0);
  if (masslessKin) {
    m3     = 0.;
    m4     = 0.;
  } else {
    m3     = m3in;
    m4     = m4in;
  }
  mSave[3] = m3;
  mSave[4] = m4;
  s3       = m3 * m3;
  s4       = m4 * m4;

  // Standard Mandelstam variables and their squares.
  sH       = sHin;
  tH       = tHin;
  uH       = (masslessKin) ? -(sH + tH) : s3 + s4 - (sH + tH); 
  mH       = sqrt(sH);
  sH2      = sH * sH;
  tH2      = tH * tH;
  uH2      = uH * uH;

  // The nominal Breit-Wigner factors with running width.
  runBW3   = runBW3in;
  runBW4   = runBW4in; 

  // Calculate squared transverse momentum.
  pT2 = (masslessKin) ?  tH * uH / sH : (tH * uH - s3 * s4) / sH;

  // Special case: pick scale as if 2 -> 1 process in disguise.
  if (isSChannel()) {

    // Different options for renormalization scale, but normally sHat.
    Q2RenSave = renormMultFac * sH;
    if (renormScale1 == 2) Q2RenSave = renormFixScale; 

    // Different options for factorization scale, but normally sHat.
    Q2FacSave = factorMultFac * sH;
    if (factorScale1 == 2) Q2RenSave = factorFixScale; 

  // Normal case with "true" 2 -> 2.  
  } else { 

    // Different options for renormalization scale.
    if (masslessKin)            Q2RenSave = (renormScale2 < 4) ? pT2 : sH;
    else if (renormScale2 == 1) Q2RenSave = pT2 + min(s3, s4);
    else if (renormScale2 == 2) Q2RenSave = sqrt((pT2 + s3) * (pT2 + s4));
    else if (renormScale2 == 3) Q2RenSave = pT2 + 0.5 * (s3 + s4);
    else                        Q2RenSave = sH;
    Q2RenSave *= renormMultFac;
    if      (renormScale2 == 5) Q2RenSave = renormFixScale; 

    // Different options for factorization scale.
    if (masslessKin)            Q2FacSave = (factorScale2 < 4) ? pT2 : sH;
    else if (factorScale2 == 1) Q2FacSave = pT2 + min(s3, s4);
    else if (factorScale2 == 2) Q2FacSave = sqrt((pT2 + s3) * (pT2 + s4));
    else if (factorScale2 == 3) Q2FacSave = pT2 + 0.5 * (s3 + s4);
    else                        Q2FacSave = sH;
    Q2FacSave *= factorMultFac;
    if      (factorScale2 == 5) Q2FacSave = factorFixScale; 
  }

  // Evaluate alpha_strong and alpha_EM.
  alpS = alphaSPtr->alphaS(Q2RenSave);  
  alpEM = alphaEMPtr->alphaEM(Q2RenSave);  

}

//*********

// As above, special kinematics for multiple interactions. 

void Sigma2Process::store2KinMI( double x1in, double x2in,
  double sHin, double tHin, double uHin, double alpSin, double alpEMin,
  bool needMasses, double m3in, double m4in) {

  // Default ordering of particles 3 and 4.
  swapTU    = false;
 
  // Incoming x values.
  x1Save    = x1in;
  x2Save    = x2in;

  // Standard Mandelstam variables and their squares.
  sH        = sHin;
  tH        = tHin;
  uH        = uHin; 
  mH        = sqrt(sH);
  sH2       = sH * sH;
  tH2       = tH * tH;
  uH2       = uH * uH;

  // Strong and electroweak couplings.
  alpS      = alpSin;
  alpEM     = alpEMin;

  // Assume vanishing masses. (Will be modified in final kinematics.) 
  m3        = 0.;
  s3        = 0.;
  m4        = 0.;
  s4        = 0.;
  sHBeta    = sH; 

  // Scattering angle.
  cosTheta  = (tH - uH) / sH;
  sinTheta  = 2. * sqrtpos( tH * uH ) / sH;

  // In some cases must use masses and redefine meaning of tHat and uHat.
  if (needMasses) { 
    m3      = m3in;
    s3      = m3 * m3;
    m4      = m4in;
    s4      = m4 * m4;
    sHMass  = sH - s3 - s4;
    sHBeta  = sqrtpos(sHMass*sHMass - 4. * s3 * s4);   
    tH      = -0.5 * (sHMass - sHBeta * cosTheta); 
    uH      = -0.5 * (sHMass + sHBeta * cosTheta); 
    tH2     = tH * tH;
    uH2     = uH * uH;
  }

  // pT2 with masses (at this stage) included.
  pT2Mass   = 0.25 * sHBeta * pow2(sinTheta);

}

//*********

// Perform kinematics for a Multiple Interaction.

bool Sigma2Process::final2KinMI() {

  // Have to set flavours and colours.
  setIdColAcol();

  // Check that masses of outgoing particles not too big.
  m3           = ParticleDataTable::m0(idSave[3]);
  m4           = ParticleDataTable::m0(idSave[4]);
  mH           = sqrt(sH);
  if (m3 + m4 + MASSMARGIN > mH) return false;
  s3           = m3 * m3;
  s4           = m4 * m4;

  // Do kinematics of the decay.
  double eIn   = 0.5 * mH;
  double e3    = 0.5 * (sH + s3 - s4) / mH;
  double e4    = 0.5 * (sH + s4 - s3) / mH;
  double pAbs  = sqrtpos( e3*e3 - s3 );
  phi          = 2. * M_PI * Rndm::flat();
  double pZ    = pAbs * cosTheta;
  double pX    = pAbs * sinTheta * sin(phi);
  double pY    = pAbs * sinTheta * cos(phi);
  double scale = eIn * sinTheta;

  // Fill particle info.
  parton[1] = Particle( idSave[1], -31, 0, 0, 3, 4, colSave[1], acolSave[1],
    0., 0., eIn, eIn, 0., scale);
  parton[2] = Particle( idSave[2], -31, 0, 0, 3, 4, colSave[2], acolSave[2],
    0., 0., -eIn, eIn, 0., scale);
  parton[3] = Particle( idSave[3],  33, 1, 2, 0, 0, colSave[3], acolSave[3],
    pX, pY, pZ, e3, m3, scale);
  parton[4] = Particle( idSave[4],  33, 1, 2, 0, 0, colSave[4], acolSave[4],
    -pX, -pY, -pZ, e4, m4, scale);

  // Boost particles from subprocess rest frame to event rest frame.
  double betaZ = (x1Save - x2Save) / (x1Save + x2Save);
  for (int i = 1; i <= 4; ++i) parton[i].bst(0., 0., betaZ);

  // Done.
  return true;

}  

//**************************************************************************

// The Sigma3Process class.
// Base class for resolved 2 -> 3 cross sections; derived from SigmaProcess.

//*********

// Input and complement kinematics for resolved 2 -> 3 process. 

void Sigma3Process::store3Kin( double x1in, double x2in, double sHin, 
  Vec4 p3cmIn, Vec4 p4cmIn, Vec4 p5cmIn, double m3in, double m4in, 
  double m5in, double runBW3in, double runBW4in, double runBW5in) {

  // Default ordering of particles 3 and 4 - not relevant here.
  swapTU   = false;

  // Incoming parton momentum fractions.
  x1Save   = x1in;
  x2Save   = x2in;

  // Incoming masses and their squares.
  if (id3Mass() == 0 && id4Mass() == 0 && id5Mass() == 0) {
    m3     = 0.;
    m4     = 0.;
    m5     = 0.;
  } else {
    m3     = m3in;
    m4     = m4in;
    m5     = m5in;
  }
  mSave[3] = m3;
  mSave[4] = m4;
  mSave[5] = m5;
  s3       = m3 * m3;
  s4       = m4 * m4;
  s5       = m5 * m5;

  // Standard Mandelstam variables and four-momenta in rest frame.
  sH       = sHin;
  mH       = sqrt(sH);
  p3cm     = p3cmIn;
  p4cm     = p4cmIn;
  p5cm     = p5cmIn;

  // The nominal Breit-Wigner factors with running width.
  runBW3   = runBW3in;
  runBW4   = runBW4in; 
  runBW5   = runBW5in; 

  // Special case: pick scale as if 2 -> 1 process in disguise.
  if (isSChannel()) {

    // Different options for renormalization scale, but normally sHat.
    Q2RenSave = renormMultFac * sH;
    if (renormScale1 == 2) Q2RenSave = renormFixScale; 

    // Different options for factorization scale, but normally sHat.
    Q2FacSave = factorMultFac * sH;
    if (factorScale1 == 2) Q2RenSave = factorFixScale; 

  // "Normal" 2 -> 3 processes, i.e. not vector boson fusion.
  } else if ( idTchan1() != 23 && idTchan1() != 24 && idTchan2() != 23 
    && idTchan1() != 24 ) {
    double mT3S = s3 + p3cm.pT2();
    double mT4S = s4 + p4cm.pT2();
    double mT5S = s5 + p5cm.pT2();
    
    // Different options for renormalization scale.
    if      (renormScale3 == 1) Q2RenSave = min( mT3S, min(mT4S, mT5S) ); 
    else if (renormScale3 == 2) Q2RenSave = sqrt( mT3S * mT4S * mT5S
      / max( mT3S, max(mT4S, mT5S) ) );
    else if (renormScale3 == 3) Q2RenSave = pow( mT3S * mT4S * mT5S, 
                                            1./3. );
    else if (renormScale3 == 4) Q2RenSave = (mT3S * mT4S * mT5S) / 3.;
    else                        Q2RenSave = sH;
    Q2RenSave *= renormMultFac;
    if      (renormScale3 == 6) Q2RenSave = renormFixScale; 
    
    // Different options for factorization scale.
    if      (factorScale3 == 1) Q2FacSave = min( mT3S, min(mT4S, mT5S) ); 
    else if (factorScale3 == 2) Q2FacSave = sqrt( mT3S * mT4S * mT5S
      / max( mT3S, max(mT4S, mT5S) ) );
    else if (factorScale3 == 3) Q2FacSave = pow( mT3S * mT4S * mT5S, 
                                            1./3. );
    else if (factorScale3 == 4) Q2FacSave = (mT3S * mT4S * mT5S) / 3.;
    else                        Q2FacSave = sH;
    Q2RenSave *= factorMultFac;
    if      (factorScale3 == 6) Q2FacSave = factorFixScale; 

  // Vector boson fusion 2 -> 3 processes; recoils in positions 4 and 5.
  } else {
    double sV4   = pow2( ParticleDataTable::m0(idTchan1()) ); 
    double sV5   = pow2( ParticleDataTable::m0(idTchan2()) ); 
    double mT3S  = s3  + p3cm.pT2();
    double mTV4S = sV4 + p4cm.pT2();
    double mTV5S = sV5 + p5cm.pT2();

    // Different options for renormalization scale.
    if      (renormScale3VV == 1) Q2RenSave = max( sV4, sV5); 
    else if (renormScale3VV == 2) Q2RenSave = sqrt( mTV4S * mTV5S );
    else if (renormScale3VV == 3) Q2RenSave = pow( mT3S * mTV4S * mTV5S, 
                                              1./3. );
    else if (renormScale3VV == 4) Q2RenSave = (mT3S * mTV4S * mTV5S) / 3.;
    else                          Q2RenSave = sH;
    Q2RenSave *= renormMultFac;
    if      (renormScale3VV == 6) Q2RenSave = renormFixScale; 
    
    // Different options for factorization scale.
    if      (factorScale3VV == 1) Q2FacSave = max( sV4, sV5); 
    else if (factorScale3VV == 2) Q2FacSave = sqrt( mTV4S * mTV5S );
    else if (factorScale3VV == 3) Q2FacSave = pow( mT3S * mTV4S * mTV5S, 
                                              1./3. );
    else if (factorScale3VV == 4) Q2FacSave = (mT3S * mTV4S * mTV5S) / 3.;
    else                          Q2FacSave = sH;
    Q2RenSave *= factorMultFac;
    if      (factorScale3VV == 6) Q2FacSave = factorFixScale; 
  }

  // Evaluate alpha_strong and alpha_EM.
  alpS = alphaSPtr->alphaS(Q2RenSave);  
  alpEM = alphaEMPtr->alphaEM(Q2RenSave);  

}

//**************************************************************************

// The SigmaLHAProcess class.
// Wrapper for Les Houches Accord external input; derived from SigmaProcess.
// Note: arbitrary subdivision into PhaseSpaceLHA and SigmaLHAProcess tasks.

//*********

// Obtain number of final-state partons from LHA object.

int SigmaLHAProcess::nFinal() const {

  // At initialization size unknown, so return 0.
  if (lhaUpPtr->sizePart() <= 0) return 0;

  // Sum up all particles that has first mother = 1.
  int nFin = 0; 
  for (int i = 3; i < lhaUpPtr->sizePart(); ++i) 
    if (lhaUpPtr->mother1(i) == 1) ++nFin;
  return nFin;

}

//**************************************************************************

} // end namespace Pythia8

// SigmaDM.cc is a part of the PYTHIA event generator.
// Copyright (C) 2018 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL v2 or later, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Function definitions (not found in the header) for the
// Dark Matter simulation classes.

#include "Pythia8/SigmaDM.h"

namespace Pythia8 {

//==========================================================================

// Sigma2ffbar2Zp2XX.
// Cross section for f fbar' -> Zprime -> XX. (Zprime a.k.a. DMmed(s=1).)

//--------------------------------------------------------------------------

// Initialize process.

void Sigma1ffbar2Zp2XX::initProc() {

  // Store mass and width for propagator.
  mRes      = particleDataPtr->m0(55);
  GammaRes  = particleDataPtr->mWidth(55);
  m2Res     = mRes*mRes;

  // Set pointer to particle properties and decay table.
  particlePtr = particleDataPtr->particleDataEntryPtr(55);

  double mf, mr, psvec, psaxi, betaf;
  preFac = 0.0;

  // Loop over all Zp decay channels.
  for (int i = 0; i < particlePtr->sizeChannels(); ++i) {
    int idAbs = abs( particlePtr->channel(i).product(0));
    if (idAbs != 52) {
      // Set onMode to off
      particlePtr->channel(i).onMode(0);
      continue;
    }

    mf = particleDataPtr->m0(idAbs);

    // Check that above threshold. Phase space.
    if (mRes > 2. * mf + MASSMARGIN) {
      mr    = pow2(mf / mRes);
      betaf = sqrtpos(1. - 4. * mr);
      psvec = betaf * (1. + 2. * mr);
      psaxi = pow3(betaf);
      double vf = settingsPtr->parm("Zp:vX");
      double af = settingsPtr->parm("Zp:aX");
      preFac += (vf * vf * psvec + af * af * psaxi ) ;
    }
  }

}

//--------------------------------------------------------------------------

// Evaluate sigmaHat(sHat), part independent of incoming flavour.

void Sigma1ffbar2Zp2XX::sigmaKin() {

  sigma0 = 1. / ( pow2(sH - m2Res) + pow2(mRes * GammaRes) );

}

//--------------------------------------------------------------------------

// Evaluate sigmaHat(sHat), including incoming flavour dependence.

double Sigma1ffbar2Zp2XX::sigmaHat() {

  // Check for allowed flavour combinations
  if (id1 + id2 != 0 || abs(id1) > 6 ) return 0.;

  // double trace  = 8.0 * pow2(s3 - tH) + pow2(s3 - uH) + 2.0 * s3 * sH ;
  // double sigma = sigma0 * trace;

  // // Colour factors.
  // if (abs(id1) < 7) sigma /= 3.;

  double vf, af;

  if (abs(id1) % 2 == 0) {
    vf = settingsPtr->parm("Zp:vu");
    af = settingsPtr->parm("Zp:au");
  }
  else {
    vf = settingsPtr->parm("Zp:vd");
    af = settingsPtr->parm("Zp:ad");
  }

  double vf2af2 = vf * vf + af * af;
  double sigma = preFac * sigma0 * vf2af2;

  // Answer.
  return sigma;

}

//--------------------------------------------------------------------------

// Select identity, colour and anticolour.

void Sigma1ffbar2Zp2XX::setIdColAcol() {

  setId(id1, id2, 55);
  // Colour flow topologies. Swap when antiquarks.
  if (abs(id1) < 9) setColAcol( 1, 0, 0, 1, 0, 0);
  else              setColAcol( 0, 0, 0, 0, 0, 0);
  if (id1 < 0) swapColAcol();

}

//==========================================================================

// Sigma2qqbar2Zpg2XXj.
// Cross section for q qbar -> Zprime -> XX + jet.

//--------------------------------------------------------------------------

// Initialize process.

void Sigma2qqbar2Zpg2XXj::initProc() {

  // Store mass and width for propagator.
  mRes      = particleDataPtr->m0(55);
  GammaRes  = particleDataPtr->mWidth(55);
  m2Res     = mRes*mRes;

  // Set pointer to particle properties and decay table.
  particlePtr = particleDataPtr->particleDataEntryPtr(55);

  double mf, mr, psvec, psaxi, betaf;
  preFac = 0.0;

  mf = particleDataPtr->m0(52);
  for (int i = 0; i < particlePtr->sizeChannels(); ++i) {
    int idAbs = abs( particlePtr->channel(i).product(0));
    if (idAbs != 52) {
      // Set onMode to off
      particlePtr->channel(i).onMode(0);
      continue;
    }

    // Check that above threshold. Phase space.
    if (m3 > 2. * mf + MASSMARGIN) {
      mr    = pow2(mf / m3);
      betaf = sqrtpos(1. - 4. * mr);
      psvec = betaf * (1. + 2. * mr);
      psaxi = pow3(betaf);
      double vf = settingsPtr->parm("Zp:vX");
      double af = settingsPtr->parm("Zp:aX");
      preFac += (vf * vf * psvec + af * af * psaxi) ;
    }
  }
}

//--------------------------------------------------------------------------

// Evaluate sigmaHat(sHat), part independent of incoming flavour.

void Sigma2qqbar2Zpg2XXj::sigmaKin() {

  double propZp = s3 / ( pow2(s3 - m2Res) + pow2(mRes * GammaRes) );

  // Cross section part common for all incoming flavours.
  sigma0 = (M_PI / sH2) * (alpEM * alpS) * propZp
    * (2./9.) * (tH2 + uH2 + 2. * sH * s3) / (tH * uH);

}

//--------------------------------------------------------------------------

// Evaluate sigmaHat(sHat), including incoming flavour dependence.

double Sigma2qqbar2Zpg2XXj::sigmaHat() {

  // Check for allowed flavour combinations
  if (id1 + id2 != 0 || abs(id1) > 6 ) return 0.;

  double vf, af;

  if (abs(id1) % 2 == 0) {
    vf = settingsPtr->parm("Zp:vu");
    af = settingsPtr->parm("Zp:au");
  }
  else {
    vf = settingsPtr->parm("Zp:vd");
    af = settingsPtr->parm("Zp:ad");
  }

  double vf2af2 = vf * vf + af * af;
  double sigma = sigma0 * vf2af2; // * preFac;

  // Answer.
  return sigma;

}

//--------------------------------------------------------------------------

// Select identity, colour and anticolour.

void Sigma2qqbar2Zpg2XXj::setIdColAcol() {

  setId(id1, id2, 55, 21);

  // Colour flow topologies.
  if (id1 > 0) setColAcol( 1, 0, 0, 2, 0, 0, 1, 2);
  else         setColAcol( 0, 2, 1, 0, 0, 0, 1, 2);

}


//==========================================================================

// Sigma2ffbar2Zp2H.
// Cross section for f fbar' -> Zprime H.

//--------------------------------------------------------------------------

// Initialize process.

void Sigma2ffbar2ZpH::initProc() {

  // Store mass and width for propagator.
  mRes      = particleDataPtr->m0(55);
  GammaRes  = particleDataPtr->mWidth(55);
  m2Res     = mRes*mRes;
  coupZpH   = settingsPtr->parm("Zp:coupH");
  gZp   = settingsPtr->parm("Zp:gZp");

  // Set pointer to particle properties and decay table.
  particlePtr = particleDataPtr->particleDataEntryPtr(55);
  // Secondary open width fraction.
  openFrac = particleDataPtr->resOpenFrac(55, 25);

}

//--------------------------------------------------------------------------

// Evaluate sigmaHat(sHat), part independent of incoming flavour.

void Sigma2ffbar2ZpH::sigmaKin() {

  double denom = (pow2(sH - m2Res) + pow2(mRes * GammaRes));

  sigma0 = (M_PI / sH2) * 8. * pow2(gZp * coupZpH)
    * (tH * uH - s3 * s4 + 2. * sH * s4);

  sigma0 /= denom;

}

//--------------------------------------------------------------------------

// Evaluate sigmaHat(sHat), including incoming flavour dependence.

double Sigma2ffbar2ZpH::sigmaHat() {

  // Check for allowed flavour combinations
  if (id1 + id2 != 0 ) return 0.;

  // Coupling a_f^2 + v_f^2 to s-channel Zp and colour factor.
  double vf = 0., af = 0.;

  if (abs(id1) % 2 == 0) {
    vf = settingsPtr->parm("Zp:vu");
    af = settingsPtr->parm("Zp:au");
  }
  else {
    vf = settingsPtr->parm("Zp:vd");
    af = settingsPtr->parm("Zp:ad");
  }

  double sigma = sigma0 * (vf * vf + af * af);
  if (abs(id1) < 9) sigma /= 3.;

  // Secondary width for Zp and H
  sigma       *= openFrac;

  // Answer.
  return sigma;

}

//--------------------------------------------------------------------------

// Select identity, colour and anticolour.

void Sigma2ffbar2ZpH::setIdColAcol() {

  setId(id1, id2, 55, 25);
  // Colour flow topologies. Swap when antiquarks.
  if (abs(id1) < 9) setColAcol( 1, 0, 0, 1, 0, 0, 0, 0);
  else              setColAcol( 0, 0, 0, 0, 0, 0, 0, 0);
  if (id1 < 0) swapColAcol();

}


//==========================================================================

// Sigma2ffbar2S2XX.
// Cross section for f fbar' -> S -> XX.

//--------------------------------------------------------------------------

// Initialize process.

void Sigma1gg2S2XX::initProc() {

  // Store mass and width for propagator.
  mRes      = particleDataPtr->m0(54);
  GammaRes  = particleDataPtr->mWidth(54);
  m2Res     = mRes*mRes;

  // Set pointer to particle properties and decay table.
  particlePtr = particleDataPtr->particleDataEntryPtr(54);

  for (int i = 0; i < particlePtr->sizeChannels(); ++i) {
    int idAbs = abs( particlePtr->channel(i).product(0));
    if (idAbs != 52) {
      // Set onMode to off
      particlePtr->channel(i).onMode(0);
    }
  }

}

//--------------------------------------------------------------------------

// Evaluate sigmaHat(sHat), part independent of incoming flavour.

void Sigma1gg2S2XX::sigmaKin() {

  double propS = sH / ( pow2(sH - m2Res) + pow2(mRes * GammaRes) );
  sigma0        = 8. * M_PI * propS;

}

//--------------------------------------------------------------------------

// Evaluate sigmaHat(sHat), including incoming flavour dependence.

double Sigma1gg2S2XX::sigmaHat() {

  // Check for allowed flavour combinations
  if (id1 != id2 || abs(id1) != 21 ) return 0.;

  double widthIn  = particlePtr->resWidthChan( mRes, 21, 21) / 64.;

  // Width out only includes open channels.
  double widthOut = particlePtr->resWidthChan( mRes, 52, -52);

  // Done.
  double sigma = widthIn * sigma0 * widthOut;

  // Answer.
  return sigma;

}

//--------------------------------------------------------------------------

// Select identity, colour and anticolour.

void Sigma1gg2S2XX::setIdColAcol() {

  setId(id1, id2, 54);
  setColAcol( 1, 2, 2, 1, 0, 0);

}

//==========================================================================

// Sigma2gg2Sg2XXj.
// Cross section for g g -> S g -> XX + jet.

//--------------------------------------------------------------------------

// Initialize process.

void Sigma2gg2Sg2XXj::initProc() {

  // Store mass and width for propagator.
  mRes      = particleDataPtr->m0(54);
  GammaRes  = particleDataPtr->mWidth(54);
  m2Res     = mRes*mRes;

  // Set pointer to particle properties and decay table.
  particlePtr = particleDataPtr->particleDataEntryPtr(54);

  for (int i = 0; i < particlePtr->sizeChannels(); ++i) {
    int idAbs = abs( particlePtr->channel(i).product(0));
    if (idAbs != 52) {
      // Set onMode to off
      particlePtr->channel(i).onMode(0);
    }
  }

}

//--------------------------------------------------------------------------

// Evaluate sigmaHat(sHat), part independent of incoming flavour.

void Sigma2gg2Sg2XXj::sigmaKin() {

  double wid = particlePtr->resWidthChan(m3, 21, 21);

  // Vec4 resonance = p3cm + p4cm;
  // double sHCalc = resonance.m2Calc();
  // double sHCalc2 = sHCalc * sHCalc;

  // Vec4 p1cm = sH * x1Save * Vec4(0., 0., 1., 1.);

  // Vec4 tChan = resonance - p1cm;
  // double tHCalc = tChan.m2Calc();
  // double tHCalc2 = tHCalc * tHCalc;
  // Vec4 uChan = p5cm - p1cm;
  // double uHCalc = uChan.m2Calc();
  // double uHCalc2 = uHCalc * uHCalc;

  // propS = sHCalc / ( pow2(sHCalc - m2Res) + pow2(mRes * GammaRes) );

  // Expression adapted from g g -> H g.

  // sigma0  = (M_PI / sH2) * (3. / 16.) * alpS * (wid / mRes)
  //   * (sH2 * sH2 + tHCalc2 * tHCalc2 + uHCalc2 * uHCalc2 + pow2(sHCalc2))
  //   / (sH * tHCalc * uHCalc * sHCalc);

  sigma0  = (M_PI / sH2) * (3. / 16.) * alpS * (wid / m3)
    * (sH2 * sH2 + tH2 * tH2 + uH2 * uH2 + pow2(sH2))
    / (sH * tH * uH * sH);

}

//--------------------------------------------------------------------------

// Evaluate sigmaHat(sHat), including incoming flavour dependence.

double Sigma2gg2Sg2XXj::sigmaHat() {

  return sigma0 * particlePtr->resWidthChan(mRes, 52, -52);

}

//--------------------------------------------------------------------------

// Select identity, colour and anticolour.

void Sigma2gg2Sg2XXj::setIdColAcol() {

  setId(id1, id2, 54, 21);

  if( rndmPtr->flat() < 0.5)
    setColAcol( 1, 2, 3, 1, 0, 0, 3, 2);
  else
    setColAcol( 1, 2, 2, 3, 0, 0, 1, 3);

}

//==========================================================================

} // end namespace Pythia8

// SigmaExtraDim.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Function definitions (not found in the header) for the 
// extra-dimensional simulation classes. 

#include "SigmaExtraDim.h"

namespace Pythia8 {

//**************************************************************************

// Sigma1gg2GravitonStar class.
// Cross section for g g -> G* (excited graviton state). 

//*********

// Initialize process. 
  
void Sigma1gg2GravitonStar::initProc() {

  // Store G* mass and width for propagator. 
  idGstar  = 5000039;
  mRes     = ParticleDataTable::m0(idGstar);
  GammaRes = ParticleDataTable::mWidth(idGstar);
  m2Res    = mRes*mRes;
  GamMRat  = GammaRes / mRes;

  // Overall coupling strength kappa * m_G*.
  kappaMG  = Settings::parm("ExtraDimensionsG*:kappaMG");

  // Set pointer to particle properties and decay table.
  gStarPtr = ParticleDataTable::particleDataPtr(idGstar);

} 

//*********

// Evaluate sigmaHat(sHat), part independent of incoming flavour. 

void Sigma1gg2GravitonStar::sigmaKin() { 

  // Incoming width for gluons.
  double widthIn  = pow2(kappaMG) * mH / (160. * M_PI);

  // Set up Breit-Wigner. Width out only includes open channels. 
  double sigBW    = 5. * M_PI/ ( pow2(sH - m2Res) + pow2(sH * GamMRat) );    
  double widthOut = gStarPtr->resWidthOpen(idGstar, mH);

  // Modify cross section in wings of peak. Done.
  sigma           = widthIn * sigBW * widthOut * pow2(sH / m2Res);    

}

//*********

// Select identity, colour and anticolour.

void Sigma1gg2GravitonStar::setIdColAcol() {

  // Flavours trivial.
  setId( 21, 21, idGstar);

  // Colour flow topology.
  setColAcol( 1, 2, 2, 1, 0, 0);

}

//*********

// Evaluate weight for G* decay angle.
  
double Sigma1gg2GravitonStar::weightDecay( Event& process, int iResBeg, 
  int iResEnd) {

  // Identity of mother of decaying reseonance(s).
  int idMother = process[process[iResBeg].mother1()].idAbs();

  // For top decay hand over to standard routine.
  if (idMother == 6) 
    return weightTopDecay( process, iResBeg, iResEnd);

  // G* should sit in entry 5.
  if (iResBeg != 5 || iResEnd != 5) return 1.;

  // Phase space factors. Reconstruct decay angle.
  double mr1    = pow2(process[6].m()) / sH;
  double mr2    = pow2(process[7].m()) / sH;
  double betaf  = sqrtpos( pow2(1. - mr1 - mr2) - 4. * mr1 * mr2); 
  double cosThe = (process[3].p() - process[4].p()) 
    * (process[7].p() - process[6].p()) / (sH * betaf);

  // Default is isotropic decay.
  double wt     = 1.;

  // Angular weight for g + g -> G* -> f + fbar.
  if (process[6].idAbs() < 19) wt = 1. - pow4(cosThe);

  // Angular weight for g + g -> G* -> g + g or gamma + gamma.
  else if (process[6].id() == 21 || process[6].id() == 22)
    wt = (1. + 6. * pow2(cosThe) + pow4(cosThe)) / 8.;
 
  // Done.
  return wt;

}

//**************************************************************************

// Sigma1ffbar2GravitonStar class.
// Cross section for f fbar -> G* (excited graviton state). 

//*********

// Initialize process. 
  
void Sigma1ffbar2GravitonStar::initProc() {

  // Store G* mass and width for propagator. 
  idGstar  = 5000039;
  mRes     = ParticleDataTable::m0(idGstar);
  GammaRes = ParticleDataTable::mWidth(idGstar);
  m2Res    = mRes*mRes;
  GamMRat  = GammaRes / mRes;

  // Overall coupling strength kappa * m_G*.
  kappaMG  = Settings::parm("ExtraDimensionsG*:kappaMG");

  // Set pointer to particle properties and decay table.
  gStarPtr = ParticleDataTable::particleDataPtr(idGstar);

} 

//*********

// Evaluate sigmaHat(sHat), part independent of incoming flavour. 

void Sigma1ffbar2GravitonStar::sigmaKin() { 

  // Incoming width for fermions, disregarding colour factor.
  double widthIn  = pow2(kappaMG) * mH / (80. * M_PI);

  // Set up Breit-Wigner. Width out only includes open channels. 
  double sigBW    = 5. * M_PI/ ( pow2(sH - m2Res) + pow2(sH * GamMRat) );    
  double widthOut = gStarPtr->resWidthOpen(idGstar, mH);

  // Modify cross section in wings of peak. Done.
  sigma0          = widthIn * sigBW * widthOut * pow2(sH / m2Res);    

}

//*********

// Select identity, colour and anticolour.

void Sigma1ffbar2GravitonStar::setIdColAcol() {

  // Flavours trivial.
  setId( id1, id2, idGstar);

  // Colour flow topologies. Swap when antiquarks.
  if (abs(id1) < 9) setColAcol( 1, 0, 0, 1, 0, 0);
  else              setColAcol( 0, 0, 0, 0, 0, 0);
  if (id1 < 0) swapColAcol();

}

//*********

// Evaluate weight for G* decay angle.
  
double Sigma1ffbar2GravitonStar::weightDecay( Event& process, int iResBeg, 
  int iResEnd) {

  // Identity of mother of decaying reseonance(s).
  int idMother = process[process[iResBeg].mother1()].idAbs();

  // For top decay hand over to standard routine.
  if (idMother == 6) 
    return weightTopDecay( process, iResBeg, iResEnd);

  // G* should sit in entry 5.
  if (iResBeg != 5 || iResEnd != 5) return 1.;

  // Phase space factors. Reconstruct decay angle.
  double mr1    = pow2(process[6].m()) / sH;
  double mr2    = pow2(process[7].m()) / sH;
  double betaf  = sqrtpos( pow2(1. - mr1 - mr2) - 4. * mr1 * mr2); 
  double cosThe = (process[3].p() - process[4].p()) 
    * (process[7].p() - process[6].p()) / (sH * betaf);

  // Default is isotropic decay.
  double wt     = 1.;

  // Angular weight for f + fbar -> G* -> f + fbar.
  if (process[6].idAbs() < 19)
    wt = (1. - 3. * pow2(cosThe) + 4. * pow4(cosThe)) / 2.;

  // Angular weight for f + fbar -> G* -> g + g or gamma + gamma.
  else if (process[6].id() == 21 || process[6].id() == 22)
    wt = 1. - pow4(cosThe);
 
  // Done.
  return wt;

}

//**************************************************************************

// Sigma2gg2GravitonStarg class.
// Cross section for g g -> G* g (excited graviton state). 

//*********

// Initialize process. 
  
void Sigma2gg2GravitonStarg::initProc() {

  // Store G* mass and width for propagator. 
  idGstar  = 5000039;
  mRes     = ParticleDataTable::m0(idGstar);
  GammaRes = ParticleDataTable::mWidth(idGstar);
  m2Res    = mRes*mRes;
  GamMRat  = GammaRes / mRes;

  // Overall coupling strength kappa * m_G*.
  kappaMG  = Settings::parm("ExtraDimensionsG*:kappaMG");

   // Secondary open width fraction.
  openFrac = ParticleDataTable::resOpenFrac(idGstar);

} 

//*********

// Evaluate sigmaHat(sHat), part independent of incoming flavour. 

void Sigma2gg2GravitonStarg::sigmaKin() { 

  //  Evaluate cross section. Secondary width for G*.
  sigma = (3. * pow2(kappaMG) * alpS) / (32. * sH * s3)
    * ( pow2(tH2 + tH * uH + uH2) / (sH2 * tH * uH) 
    + 2. * (tH2 / uH + uH2 / tH) / sH + 3. * (tH / uH + uH / tH)
    + 2. * (sH / uH + sH/tH) + sH2 / (tH * uH) );
  sigma *= openFrac;

}

//*********

// Select identity, colour and anticolour.

void Sigma2gg2GravitonStarg::setIdColAcol() {

  // Flavours trivial.
  setId( 21, 21, idGstar, 21);

  // Colour flow topologies: random choice between two mirrors.
  if (Rndm::flat() < 0.5) setColAcol( 1, 2, 2, 3, 0, 0, 1, 3);
  else                    setColAcol( 1, 2, 3, 1, 0, 0, 3, 2);

}

//*********

// Evaluate weight for decay angles: currently G* assumed isotropic.
  
double Sigma2gg2GravitonStarg::weightDecay( Event& process, int iResBeg, 
  int iResEnd) {

  // Identity of mother of decaying reseonance(s).
  int idMother = process[process[iResBeg].mother1()].idAbs();

  // For top decay hand over to standard routine.
  if (idMother == 6) 
    return weightTopDecay( process, iResBeg, iResEnd);

  // No equations for G* decay so assume isotropic.
  return 1.;

}

//**************************************************************************

// Sigma2qg2GravitonStarq class.
// Cross section for q g -> G* q (excited graviton state). 

//*********

// Initialize process. 
  
void Sigma2qg2GravitonStarq::initProc() {

  // Store G* mass and width for propagator. 
  idGstar  = 5000039;
  mRes     = ParticleDataTable::m0(idGstar);
  GammaRes = ParticleDataTable::mWidth(idGstar);
  m2Res    = mRes*mRes;
  GamMRat  = GammaRes / mRes;

  // Overall coupling strength kappa * m_G*.
  kappaMG  = Settings::parm("ExtraDimensionsG*:kappaMG");

   // Secondary open width fraction.
  openFrac = ParticleDataTable::resOpenFrac(idGstar);

} 

//*********

// Evaluate sigmaHat(sHat), part independent of incoming flavour. 

void Sigma2qg2GravitonStarq::sigmaKin() { 

  //  Evaluate cross section. Secondary width for G*.
  sigma = -(pow2(kappaMG) * alpS) / (192. * sH * s3)
    * ( 4. * (sH2 + uH2) / (tH * sH) + 9. * (sH + uH) / sH + sH / uH
    + uH2 / sH2 + 3. * tH * (4. + sH / uH + uH / sH) / sH
    + 4. * tH2 * (1. / uH + 1. / sH) / sH + 2. * tH2 * tH / (uH * sH2) );
  sigma *= openFrac;

}

//*********

// Select identity, colour and anticolour.

void Sigma2qg2GravitonStarq::setIdColAcol() {

  // Flavour set up for q g -> H q.
  int idq = (id2 == 21) ? id1 : id2;
  setId( id1, id2, idGstar, idq);

  // tH defined between f and f': must swap tHat <-> uHat if q g in.
  swapTU = (id2 == 21); 

  // Colour flow topologies. Swap when antiquarks.
  if (id2 == 21) setColAcol( 1, 0, 2, 1, 0, 0, 2, 0);
  else           setColAcol( 2, 1, 1, 0, 0, 0, 2, 0);
  if (idq < 0) swapColAcol();

}

//*********

// Evaluate weight for decay angles: currently G* assumed isotropic.
  
double Sigma2qg2GravitonStarq::weightDecay( Event& process, int iResBeg, 
  int iResEnd) {

  // Identity of mother of decaying reseonance(s).
  int idMother = process[process[iResBeg].mother1()].idAbs();

  // For top decay hand over to standard routine.
  if (idMother == 6) 
    return weightTopDecay( process, iResBeg, iResEnd);

  // No equations for G* decay so assume isotropic.
  return 1.;

}

//**************************************************************************

// Sigma2qqbar2GravitonStarg class.
// Cross section for q qbar -> G* g (excited graviton state). 

//*********

// Initialize process. 
  
void Sigma2qqbar2GravitonStarg::initProc() {

  // Store G* mass and width for propagator. 
  idGstar  = 5000039;
  mRes     = ParticleDataTable::m0(idGstar);
  GammaRes = ParticleDataTable::mWidth(idGstar);
  m2Res    = mRes*mRes;
  GamMRat  = GammaRes / mRes;

  // Overall coupling strength kappa * m_G*.
  kappaMG  = Settings::parm("ExtraDimensionsG*:kappaMG");

   // Secondary open width fraction.
  openFrac = ParticleDataTable::resOpenFrac(idGstar);

} 

//*********

// Evaluate sigmaHat(sHat), part independent of incoming flavour. 

void Sigma2qqbar2GravitonStarg::sigmaKin() { 

  //  Evaluate cross section. Secondary width for G*.
  sigma = (pow2(kappaMG) * alpS) / (72. * sH * s3)
    * ( 4. * (tH2 + uH2) / sH2 + 9. * (tH + uH) / sH 
    + (tH2 / uH + uH2 / tH) / sH + 3. * (4. + tH / uH + uH/ tH)
    + 4. * (sH / uH + sH / tH) + 2. * sH2 / (tH * uH) );
  sigma *= openFrac;

}

//*********

// Select identity, colour and anticolour.

void Sigma2qqbar2GravitonStarg::setIdColAcol() {

  // Flavours trivial.
  setId( id1, id2, idGstar, 21);

  // Colour flow topologies. Swap when antiquarks.
  setColAcol( 1, 0, 0, 2, 0, 0, 1, 2);
  if (id1 < 0) swapColAcol();

}

//*********

// Evaluate weight for decay angles: currently G* assumed isotropic.
  
double Sigma2qqbar2GravitonStarg::weightDecay( Event& process, int iResBeg, 
  int iResEnd) {

  // Identity of mother of decaying reseonance(s).
  int idMother = process[process[iResBeg].mother1()].idAbs();

  // For top decay hand over to standard routine.
  if (idMother == 6) 
    return weightTopDecay( process, iResBeg, iResEnd);

  // No equations for G* decay so assume isotropic.
  return 1.;

}


//**************************************************************************

} // end namespace Pythia8

// SigmaQCD.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Function definitions (not found in the header) for the 
// QCD simulation classes. 

#include "SigmaQCD.h"

namespace Pythia8 {

//**************************************************************************

// Sigma0AB2AB class.
// Cross section for elastic scattering A B -> A B.

//*********

// Select identity, colour and anticolour.

void Sigma0AB2AB::setIdColAcol() {

  // Flavours and colours are trivial. 
  setId( idA, idB, idA, idB);
  setColAcol( 0, 0, 0, 0, 0, 0, 0, 0);
}

//**************************************************************************

// Sigma0AB2XB class.
// Cross section for single diffractive scattering A B -> X B.

//*********

// Select identity, colour and anticolour.

void Sigma0AB2XB::setIdColAcol() {

  // Flavours and colours are trivial. 
  int idX          = 10* (abs(idA) / 10) + 9900000; 
  if (idA < 0) idX = -idX;
  setId( idA, idB, idX, idB);
  setColAcol( 0, 0, 0, 0, 0, 0, 0, 0);

}

//**************************************************************************

// Sigma0AB2AX class.
// Cross section for single diffractive scattering A B -> A X.

//*********

// Select identity, colour and anticolour.

void Sigma0AB2AX::setIdColAcol() {

  // Flavours and colours are trivial. 
  int idX          = 10* (abs(idB) / 10) + 9900000; 
  if (idB < 0) idX = -idX;
  setId( idA, idB, idA, idX);
  setColAcol( 0, 0, 0, 0, 0, 0, 0, 0);

}

//**************************************************************************

// Sigma0AB2XX class.
// Cross section for double diffractive scattering A B -> X X.

//*********

// Select identity, colour and anticolour.

void Sigma0AB2XX::setIdColAcol() {

  // Flavours and colours are trivial. 
  int          idX1 = 10* (abs(idA) / 10) + 9900000; 
  if (idA < 0) idX1 = -idX1;
  int          idX2 = 10* (abs(idB) / 10) + 9900000; 
  if (idB < 0) idX2 = -idX2;
  setId( idA, idB, idX1, idX2);
  setColAcol( 0, 0, 0, 0, 0, 0, 0, 0);

}

//**************************************************************************

// Sigma2gg2gg class.
// Cross section for g g -> g g.

//*********

// Evaluate d(sigmaHat)/d(tHat) - no incoming flavour dependence.

void Sigma2gg2gg::sigmaKin() {

  // Calculate kinematics dependence.
  sigTS  = (9./4.) * (tH2 / sH2 + 2. * tH / sH + 3. + 2. * sH / tH 
           + sH2 / tH2);
  sigUS  = (9./4.) * (uH2 / sH2 + 2. * uH / sH + 3. + 2. * sH / uH 
           + sH2 / uH2);
  sigTU  = (9./4.) * (tH2 / uH2 + 2. * tH / uH + 3. + 2. * uH / tH 
           + uH2 / tH2);
  sigSum = sigTS + sigUS + sigTU;

  // Answer contains factor 1/2 from identical gluons.
  sigma  = (M_PI / sH2) * pow2(alpS) * 0.5 * sigSum;  

}

//*********

// Select identity, colour and anticolour.

void Sigma2gg2gg::setIdColAcol() {

  // Flavours are trivial.
  setId( id1, id2, 21, 21);

  // Three colour flow topologies, each with two orientations.
  double sigRand = sigSum * Rndm::flat();
  if (sigRand < sigTS) setColAcol( 1, 2, 2, 3, 1, 4, 4, 3);
  else if (sigRand < sigTS + sigUS) 
                       setColAcol( 1, 2, 3, 1, 3, 4, 4, 2);
  else                 setColAcol( 1, 2, 3, 4, 1, 4, 3, 2); 
  if (Rndm::flat() > 0.5) swapColAcol();

}

//**************************************************************************

// Sigma2gg2qqbar class.
// Cross section for g g -> q qbar (q = u, d, s, i.e. almost massless).

//*********

// Initialize process. 
  
void Sigma2gg2qqbar::initProc() {

  // Read number of quarks to be considered in massless approximation.
  nQuarkNew       = Settings::mode("HardQCD:nQuarkNew");

} 

//*********

// Evaluate d(sigmaHat)/d(tHat) - no incoming flavour dependence. 

void Sigma2gg2qqbar::sigmaKin() { 

  // Pick new flavour.
  idNew = 1 + int( nQuarkNew * Rndm::flat() ); 
  mNew  = ParticleDataTable::m0(idNew);
  m2New = mNew*mNew;
  
  // Calculate kinematics dependence.
  sigTS = 0.;
  sigUS = 0.;
  if (sH > 4. * m2New) {
    sigTS = (1./6.) * uH / tH - (3./8.) * uH2 / sH2;
    sigUS = (1./6.) * tH / uH - (3./8.) * tH2 / sH2; 
  }
  sigSum = sigTS + sigUS;

  // Answer is proportional to number of outgoing flavours.
  sigma  = (M_PI / sH2) * pow2(alpS) * nQuarkNew * sigSum;  

}

//*********

// Select identity, colour and anticolour.

void Sigma2gg2qqbar::setIdColAcol() {

  // Flavours are trivial.
  setId( id1, id2, idNew, -idNew);

  // Two colour flow topologies.
  double sigRand = sigSum * Rndm::flat();
  if (sigRand < sigTS) setColAcol( 1, 2, 2, 3, 1, 0, 0, 3);
  else                 setColAcol( 1, 2, 3, 1, 3, 0, 0, 2); 

}

//**************************************************************************

// Sigma2qg2qg class.
// Cross section for q g -> q g.

//*********

// Evaluate d(sigmaHat)/d(tHat) - no incoming flavour dependence. 

void Sigma2qg2qg::sigmaKin() { 

  // Calculate kinematics dependence.
  sigTS  = uH2 / tH2 - (4./9.) * uH / sH;
  sigTU  = sH2 / tH2 - (4./9.) * sH / uH;
  sigSum = sigTS + sigTU;

  // Answer.
  sigma  = (M_PI / sH2) * pow2(alpS) * sigSum;  

}

//*********

// Select identity, colour and anticolour.

void Sigma2qg2qg::setIdColAcol() {

  // Outgoing = incoming flavours.
  setId( id1, id2, id1, id2);

  // Two colour flow topologies. Swap if first is gluon, or when antiquark.
  double sigRand = sigSum * Rndm::flat();
  if (sigRand < sigTS) setColAcol( 1, 0, 2, 1, 3, 0, 2, 3);
  else                 setColAcol( 1, 0, 2, 3, 2, 0, 1, 3); 
  if (id1 == 21) swapCol1234();
  if (id1 < 0 || id2 < 0) swapColAcol();

}

//**************************************************************************

// Sigma2qq2qq class.
// Cross section for q qbar' -> q qbar' or q q' -> q q' 
// (qbar qbar' -> qbar qbar'), q' may be same as q.

//*********

// Evaluate d(sigmaHat)/d(tHat), part independent of incoming flavour. 

void Sigma2qq2qq::sigmaKin() { 

  // Calculate kinematics dependence for different terms.
  sigT   = (4./9.) * (sH2 + uH2) / tH2;
  sigU   = (4./9.) * (sH2 + tH2) / uH2;
  sigTU  = - (8./27.) * sH2 / (tH * uH);
  sigST  = - (8./27.) * uH2 / (sH * tH);

}

//*********


// Evaluate d(sigmaHat)/d(tHat), including incoming flavour dependence. 

double Sigma2qq2qq::sigmaHat() {  

  // Combine cross section terms; factor 1/2 when identical quarks.
  if      (id2 ==  id1) sigSum = 0.5 * (sigT + sigU + sigTU);
  else if (id2 == -id1) sigSum = sigT + sigST;
  else                      sigSum = sigT;

  // Answer.
  return (M_PI/sH2) * pow2(alpS) * sigSum;  

}

//*********

// Select identity, colour and anticolour.

void Sigma2qq2qq::setIdColAcol() {

  // Outgoing = incoming flavours.
  setId( id1, id2, id1, id2);

  // Colour flow topologies. Swap when antiquarks.
  if (id1 * id2 > 0)  setColAcol( 1, 0, 2, 0, 2, 0, 1, 0);
  else                setColAcol( 1, 0, 0, 1, 2, 0, 0, 2);
  if (id2 == id1 && (sigT + sigU) * Rndm::flat() > sigT)
                      setColAcol( 1, 0, 2, 0, 1, 0, 2, 0);
  if (id1 < 0) swapColAcol();

}

//**************************************************************************

// Sigma2qqbar2gg class.
// Cross section for q qbar -> g g.

//*********

// Evaluate d(sigmaHat)/d(tHat) - no incoming flavour dependence. 

void Sigma2qqbar2gg::sigmaKin() { 

  // Calculate kinematics dependence.
  sigTS  = (32./27.) * uH / tH - (8./3.) * uH2 / sH2;
  sigUS  = (32./27.) * tH / uH - (8./3.) * tH2 / sH2;
  sigSum = sigTS + sigUS;

  // Answer contains factor 1/2 from identical gluons.
  sigma  = (M_PI / sH2) * pow2(alpS) * 0.5 * sigSum;  

}

//*********

// Select identity, colour and anticolour.

void Sigma2qqbar2gg::setIdColAcol() {

  // Outgoing flavours trivial.
  setId( id1, id2, 21, 21);

  // Two colour flow topologies. Swap if first is antiquark.
  double sigRand = sigSum * Rndm::flat();
  if (sigRand < sigTS) setColAcol( 1, 0, 0, 2, 1, 3, 3, 2);
  else                 setColAcol( 1, 0, 0, 2, 3, 2, 1, 3); 
  if (id1 < 0) swapColAcol();

}

//**************************************************************************

// Sigma2qqbar2qqbarNew class.
// Cross section q qbar -> q' qbar'.

//*********

// Initialize process. 
  
void Sigma2qqbar2qqbarNew::initProc() {

  // Read number of quarks to be considered in massless approximation.
  nQuarkNew       = Settings::mode("HardQCD:nQuarkNew");

} 

//*********

// Evaluate d(sigmaHat)/d(tHat) - no incoming flavour dependence. 

void Sigma2qqbar2qqbarNew::sigmaKin() { 

  // Pick new flavour.
  idNew = 1 + int( nQuarkNew * Rndm::flat() ); 
  mNew  = ParticleDataTable::m0(idNew);
  m2New = mNew*mNew;

  // Calculate kinematics dependence.
  sigS                      = 0.;
  if (sH > 4. * m2New) sigS = (4./9.) * (tH2 + uH2) / sH2; 

  // Answer is proportional to number of outgoing flavours.
  sigma = (M_PI / sH2) * pow2(alpS) * nQuarkNew * sigS;  

}

//*********

// Select identity, colour and anticolour.

void Sigma2qqbar2qqbarNew::setIdColAcol() {

  // Set outgoing flavours ones.
  id3 = (id1 > 0) ? idNew : -idNew;
  setId( id1, id2, id3, -id3);

  // Colour flow topologies. Swap when antiquarks.
  setColAcol( 1, 0, 0, 2, 1, 0, 0, 2);
  if (id1 < 0) swapColAcol();

}

//**************************************************************************

// Sigma2gg2QQbar class.
// Cross section g g -> Q Qbar (Q = c, b or t).
// Only provided for fixed m3 = m4 so do some gymnastics:
// i) s34Avg picked so that beta34 same when s3, s4 -> s34Avg.
// ii) tHQ = tH - mQ^2 = -0.5 sH (1 - beta34 cos(thetaH)) for m3 = m4 = mQ,
//     but tH - uH = sH beta34 cos(thetaH) also for m3 != m4, so use
//     tH, uH selected for m3 != m4 to derive tHQ, uHQ valid for m3 = m4.   

//*********

// Initialize process. 
  
void Sigma2gg2QQbar::initProc() {

  // Process name.
  nameSave                 = "g g -> Q Qbar";
  if (idNew == 4) nameSave = "g g -> c cbar";
  if (idNew == 5) nameSave = "g g -> b bbar";
  if (idNew == 6) nameSave = "g g -> t tbar";
  if (idNew == 7) nameSave = "g g -> b' b'bar";
  if (idNew == 8) nameSave = "g g -> t' t'bar";

  // Secondary open width fraction.
  openFracPair = ParticleDataTable::resOpenFrac(idNew, -idNew);

} 

//*********

// Evaluate d(sigmaHat)/d(tHat) - no incoming flavour dependence. 

void Sigma2gg2QQbar::sigmaKin() { 

  // Modified Mandelstam variables for massive kinematics with m3 = m4.
  double s34Avg = 0.5 * (s3 + s4) - 0.25 * pow2(s3 - s4) / sH; 
  double tHQ    = -0.5 * (sH - tH + uH);
  double uHQ    = -0.5 * (sH + tH - uH); 
  double tHQ2   = tHQ * tHQ;
  double uHQ2   = uHQ * uHQ;

  // Calculate kinematics dependence.
  double tumHQ = tHQ * uHQ - s34Avg * sH;
  sigTS = ( uHQ / tHQ - 2.25 * uHQ2 / sH2 + 4.5 * s34Avg * tumHQ 
    / ( sH * tHQ2) + 0.5 * s34Avg * (tHQ + s34Avg) / tHQ2 
    - s34Avg*s34Avg / (sH * tHQ) ) / 6.;
  sigUS = ( tHQ / uHQ - 2.25 * tHQ2 / sH2 + 4.5 * s34Avg * tumHQ 
    / ( sH * uHQ2) + 0.5 * s34Avg * (uHQ + s34Avg) / uHQ2 
    - s34Avg*s34Avg / (sH * uHQ) ) / 6.;
  sigSum = sigTS + sigUS;

  // Answer.
  sigma = (M_PI / sH2) * pow2(alpS) * sigSum * openFracPair;  

}

//*********

// Select identity, colour and anticolour.

void Sigma2gg2QQbar::setIdColAcol() {

  // Flavours are trivial.
  setId( id1, id2, idNew, -idNew);

  // Two colour flow topologies.
  double sigRand = sigSum * Rndm::flat();
  if (sigRand < sigTS) setColAcol( 1, 2, 2, 3, 1, 0, 0, 3);
  else                 setColAcol( 1, 2, 3, 1, 3, 0, 0, 2); 

}

//*********

// Evaluate weight for decay angles of W in top decay.

double Sigma2gg2QQbar::weightDecay( Event& process, int iResBeg, 
  int iResEnd) {

  // For top decay hand over to standard routine, else done.
  if (idNew == 6 && process[process[iResBeg].mother1()].idAbs() == 6) 
       return weightTopDecay( process, iResBeg, iResEnd);
  else return 1.; 

}

//**************************************************************************

// Sigma2qqbar2QQbar class.
// Cross section q qbar -> Q Qbar (Q = c, b or t).
// Only provided for fixed m3 = m4 so do some gymnastics:
// i) s34Avg picked so that beta34 same when s3, s4 -> s34Avg.
// ii) tHQ = tH - mQ^2 = -0.5 sH (1 - beta34 cos(thetaH)) for m3 = m4 = mQ,
//     but tH - uH = sH beta34 cos(thetaH) also for m3 != m4, so use
//     tH, uH selected for m3 != m4 to derive tHQ, uHQ valid for m3 = m4.   

//*********

// Initialize process, especially parton-flux object. 
  
void Sigma2qqbar2QQbar::initProc() {

  // Process name.
  nameSave                 = "q qbar -> Q Qbar";
  if (idNew == 4) nameSave = "q qbar -> c cbar";
  if (idNew == 5) nameSave = "q qbar -> b bbar";
  if (idNew == 6) nameSave = "q qbar -> t tbar";
  if (idNew == 7) nameSave = "q qbar -> b' b'bar";
  if (idNew == 8) nameSave = "q qbar -> t' t'bar";

  // Secondary open width fraction.
  openFracPair = ParticleDataTable::resOpenFrac(idNew, -idNew);

} 

//*********

// Evaluate d(sigmaHat)/d(tHat) - no incoming flavour dependence. 

void Sigma2qqbar2QQbar::sigmaKin() { 

  // Modified Mandelstam variables for massive kinematics with m3 = m4.
  double s34Avg = 0.5 * (s3 + s4) - 0.25 * pow2(s3 - s4) / sH; 
  double tHQ    = -0.5 * (sH - tH + uH);
  double uHQ    = -0.5 * (sH + tH - uH); 
  double tHQ2   = tHQ * tHQ;
  double uHQ2   = uHQ * uHQ;

  // Calculate kinematics dependence.
  double sigS = (4./9.) * ((tHQ2 + uHQ2) / sH2 + 2. * s34Avg / sH); 

  // Answer.
  sigma = (M_PI / sH2) * pow2(alpS) * sigS * openFracPair;  

}

//*********

// Select identity, colour and anticolour.

void Sigma2qqbar2QQbar::setIdColAcol() {

  // Set outgoing flavours.
  id3 = (id1 > 0) ? idNew : -idNew;
  setId( id1, id2, id3, -id3);

  // Colour flow topologies. Swap when antiquarks.
  setColAcol( 1, 0, 0, 2, 1, 0, 0, 2);
  if (id1 < 0) swapColAcol();

}

//*********

// Evaluate weight for decay angles of W in top decay.

double Sigma2qqbar2QQbar::weightDecay( Event& process, int iResBeg, 
  int iResEnd) {

  // For top decay hand over to standard routine, else done.
  if (idNew == 6 && process[process[iResBeg].mother1()].idAbs() == 6) 
       return weightTopDecay( process, iResBeg, iResEnd);
  else return 1.; 

}

//**************************************************************************

} // end namespace Pythia8

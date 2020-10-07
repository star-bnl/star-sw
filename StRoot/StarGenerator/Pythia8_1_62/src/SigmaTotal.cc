// SigmaTotal.cc is a part of the PYTHIA event generator.
// Copyright (C) 2012 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Function definitions (not found in the header) for the SigmaTotal class.

#include "SigmaTotal.h"

namespace Pythia8 {

//==========================================================================

// The SigmaTotal class.

// Formulae are taken from:
// G.A. Schuler and T. Sjostrand, Phys. Rev. D49 (1994) 2257,
//   Z. Phys. C73 (1997) 677 
// which borrows some total cross sections from
// A. Donnachie and P.V. Landshoff, Phys. Lett. B296 (1992) 227.

// Implemented processes with their process number iProc:
// =  0 : p + p;     =  1 : pbar + p;
// =  2 : pi+ + p;   =  3 : pi- + p;     =  4 : pi0/rho0 + p; 
// =  5 : phi + p;   =  6 : J/psi + p;
// =  7 : rho + rho; =  8 : rho + phi;   =  9 : rho + J/psi;
// = 10 : phi + phi; = 11 : phi + J/psi; = 12 : J/psi + J/psi.  
// = 13 : Pom + p (preliminary). 

//--------------------------------------------------------------------------
 
// Definitions of static variables.
// Note that a lot of parameters are hardcoded as const here, rather 
// than being interfaced for public change, since any changes would
// have to be done in a globally consistent manner. Which basically 
// means a rewrite/replacement of the whole class.

// Minimum threshold below which no cross sections will be defined.
const double SigmaTotal::MMIN  = 2.; 

// General constants in total cross section parametrization:
// sigmaTot = X * s^epsilon + Y * s^eta (pomeron + reggeon).
const double SigmaTotal::EPSILON = 0.0808;
const double SigmaTotal::ETA     = -0.4525;
const double SigmaTotal::X[] = { 21.70, 21.70, 13.63, 13.63, 13.63, 
  10.01, 0.970, 8.56, 6.29, 0.609, 4.62, 0.447, 0.0434};
const double SigmaTotal::Y[] = { 56.08, 98.39, 27.56, 36.02, 31.79, 
  1.51, -0.146, 13.08, -0.62, -0.060, 0.030, -0.0028, 0.00028};

// Type of the two incoming hadrons as function of the process number:
// = 0 : p/n ; = 1 : pi/rho/omega; = 2 : phi; = 3 : J/psi.
const int SigmaTotal::IHADATABLE[] = { 0, 0, 1, 1, 1, 2, 3, 1, 1, 
  1, 2, 2, 3};
const int SigmaTotal::IHADBTABLE[] = { 0, 0, 0, 0, 0, 0, 0, 1, 2, 
  3, 2, 3, 3};

// Hadron-Pomeron coupling beta(t) = beta(0) * exp(b*t).
const double SigmaTotal::BETA0[] = { 4.658, 2.926, 2.149, 0.208}; 
const double SigmaTotal::BHAD[]  = {   2.3,   1.4,   1.4,  0.23};

// Pomeron trajectory alpha(t) = 1 + epsilon + alpha' * t
const double SigmaTotal::ALPHAPRIME = 0.25; 

// Conversion coefficients = 1/(16pi) * (mb <-> GeV^2) * (G_3P)^n,
// with n = 0 elastic, n = 1 single and n = 2 double diffractive. 
const double SigmaTotal::CONVERTEL = 0.0510925;
const double SigmaTotal::CONVERTSD = 0.0336;  
const double SigmaTotal::CONVERTDD = 0.0084;  

// Diffractive mass spectrum starts at m + MMIN0 and has a low-mass 
// enhancement, factor cRes, up to around m + mRes0.
const double SigmaTotal::MMIN0 = 0.28; 
const double SigmaTotal::CRES  = 2.0; 
const double SigmaTotal::MRES0 = 1.062;

// Parameters and coefficients for single diffractive scattering.
const int SigmaTotal::ISDTABLE[] = { 0, 0, 1, 1, 1, 2, 3, 4, 5, 
  6, 7, 8, 9}; 
const double SigmaTotal::CSD[10][8] = {
  { 0.213, 0.0, -0.47, 150., 0.213, 0.0, -0.47, 150., } , 
  { 0.213, 0.0, -0.47, 150., 0.267, 0.0, -0.47, 100., } ,
  { 0.213, 0.0, -0.47, 150., 0.232, 0.0, -0.47, 110., } ,
  { 0.213, 7.0, -0.55, 800., 0.115, 0.0, -0.47, 110., } ,
  { 0.267, 0.0, -0.46,  75., 0.267, 0.0, -0.46,  75., } ,
  { 0.232, 0.0, -0.46,  85., 0.267, 0.0, -0.48, 100., } ,
  { 0.115, 0.0, -0.50,  90., 0.267, 6.0, -0.56, 420., } ,
  { 0.232, 0.0, -0.48, 110., 0.232, 0.0, -0.48, 110., } ,
  { 0.115, 0.0, -0.52, 120., 0.232, 6.0, -0.56, 470., } ,
  { 0.115, 5.5, -0.58, 570., 0.115, 5.5, -0.58, 570.  } };

// Parameters and coefficients for double diffractive scattering.
const int SigmaTotal::IDDTABLE[] = { 0, 0, 1, 1, 1, 2, 3, 4, 5, 
  6, 7, 8, 9}; 
const double SigmaTotal::CDD[10][9] = {
  { 3.11, -7.34,  9.71, 0.068, -0.42, 1.31, -1.37,  35.0,  118., } ,  
  { 3.11, -7.10,  10.6, 0.073, -0.41, 1.17, -1.41,  31.6,   95., } ,
  { 3.12, -7.43,  9.21, 0.067, -0.44, 1.41, -1.35,  36.5,  132., } ,  
  { 3.13, -8.18, -4.20, 0.056, -0.71, 3.12, -1.12,  55.2, 1298., } ,
  { 3.11, -6.90,  11.4, 0.078, -0.40, 1.05, -1.40,  28.4,   78., } , 
  { 3.11, -7.13,  10.0, 0.071, -0.41, 1.23, -1.34,  33.1,  105., } ,
  { 3.12, -7.90, -1.49, 0.054, -0.64, 2.72, -1.13,  53.1,  995., } , 
  { 3.11, -7.39,  8.22, 0.065, -0.44, 1.45, -1.36,  38.1,  148., } ,
  { 3.18, -8.95, -3.37, 0.057, -0.76, 3.32, -1.12,  55.6, 1472., } ,  
  { 4.18, -29.2,  56.2, 0.074, -1.36, 6.67, -1.14, 116.2, 6532.  } };
const double SigmaTotal::SPROTON = 0.880;

//--------------------------------------------------------------------------

// Store pointer to Info and initialize data members.

void SigmaTotal::init(Info* infoPtrIn, Settings& settings,
  ParticleData* particleDataPtrIn) {

  // Store pointers.
  infoPtr         = infoPtrIn;
  particleDataPtr = particleDataPtrIn;

  // User-set values for cross sections.  
  setTotal   = settings.flag("SigmaTotal:setOwn");
  sigTotOwn  = settings.parm("SigmaTotal:sigmaTot");
  sigElOwn   = settings.parm("SigmaTotal:sigmaEl");
  sigXBOwn   = settings.parm("SigmaTotal:sigmaXB");
  sigAXOwn   = settings.parm("SigmaTotal:sigmaAX");
  sigXXOwn   = settings.parm("SigmaTotal:sigmaXX");

  // User-set values to dampen diffractive cross sections.
  doDampen   = settings.flag("SigmaDiffractive:dampen");
  maxXBOwn   = settings.parm("SigmaDiffractive:maxXB");
  maxAXOwn   = settings.parm("SigmaDiffractive:maxAX");
  maxXXOwn   = settings.parm("SigmaDiffractive:maxXX");

  // User-set values for handling of elastic sacattering. 
  setElastic = settings.flag("SigmaElastic:setOwn");
  bSlope     = settings.parm("SigmaElastic:bSlope");  
  rho        = settings.parm("SigmaElastic:rho");  
  lambda     = settings.parm("SigmaElastic:lambda");  
  tAbsMin    = settings.parm("SigmaElastic:tAbsMin");  
  alphaEM0   = settings.parm("StandardModel:alphaEM0");

  // Parameters for diffractive systems.
  sigmaPomP  = settings.parm("Diffraction:sigmaRefPomP");
  mPomP      = settings.parm("Diffraction:mRefPomP");
  pPomP      = settings.parm("Diffraction:mPowPomP");

}

//--------------------------------------------------------------------------

// Function that calculates the relevant properties.

bool SigmaTotal::calc( int idA, int idB, double eCM) {

  // Derived quantities.
  alP2 = 2. * ALPHAPRIME;
  s0   = 1. / ALPHAPRIME;

  // Reset everything to zero to begin with.
  isCalc = false;
  sigTot = sigEl = sigXB = sigAX = sigXX = sigND = bEl = s = bA = bB = 0.;

  // Order flavour of incoming hadrons: idAbsA < idAbsB (restore later).
  int idAbsA = abs(idA);
  int idAbsB = abs(idB);
  bool swapped = false;
  if (idAbsA > idAbsB) {
    swap( idAbsA, idAbsB);
    swapped = true;
  } 
  double sameSign = (idA * idB > 0);

  // Find process number.
  int iProc                                       = -1;
  if (idAbsA > 1000) {
    iProc                                         = (sameSign) ? 0 : 1;
  } else if (idAbsA > 100 && idAbsB > 1000) {
    iProc                                         = (sameSign) ? 2 : 3;
    if (idAbsA/10 == 11 || idAbsA/10 == 22) iProc = 4;
    if (idAbsA > 300) iProc                       = 5;
    if (idAbsA > 400) iProc                       = 6;
    if (idAbsA > 900) iProc                       = 13;
  } else if (idAbsA > 100) {    
    iProc                                         = 7;
    if (idAbsB > 300) iProc                       = 8;
    if (idAbsB > 400) iProc                       = 9;
    if (idAbsA > 300) iProc                       = 10;
    if (idAbsA > 300 && idAbsB > 400) iProc       = 11;
    if (idAbsA > 400) iProc                       = 12;
  }
  if (iProc == -1) return false;

  // Primitive implementation of Pomeron + p.
  if (iProc == 13) {
    s      = eCM*eCM;
    sigTot = sigmaPomP * pow( eCM / mPomP, pPomP);
    sigND  = sigTot;
    isCalc = true;
    return true;
  }

  // Find hadron masses and check that energy is enough.
  // For mesons use the corresponding vector meson masses.
  int idModA = (idAbsA > 1000) ? idAbsA : 10 * (idAbsA/10) + 3; 
  int idModB = (idAbsB > 1000) ? idAbsB : 10 * (idAbsB/10) + 3; 
  double mA  = particleDataPtr->m0(idModA);
  double mB  = particleDataPtr->m0(idModB);
  if (eCM < mA + mB + MMIN) {
    infoPtr->errorMsg("Error in SigmaTotal::calc: too low energy");
    return false;
  }
  
  // Evaluate the total cross section.
  s           = eCM*eCM;
  double sEps = pow( s, EPSILON);
  double sEta = pow( s, ETA);
  sigTot      = X[iProc] * sEps + Y[iProc] * sEta;

  // Slope of hadron form factors.
  int iHadA = IHADATABLE[iProc];
  int iHadB = IHADBTABLE[iProc];  
  bA        = BHAD[iHadA];
  bB        = BHAD[iHadB];
   
  // Elastic slope parameter and cross section.
  bEl   = 2.*bA + 2.*bB + 4.*sEps - 4.2;
  sigEl = CONVERTEL * pow2(sigTot) / bEl;

  // Lookup coefficients for single and double diffraction.
  int iSD = ISDTABLE[iProc];
  int iDD = IDDTABLE[iProc];
  double sum1, sum2, sum3, sum4;

  // Single diffractive scattering A + B -> X + B cross section.
  mMinXBsave      = mA + MMIN0;
  double sMinXB   = pow2(mMinXBsave);
  mResXBsave      = mA + MRES0;
  double sResXB   = pow2(mResXBsave);
  double sRMavgXB = mResXBsave * mMinXBsave;
  double sRMlogXB = log(1. + sResXB/sMinXB);
  double sMaxXB   = CSD[iSD][0] * s + CSD[iSD][1];
  double BcorrXB  = CSD[iSD][2] + CSD[iSD][3] / s;
  sum1  = log( (2.*bB + alP2 * log(s/sMinXB)) 
    / (2.*bB + alP2 * log(s/sMaxXB)) ) / alP2; 
  sum2  = CRES * sRMlogXB / (2.*bB + alP2 * log(s/sRMavgXB) + BcorrXB) ; 
  sigXB = CONVERTSD * X[iProc] * BETA0[iHadB] * max( 0., sum1 + sum2);

  // Single diffractive scattering A + B -> A + X cross section.
  mMinAXsave      = mB + MMIN0;
  double sMinAX   = pow2(mMinAXsave);
  mResAXsave      = mB + MRES0;
  double sResAX   = pow2(mResAXsave);
  double sRMavgAX = mResAXsave * mMinAXsave;
  double sRMlogAX = log(1. + sResAX/sMinAX);
  double sMaxAX   = CSD[iSD][4] * s + CSD[iSD][5];
  double BcorrAX  = CSD[iSD][6] + CSD[iSD][7] / s;
  sum1  = log( (2.*bA + alP2 * log(s/sMinAX)) 
    / (2.*bA + alP2 * log(s/sMaxAX)) ) / alP2; 
  sum2  = CRES * sRMlogAX / (2.*bA + alP2 * log(s/sRMavgAX) + BcorrAX) ;  
  sigAX = CONVERTSD * X[iProc] * BETA0[iHadA] * max( 0., sum1 + sum2);
 
  // Order single diffractive correctly.
  if (swapped) {
    swap( bB, bA);
    swap( sigXB, sigAX);
    swap( mMinXBsave, mMinAXsave);
    swap( mResXBsave, mResAXsave);
   }

  // Double diffractive scattering A + B -> X1 + X2 cross section.
  double y0min = log( s * SPROTON / (sMinXB * sMinAX) ) ;
  double sLog  = log(s);  
  double Delta0 = CDD[iDD][0] + CDD[iDD][1] / sLog 
    + CDD[iDD][2] / pow2(sLog);
  sum1 = (y0min * (log( max( 1e-10, y0min/Delta0) ) - 1.) + Delta0)/ alP2;
  if (y0min < 0.) sum1 = 0.;
  double sMaxXX = s * ( CDD[iDD][3] + CDD[iDD][4] / sLog
    + CDD[iDD][5] / pow2(sLog) );
  double sLogUp = log( max( 1.1, s * s0 / (sMinXB * sRMavgAX) ));
  double sLogDn = log( max( 1.1, s * s0 / (sMaxXX * sRMavgAX) ));
  sum2   = CRES * log( sLogUp / sLogDn ) * sRMlogAX / alP2;
  sLogUp = log( max( 1.1, s * s0 / (sMinAX * sRMavgXB) ));
  sLogDn = log( max( 1.1, s * s0 / (sMaxXX * sRMavgXB) ));
  sum3   = CRES * log(sLogUp / sLogDn) * sRMlogXB / alP2;
  double BcorrXX =  CDD[iDD][6] + CDD[iDD][7] / eCM + CDD[iDD][8] / s;
  sum4   = pow2(CRES) * sRMlogAX * sRMlogXB 
    / max( 0.1, alP2 * log( s * s0 / (sRMavgAX * sRMavgXB) ) + BcorrXX);
  sigXX  = CONVERTDD * X[iProc] * max( 0., sum1 + sum2 + sum3 + sum4);

  // Option with user-requested damping of diffractive cross sections.
  if (doDampen) {
    sigXB = sigXB * maxXBOwn / (sigXB + maxXBOwn);
    sigAX = sigAX * maxAXOwn / (sigAX + maxAXOwn);
    sigXX = sigXX * maxXXOwn / (sigXX + maxXXOwn);
  }
 
  // Option with user-set values for total and partial cross sections.
  // (Is not done earlier since want diffractive slopes anyway.)
  double sigNDOwn = sigTotOwn - sigElOwn - sigXBOwn - sigAXOwn - sigXXOwn; 
  double sigElMax = sigEl;
  if (setTotal && sigNDOwn > 0.) {
    sigTot   = sigTotOwn;
    sigEl    = sigElOwn;
    sigXB    = sigXBOwn;
    sigAX    = sigAXOwn;
    sigXX    = sigXXOwn;
    sigElMax = sigEl;

    // Sub-option to set elastic parameters, including Coulomb contribution.
    if (setElastic) {
      bEl      = bSlope;
      sigEl    = CONVERTEL * pow2(sigTot) * (1. + rho*rho) / bSlope;
      sigElMax = 2. * (sigEl * exp(-bSlope * tAbsMin)
               + alphaEM0 * alphaEM0 / (4. * CONVERTEL * tAbsMin) );
    }
  }

  // Inelastic nondiffractive by unitarity.
  sigND = sigTot - sigEl - sigXB - sigAX - sigXX; 
  if (sigND < 0.) infoPtr->errorMsg("Error in SigmaTotal::init: "
    "sigND < 0"); 
  else if (sigND < 0.4 * sigTot) infoPtr->errorMsg("Warning in "
    "SigmaTotal::init: sigND suspiciously low"); 

  // Upper estimate of elastic, including Coulomb term, where appropriate.
  sigEl = sigElMax;

  // Done.
  isCalc = true;
  return true;

}

//==========================================================================

} // end namespace Pythia8

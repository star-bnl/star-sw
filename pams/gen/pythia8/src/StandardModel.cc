// StandardModel.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Function definitions (not found in the header) for the AlphaStrong class.

#include "StandardModel.h"

namespace Pythia8 {

//**************************************************************************

// The AlphaStrong class.

//*********

// Constants: could be changed here if desired, but normally should not.
// These are of technical nature, as described for each.

// Number of iterations to determine Lambda from given alpha_s.
const int AlphaStrong::NITER           = 10;

// Always evaluate running alpha_s above Lambda3 to avoid disaster.
// Safety margin picked to freeze roughly for alpha_s = 10.
const double AlphaStrong::SAFETYMARGIN1 = 1.07;
const double AlphaStrong::SAFETYMARGIN2 = 1.33;

//*********

// Initialize alpha_strong calculation by finding Lambda values etc.

void AlphaStrong::init( double valueIn, int orderIn) {

  // Order of alpha_s evaluation. Charm, bottom and Z0 masses. 
  // Pick defaults if ParticleDataTable not properly initialized.
  valueRef = valueIn;
  order    = max( 0, min( 2, orderIn ) );
  mc                           = ParticleDataTable::m0(4);
  if (mc < 1. || mc > 2.) mc   = 1.5;
  mb                           = ParticleDataTable::m0(5);
  if (mb < 3. || mb > 6.) mb   = 4.8;
  mZ                           = ParticleDataTable::m0(23);
  if (mZ < 90. || mZ > 92.) mZ = 91.188;

  // Fix alpha_s.
  if (order == 0) {
    Lambda3Save = Lambda4Save = Lambda5Save = scale2Min = 0.;

  // First order alpha_s: match at flavour thresholds.
  } else if (order == 1) {
    Lambda5Save = mZ * exp( -6. * M_PI / (23. * valueRef) );
    Lambda4Save = Lambda5Save * pow(mb/Lambda5Save, 2./25.); 
    Lambda3Save = Lambda4Save * pow(mc/Lambda4Save, 2./27.); 
    scale2Min   = pow2(SAFETYMARGIN1 * Lambda3Save);

  // Second order alpha_s: iterative match at flavour thresholds.
  } else {
    double b15 = 348. / 529.;
    double b14 = 462. / 625.;
    double b13 = 64. / 81.;    
    double b25 = 224687. / 242208.;      
    double b24 = 548575. / 426888.;
    double b23 = 938709. / 663552.;
    double logScale, loglogScale, correction, valueIter;

    // Find Lambda_5 at m_Z.
    Lambda5Save = mZ * exp( -6. * M_PI / (23. * valueRef) );
    for (int iter = 0; iter < NITER; ++iter) {
      logScale    = 2. * log(mZ/Lambda5Save);
      loglogScale = log(logScale);
      correction  = 1. - b15 * loglogScale / logScale 
        + pow2(b15 / logScale) * (pow2(loglogScale - 0.5) + b25 - 1.25);
      valueIter   = valueRef / correction; 
      Lambda5Save = mZ * exp( -6. * M_PI / (23. * valueIter) );
    }

    // Find Lambda_4 at m_b.
    double logScaleB    = 2. * log(mb/Lambda5Save);
    double loglogScaleB = log(logScaleB);
    double valueB       = 12. * M_PI / (23. * logScaleB) 
      * (1. - b15 * loglogScaleB / logScaleB
        + pow2(b15 / logScaleB) * (pow2(loglogScaleB - 0.5) + b25- 1.25) ); 
    Lambda4Save         = Lambda5Save;
    for (int iter = 0; iter < NITER; ++iter) {
      logScale    = 2. * log(mb/Lambda4Save);
      loglogScale = log(logScale);
      correction  = 1. - b14 * loglogScale / logScale 
        + pow2(b14 / logScale) * (pow2(loglogScale - 0.5) + b24 - 1.25);
      valueIter   = valueB / correction; 
      Lambda4Save = mb * exp( -6. * M_PI / (25. * valueIter) );
    }

    // Find Lambda_3 at m_c.
    double logScaleC    = 2. * log(mc/Lambda4Save);
    double loglogScaleC = log(logScaleC);
    double valueC       = 12. * M_PI / (25. * logScaleC) 
      * (1. - b14 * loglogScaleC / logScaleC
        + pow2(b14 / logScaleC) * (pow2(loglogScaleC - 0.5) + b24 - 1.25) ); 
    Lambda3Save = Lambda4Save;
    for (int iter = 0; iter < NITER; ++iter) {
      logScale    = 2. * log(mc/Lambda3Save);
      loglogScale = log(logScale);
      correction  = 1. - b13 * loglogScale / logScale 
        + pow2(b13 / logScale) * (pow2(loglogScale - 0.5) + b23 - 1.25);
      valueIter   = valueC / correction; 
      Lambda3Save = mc * exp( -6. * M_PI / (27. * valueIter) );
    }
    scale2Min     = pow2(SAFETYMARGIN2 * Lambda3Save);
  }

  // Save squares of mass and Lambda values as well.
  mc2          = pow2(mc);
  mb2          = pow2(mb);
  Lambda3Save2 = pow2(Lambda3Save);
  Lambda4Save2 = pow2(Lambda4Save);
  Lambda5Save2 = pow2(Lambda5Save);
  valueNow     = valueIn;
  scale2Now    = mZ * mZ;
  isInit = true;

}

//*********

// Calculate alpha_s value    

double AlphaStrong::alphaS( double scale2) {

  // Check for initialization and ensure minimal scale2 value.
  if (!isInit) return 0.;
  if (scale2 < scale2Min) scale2 = scale2Min;

  // If equal to old scale then same answer.
  if (scale2 == scale2Now && (order < 2 || lastCallToFull)) return valueNow;
  scale2Now      = scale2;
  lastCallToFull = true;

  // Fix alpha_s.
  if (order == 0) {
    valueNow = valueRef;        
  
  // First order alpha_s: differs by mass region.  
  } else if (order == 1) {
    if (scale2 > mb2) 
         valueNow = 12. * M_PI / (23. * log(scale2/Lambda5Save2));
    else if (scale2 > mc2) 
         valueNow = 12. * M_PI / (25. * log(scale2/Lambda4Save2));
    else valueNow = 12. * M_PI / (27. * log(scale2/Lambda3Save2));
  
  // Second order alpha_s: differs by mass region.  
  } else {
    double Lambda2, b0, b1, b2;
    if (scale2 > mb2) {
      Lambda2 = Lambda5Save2;
      b0      = 23.;
      b1      = 348. / 529.; 
      b2      = 224687. / 242208.;      
    } else if (scale2 > mc2) {     
      Lambda2 = Lambda4Save2;
      b0      = 25.;
      b1      = 462. / 625.;
      b2      = 548575. / 426888.;
    } else {       
      Lambda2 = Lambda3Save2;
      b0      = 27.;
      b1      = 64. / 81.;
      b2      = 938709. / 663552.;
    }
    double logScale    = log(scale2/Lambda2);
    double loglogScale = log(logScale);
    valueNow = 12. * M_PI / (b0 * logScale) 
      * ( 1. - b1 * loglogScale / logScale 
        + pow2(b1 / logScale) * (pow2(loglogScale - 0.5) + b2 - 1.25) ); 
  }

  // Done.
  return valueNow;

} 

//*********

// Calculate alpha_s value, but only use up to first-order piece.
// (To be combined with alphaS2OrdCorr.)

double  AlphaStrong::alphaS1Ord( double scale2) {

  // Check for initialization and ensure minimal scale2 value.
  if (!isInit) return 0.;
  if (scale2 < scale2Min) scale2 = scale2Min;

  // If equal to old scale then same answer.
  if (scale2 == scale2Now && (order < 2 || !lastCallToFull)) return valueNow;
  scale2Now      = scale2;
  lastCallToFull = false;

  // Fix alpha_S.
  if (order == 0) {
    valueNow = valueRef;        
  
  // First/second order alpha_s: differs by mass region.  
  } else {
    if (scale2 > mb2) 
         valueNow = 12. * M_PI / (23. * log(scale2/Lambda5Save2));
    else if (scale2 > mc2) 
         valueNow = 12. * M_PI / (25. * log(scale2/Lambda4Save2));
    else valueNow = 12. * M_PI / (27. * log(scale2/Lambda3Save2));
  }

  // Done.
  return valueNow;
} 

//*********

// Calculates the second-order extra factor in alpha_s.
// (To be combined with alphaS1Ord.)

double AlphaStrong::alphaS2OrdCorr( double scale2) {

  // Check for initialization and ensure minimal scale2 value.
  if (!isInit) return 1.;
  if (scale2 < scale2Min) scale2 = scale2Min;

  // Only meaningful for second order calculations.
  if (order < 2) return 1.; 
  
  // Second order correction term: differs by mass region.  
  double Lambda2, b0, b1, b2;
  if (scale2 > mb2) {
    Lambda2 = Lambda5Save2;
    b0      = 23.;
    b1      = 348. / 529.;       
    b2      = 224687. / 242208.;      
  } else if (scale2 > mc2) {     
    Lambda2 = Lambda4Save2;
    b0      = 25.;
    b1      = 462. / 625.;
    b2      = 548575. / 426888.;
  } else {       
    Lambda2 = Lambda3Save2;
    b0      = 27.;
    b1      = 64. / 81.;
    b2      = 938709. / 663552.;
  }
  double logScale    = log(scale2/Lambda2);
  double loglogScale = log(logScale);
  return ( 1. - b1 * loglogScale / logScale 
    + pow2(b1 / logScale) * (pow2(loglogScale - 0.5) + b2 - 1.25) ); 

} 

//**************************************************************************

// The AlphaEM class.

//*********

// Definitions of static variables.
double AlphaEM::alpEM0  = 0.00729735;
double AlphaEM::alpEMmZ = 0.00781751;
double AlphaEM::mZ2     = 8315.;
// Effective thresholds for electron, muon, light quarks, tau+c, b.
double AlphaEM::Q2step[5]    = {0.26e-6, 0.011, 0.25, 3.5, 90.};
// Running coefficients are sum charge2 / 3 pi in pure QED, here slightly
// enhanced for quarks to approximately account for QCD corrections.
double AlphaEM::bRun[5]      = {0.1061, 0.2122, 0.460, 0.700, 0.725};
double AlphaEM::alpEMstep[5] = {};

//*********

// Initialize alpha_EM calculation.

void AlphaEM::initStatic() {

  // Read in alpha_EM value at 0 and m_Z, and mass of Z.
  alpEM0    = Settings::parm("StandardModel:alphaEM0");
  alpEMmZ   = Settings::parm("StandardModel:alphaEMmZ");
  double mZ = ParticleDataTable::m0(23);   
  mZ2       = mZ * mZ;

  // AlphaEM values at matching scales and matching b value.

  // Step down from mZ to tau/charm threshold. 
  alpEMstep[4] = alpEMmZ / ( 1. + alpEMmZ * bRun[4] 
    * log(mZ2 / Q2step[4]) );
  alpEMstep[3] = alpEMstep[4] / ( 1. - alpEMstep[4] * bRun[3] 
    * log(Q2step[3] / Q2step[4]) );

  // Step up from me to light-quark threshold.
  alpEMstep[0] = alpEM0;   
  alpEMstep[1] = alpEMstep[0] / ( 1. - alpEMstep[0] * bRun[0] 
    * log(Q2step[1] / Q2step[0]) );
  alpEMstep[2] = alpEMstep[1] / ( 1. - alpEMstep[1] * bRun[1] 
    * log(Q2step[2] / Q2step[1]) );

  // Fit b in range between light-quark and tau/charm to join smoothly.
  bRun[2] = (1./alpEMstep[3] - 1./alpEMstep[2])
    / log(Q2step[2] / Q2step[3]);

}

//*********

// Calculate alpha_EM value    

double AlphaEM::alphaEM( double scale2) {

  // Fix alphaEM; for order = -1 fixed at m_Z.
  if (order == 0)  return alpEM0;
  if (order <  0)  return alpEMmZ;

  // Running alphaEM.
  for (int i = 4; i >= 0; --i) if (scale2 > Q2step[i])
    return alpEMstep[i] / (1. - bRun[i] * alpEMstep[i] 
      * log(scale2 / Q2step[i]) );
  return alpEM0;

}

//**************************************************************************

// The CoupEW class.

//*********

// Definitions of static variables.

double CoupEW::s2tW = 0.232;
double CoupEW::c2tW = 0.768;
double CoupEW::s2tWbar = 0.232;
double CoupEW::efSave[20] = { 0., -1./3., 2./3., -1./3., 2./3., -1./3., 
  2./3., -1./3., 2./3., 0., 0., -1., 0., -1., 0., -1., 0., -1., 0., 0.};
double CoupEW::vfSave[20] = { }; 
double CoupEW::afSave[20] = { 0., -1., 1., -1., 1., -1., 1., -1., 1., 
  0., 0., -1., 1., -1., 1., -1., 1., -1., 1., 0.};
double CoupEW::lfSave[20] = { }; 
double CoupEW::rfSave[20] = { }; 
double CoupEW::ef2Save[20] = { }; 
double CoupEW::vf2Save[20] = { }; 
double CoupEW::af2Save[20] = { }; 
double CoupEW::efvfSave[20] = { }; 
double CoupEW::vf2af2Save[20] = { }; 

//*********

// Initialize electroweak mixing angle and couplings.

void CoupEW::initStatic() { 

  // Read in electroweak mixing angle.
  s2tW = Settings::parm("StandardModel:sin2thetaW");
  c2tW = 1. - s2tW;
  s2tWbar = Settings::parm("StandardModel:sin2thetaWbar");

  // Initialize electroweak couplings.
  for (int i = 0; i < 20; ++i) {  
    vfSave[i]  = afSave[i] - 4. * s2tWbar * efSave[i];
    lfSave[i]  = afSave[i] - 2. * s2tWbar * efSave[i];
    rfSave[i]  =           - 2. * s2tWbar * efSave[i];
    ef2Save[i] = pow2(efSave[i]);
    vf2Save[i] = pow2(vfSave[i]);
    af2Save[i] = pow2(afSave[i]);
    efvfSave[i] = efSave[i] * vfSave[i];
    vf2af2Save[i] = vf2Save[i] + af2Save[i];
  }

}

//**************************************************************************

// The VCKM class.

//*********

// Definitions of static variables. Initialize to all elements zero.

double VCKM::Vsave[5][5]  = { };
double VCKM::V2save[5][5] = { };
double VCKM::V2out[20]    = { };

//*********

// Prepare to handle CKM matrix elements.

void VCKM::initStatic() { 

  // Read in matrix element values and store them.
  Vsave[1][1] = Settings::parm("StandardModel:Vud");
  Vsave[1][2] = Settings::parm("StandardModel:Vus");
  Vsave[1][3] = Settings::parm("StandardModel:Vub");
  Vsave[2][1] = Settings::parm("StandardModel:Vcd");
  Vsave[2][2] = Settings::parm("StandardModel:Vcs");
  Vsave[2][3] = Settings::parm("StandardModel:Vcb");
  Vsave[3][1] = Settings::parm("StandardModel:Vtd");
  Vsave[3][2] = Settings::parm("StandardModel:Vts");
  Vsave[3][3] = Settings::parm("StandardModel:Vtb");

  // Also allow for the potential existence of a fourth generation.
  Vsave[1][4] = Settings::parm("FourthGeneration:VubPrime");
  Vsave[2][4] = Settings::parm("FourthGeneration:VcbPrime");
  Vsave[3][4] = Settings::parm("FourthGeneration:VtbPrime");
  Vsave[4][1] = Settings::parm("FourthGeneration:VtPrimed");
  Vsave[4][2] = Settings::parm("FourthGeneration:VtPrimes");
  Vsave[4][3] = Settings::parm("FourthGeneration:VtPrimeb");
  Vsave[4][4] = Settings::parm("FourthGeneration:VtPrimebPrime");

  // Calculate squares of matrix elements.
  for(int i = 1; i < 5; ++i) for(int j = 1; j < 5; ++j) 
    V2save[i][j] = pow2(Vsave[i][j]); 
  
  // Sum VCKM^2_out sum for given incoming flavour, excluding top as partner.
  V2out[1] = V2save[1][1] + V2save[2][1];
  V2out[2] = V2save[1][1] + V2save[1][2] + V2save[1][3];
  V2out[3] = V2save[1][2] + V2save[2][2];
  V2out[4] = V2save[2][1] + V2save[2][2] + V2save[2][3];
  V2out[5] = V2save[1][3] + V2save[2][3];
  V2out[6] = V2save[3][1] + V2save[3][2] + V2save[3][3];
  V2out[7] = V2save[1][4] + V2save[2][4];
  V2out[8] = V2save[4][1] + V2save[4][2] + V2save[4][3];
  for (int i = 11; i <= 18; ++i) V2out[i] = 1.;
 
}

//*********

// Return CKM value for incoming flavours (sign irrelevant).

double VCKM::Vid(int id1, int id2) {

  // Use absolute sign (want to cover both f -> f' W and f fbar' -> W).
  int id1Abs = abs(id1);
  int id2Abs = abs(id2);
  if (id1Abs == 0 || id2Abs == 0 || (id1Abs + id2Abs)%2 != 1) return 0.;

  // Ensure proper order before reading out from Vsave or lepton match.
  if (id1Abs%2 == 1) swap(id1Abs, id2Abs);
  if (id1Abs <= 8 && id2Abs <= 8) return Vsave[id1Abs/2][(id2Abs + 1)/2];
  if ( (id1Abs == 12 || id1Abs == 14 || id1Abs == 16 || id1Abs == 18) 
    && id2Abs == id1Abs - 1 ) return 1.;
  
  // No more valid cases.
  return 0.;

}

//*********

// Return squared CKM value for incoming flavours (sign irrelevant).

double VCKM::V2id(int id1, int id2) {

  // Use absolute sign (want to cover both f -> f' W and f fbar' -> W).
  int id1Abs = abs(id1);
  int id2Abs = abs(id2);
  if (id1Abs == 0 || id2Abs == 0 || (id1Abs + id2Abs)%2 != 1) return 0.;

  // Ensure proper order before reading out from V2save or lepton match.
  if (id1Abs%2 == 1) swap(id1Abs, id2Abs);
  if (id1Abs <= 8 && id2Abs <= 8) return V2save[id1Abs/2][(id2Abs + 1)/2];
  if ( (id1Abs == 12 || id1Abs == 14 || id1Abs == 16 || id1Abs == 18) 
    && id2Abs == id1Abs - 1 ) return 1.;
  
  // No more valid cases.
  return 0.;

}

//*********

// Pick an outgoing flavour for given incoming one, given CKM mixing.

int VCKM::V2pick(int id) {

  // Initial values.
  int idIn = abs(id);
  int idOut = 0;
  
  // Quarks: need to make random choice.
  if (idIn >= 1 && idIn <= 8) {
    double V2rndm = Rndm::flat() * V2out[idIn]; 
    if (idIn == 1) idOut = (V2rndm < V2save[1][1]) ? 2 : 4;
    else if (idIn == 2) idOut = (V2rndm < V2save[1][1]) ? 1 
      : ( (V2rndm < V2save[1][1] + V2save[1][2]) ? 3 : 5 );
    else if (idIn == 3) idOut = (V2rndm < V2save[1][2]) ? 2 : 4;
    else if (idIn == 4) idOut = (V2rndm < V2save[2][1]) ? 1 
      : ( (V2rndm < V2save[2][1] + V2save[2][2]) ? 3 : 5 );
    else if (idIn == 5) idOut = (V2rndm < V2save[1][3]) ? 2 : 4;
    else if (idIn == 6) idOut = (V2rndm < V2save[3][1]) ? 1 
      : ( (V2rndm < V2save[3][1] + V2save[3][2]) ? 3 : 5 );
    else if (idIn == 7) idOut = (V2rndm < V2save[1][4]) ? 2 : 4;
    else if (idIn == 8) idOut = (V2rndm < V2save[4][1]) ? 1 
      : ( (V2rndm < V2save[4][1] + V2save[4][2]) ? 3 : 5 );
  
  // Leptons: unambiguous. 
  } else if (idIn >= 11 && idIn <= 18) {
    if (idIn%2 == 1) idOut = idIn + 1;
    else idOut             = idIn - 1;
  } 

  // Done. Return with sign.
  return ( (id > 0) ? idOut : -idOut );

}

//**************************************************************************

} // end namespace Pythia8

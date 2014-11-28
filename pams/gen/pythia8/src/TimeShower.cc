// TimeShower.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Function definitions (not found in the header) for the TimeShower class. 

#include "TimeShower.h"

namespace Pythia8 {

//**************************************************************************

// The TimeShower class.

//*********

// Constants: could be changed here if desired, but normally should not.
// These are of technical nature, as described for each.

// For small x approximate 1 - sqrt(1 - x) by x/2.
const double TimeShower::SIMPLIFYROOT = 1e-8;

// Do not allow x too close to 0 or 1 in matrix element expressions.
// Warning: cuts into phase space for E_CM > 2 * pTmin * sqrt(1/XMARGIN),
// i.e. will become problem roughly for E_CM > 10^6 GeV.
const double TimeShower::XMARGIN      = 1e-12;
const double TimeShower::XMARGINCOMB  = 1e-4;

// Lower limit on PDF value in order to avoid division by zero.
const double TimeShower::TINYPDF      = 1e-10;

// Big starting value in search for smallest invariant-mass pair.
const double TimeShower::LARGEM2      = 1e20;

// In g -> q qbar or gamma -> f fbar require m2_pair > this * m2_q/f. 
const double TimeShower::THRESHM2      = 4.004;

// Never pick pT so low that alphaS is evaluated too close to Lambda_3. 
const double TimeShower::LAMBDA3MARGIN = 1.1;

//*********

// Initialize alphaStrong, alphaEM and related pTmin parameters.

void TimeShower::init( BeamParticle* beamAPtrIn, 
  BeamParticle* beamBPtrIn) {

  // Store input pointers for future use. 
  beamAPtr           = beamAPtrIn;
  beamBPtr           = beamBPtrIn;

  // Main flags.
  doQCDshower        = Settings::flag("TimeShower:QCDshower");
  doQEDshowerByQ     = Settings::flag("TimeShower:QEDshowerByQ");
  doQEDshowerByL     = Settings::flag("TimeShower:QEDshowerByL");
  doQEDshowerByGamma = Settings::flag("TimeShower:QEDshowerByGamma");
  doMEcorrections    = Settings::flag("TimeShower:MEcorrections");
  doPhiPolAsym       = Settings::flag("TimeShower:phiPolAsym"); 
  allowBeamRecoil    = Settings::flag("TimeShower:allowBeamRecoil"); 

  // Matching in pT of hard interaction to shower evolution.
  pTmaxFudge         = Settings::parm("TimeShower:pTmaxFudge"); 

  // Charm and bottom mass thresholds.
  mc                 = ParticleDataTable::m0(4); 
  mb                 = ParticleDataTable::m0(5); 
  m2c                = mc * mc;
  m2b                = mb * mb;
       
  // Parameters of alphaStrong generation .
  alphaSvalue        = Settings::parm("TimeShower:alphaSvalue");
  alphaSorder        = Settings::mode("TimeShower:alphaSorder");
  alphaS2pi          = 0.5 * alphaSvalue / M_PI;

  // Initialize alphaStrong generation.
  alphaS.init( alphaSvalue, alphaSorder); 
  
  // Lambda for 5, 4 and 3 flavours.
  Lambda3flav        = alphaS.Lambda3(); 
  Lambda4flav        = alphaS.Lambda4(); 
  Lambda5flav        = alphaS.Lambda5(); 
  Lambda5flav2       = pow2(Lambda5flav);
  Lambda4flav2       = pow2(Lambda4flav);
  Lambda3flav2       = pow2(Lambda3flav);
 
  // Parameters of QCD evolution. 
  nGluonToQuark      = Settings::mode("TimeShower:nGluonToQuark");
  pTcolCutMin        = Settings::parm("TimeShower:pTmin"); 
  pTcolCut           = max( pTcolCutMin, LAMBDA3MARGIN * Lambda3flav );
  pT2colCut          = pow2(pTcolCut);  
       
  // Parameters of alphaEM generation .
  alphaEMorder       = Settings::mode("TimeShower:alphaEMorder");

  // Initialize alphaEM generation.
  alphaEM.init( alphaEMorder); 
 
  // Parameters of QED evolution.
  nGammaToQuark      = Settings::mode("TimeShower:nGammaToQuark");
  nGammaToLepton     = Settings::mode("TimeShower:nGammaToLepton");
  pTchgQCut          = Settings::parm("TimeShower:pTminChgQ"); 
  pT2chgQCut         = pow2(pTchgQCut);
  pTchgLCut          = Settings::parm("TimeShower:pTminChgL"); 
  pT2chgLCut         = pow2(pTchgLCut);
  mMaxGamma          = Settings::parm("TimeShower:mMaxGamma"); 
  m2MaxGamma         = pow2(mMaxGamma);

  // Consisteny check for gamma -> f fbar variables.
  if (nGammaToQuark <= 0 && nGammaToLepton <= 0) doQEDshowerByGamma = false;  

  // Fraction and colorr factor of gluon emission off onium octat state.
  octetOniumFraction = Settings::parm("TimeShower:octetOniumFraction");
  octetOniumColFac   = Settings::parm("TimeShower:octetOniumColFac");

  // Z0 properties needed for gamma/Z0 mixing.
  mZ                 = ParticleDataTable::m0(23);
  gammaZ             = ParticleDataTable::mWidth(23);
  thetaWRat          = 1. / (16. * CoupEW::sin2thetaW() * CoupEW::cos2thetaW());

}

//*********

// Top-level routine to do a full time-like shower in resonance decay.

int TimeShower::shower( int iBeg, int iEnd, Event& event, double pTmax) {

  // Add new system, with two empty beam slots.
  int iSys = event.newSystem();
  event.addToSystem( iSys, 0);
  event.addToSystem( iSys, 0);
    
  // Loop over allowed range to find all final-state particles.
  for (int i = iBeg; i <= iEnd; ++i) 
  if (event[i].isFinal()) event.addToSystem( iSys, i);

  // Let prepare routine do the setup.    
  prepare( iSys, event);

  // Begin evolution down in pT from hard pT scale. 
  int nBranch = 0;
  do {
    double pTtimes = pTnext( event, pTmax, 0.);

    // Do a final-state emission (if allowed).
    if (pTtimes > 0.) {
      if (branch( event)) ++nBranch; 
      pTmax = pTtimes;
    }
    
    // Keep on evolving until nothing is left to be done.
    else pTmax = 0.;
  } while (pTmax > 0.); 

  // Return number of emissions that were performed.
  return nBranch;

}

//*********

// Prepare system for evolution; identify ME.

void TimeShower::prepare( int iSys, Event& event) {

  // Reset dipole-ends list for first interaction and for resonance decays.
  if (iSys == 0 || event.getInSystem( iSys, 0) == 0) dipEnd.resize(0);
  int dipEndSizeBeg = dipEnd.size();
 
  // Loop through final state of system to find possible dipole ends.
  for (int i = 2; i < event.sizeSystem( iSys); ++i) {
    int iRad = event.getInSystem( iSys, i);
    if (event[iRad].isFinal() && event[iRad].scale() > 0.) {

      // Identify colour octet onium state. Check whether QCD shower allowed.
      int idRad = event[iRad].id();
      bool isOctetOnium 
        = ( idRad == 9900441 || idRad == 9900443 || idRad == 9910441 
         || idRad == 9900551 || idRad == 9900553 || idRad == 9910551 ); 
      bool doQCD = doQCDshower;
      if (doQCD && isOctetOnium) doQCD = (Rndm::flat() < octetOniumFraction);

      // Find dipole end formed by colour index.
      int colTag = event[iRad].col();    
      if (doQCD && colTag > 0) 
        setupQCDdip( iSys, i,  colTag,  1, event, isOctetOnium); 

      // Find dipole end formed by anticolour index.
      int acolTag = event[iRad].acol();     
      if (doQCD && acolTag > 0) 
        setupQCDdip( iSys, i, acolTag, -1, event, isOctetOnium); 

      // Find "charge-dipole" and "photon-dipole" ends. 
      int  chgType  = event[iRad].chargeType();  
      bool doChgDip = (chgType != 0) 
                       && ( ( doQEDshowerByQ && event[iRad].isQuark()  )
                         || ( doQEDshowerByL && event[iRad].isLepton() ) );
      int  gamType  = (event[iRad].id() == 22) ? 1 : 0;
      bool doGamDip = (gamType == 1) && doQEDshowerByGamma;
      if (doChgDip || doGamDip) setupQEDdip( iSys, i, chgType, gamType, event); 

    // End loop over system final state. Have now found the dipole ends.
    }
  }

  // Loop through dipole ends to find matrix element corrections.
  for (int iDip = dipEndSizeBeg; iDip < int(dipEnd.size()); ++iDip) 
    findMEtype( event, dipEnd[iDip]); 

}

//*********

// Update dipole list after each ISR emission (so not used for resonances).  

void TimeShower::update( int iSys, Event& event) {

  // Find new and old positions of partons in the system.
  vector<int> iNew, iOld;
  int size = event.sizeSystem( iSys) - 1;  
  for (int i = 0; i < size; ++i) {
    iNew.push_back( event.getInSystem( iSys, i) );
    if (i < 2) iOld.push_back( event[iNew[i]].daughter2() );
    else       iOld.push_back( event[iNew[i]].mother1() );
  }
  int iNewNew = event.getInSystem( iSys, size);
  
  // Swap beams to let 0 be side on which branching occured. 
  if (event[iNew[0]].status() != -41) {
    swap( iNew[0], iNew[1]);   
    swap( iOld[0], iOld[1]);   
  }

  // Loop over all dipole ends belonging to the system.
  for (int iDip = 0; iDip < int(dipEnd.size()); ++iDip) 
  if (dipEnd[iDip].system == iSys) {
    TimeDipoleEnd& dipNow = dipEnd[iDip];

    // Replace radiator (always in final state so simple).
    for (int i = 2; i < size; ++i) 
    if (dipNow.iRadiator == iOld[i]) { 
      dipNow.iRadiator = iNew[i];
      break;
    }

    // Replace ME partner (always in final state, if exists, so simple).
    for (int i = 2; i < size; ++i) 
    if (dipNow.iMEpartner == iOld[i]) { 
      dipNow.iMEpartner = iNew[i];
      break;
    }

    // Recoiler: by default pick old one, only moved. Note excluded beam.
    int iRec = 0;
    for (int i = 1; i < size; ++i) 
    if (dipNow.iRecoiler == iOld[i]) { 
      iRec = iNew[i];
      break;
    }

    // QCD recoiler: check if colour hooks up with new final parton.
    if ( dipNow.colType > 0 
      && event[dipNow.iRadiator].col() == event[iNewNew].acol() ) { 
      iRec = iNewNew;
      dipNow.isrType = 0;
    }
    if ( dipNow.colType < 0 
      && event[dipNow.iRadiator].acol() == event[iNewNew].col() ) { 
      iRec = iNewNew;
      dipNow.isrType = 0;
    }
      
    // QCD recoiler: check if colour hooks up with new beam parton.
    if ( iRec == 0 && dipNow.colType > 0 
      && event[dipNow.iRadiator].col() == event[iNew[0]].col() )
      iRec = iNew[0];
    if ( iRec == 0 && dipNow.colType < 0 
      && event[dipNow.iRadiator].acol() == event[iNew[0]].acol() ) 
      iRec = iNew[0];

    // QED/photon recoiler: either to new particle or remains to beam.
    if ( iRec == 0 && (dipNow.chgType != 0 || dipNow.gamType != 0) ) {
      if ( event[iNew[0]].chargeType() == 0 ) {
        iRec = iNewNew;
        dipNow.isrType = 0;
      } else {
        iRec = iNew[0];
      }
    }

    // Done. Kill dipole if failed to find new recoiler. 
    dipNow.iRecoiler = iRec;
    if (iRec == 0) {
      dipNow.colType = 0;
      dipNow.chgType = 0;
      dipNow.gamType = 0; 
      infoPtr->errorMsg("Error in TimeShower::update: "
      "failed to locate new recoiling partner");
    } 
  }
  
  // Find new dipole end formed by colour index.
  int colTag = event[iNewNew].col();     
  if (doQCDshower && colTag > 0) setupQCDdip( iSys, size, colTag, 1, event); 
  
  // Find new dipole end formed by anticolour index.
  int acolTag = event[iNewNew].acol();     
  if (doQCDshower && acolTag > 0) setupQCDdip( iSys, size, acolTag, -1, event); 

  // Find new "charge-dipole" and "photon-dipole" ends. 
  int  chgType  = event[iNewNew].chargeType();  
  bool doChgDip = (chgType != 0) 
                  && ( ( doQEDshowerByQ && event[iNewNew].isQuark()  )
                    || ( doQEDshowerByL && event[iNewNew].isLepton() ) );
  int  gamType  = (event[iNewNew].id() == 22) ? 1 : 0;
  bool doGamDip = (gamType == 1) && doQEDshowerByGamma;
  if (doChgDip || doGamDip) setupQEDdip( iSys, size, chgType, gamType, event); 

}

//*********

// Setup a dipole end for a QCD colour charge.

void TimeShower::setupQCDdip( int iSys, int i, int colTag, int colSign, 
  Event& event, bool isOctetOnium) {
 
  // Initial values. Find if allowed to hook up beams.
  int iRad = event.getInSystem( iSys, i);
  int iRec = 0;
  int size = event.sizeSystem( iSys);
  int jMin = ( allowBeamRecoil && (event.getInSystem( iSys, 0) > 0)
    && (event.getInSystem( iSys, 1) > 0) ) ? 0 : 2;

  // Colour: other end by same index in beam or opposite in final state.
  if (colSign > 0) 
  for (int j = jMin; j < size; ++j) if (j != i) {
    int iRecNow = event.getInSystem( iSys, j);
    if ( (j < 2 && event[iRecNow].col()  == colTag)
      || (i > 1 && event[iRecNow].acol() == colTag) ) { 
      iRec = iRecNow;
      break;
    }
  }

  // Anticolour: other end by same index in beam or opposite in final state.
  if (colSign < 0) 
  for (int j = jMin; j < size; ++j) if (j != i) {
    int iRecNow = event.getInSystem( iSys, j);
    if ( (j < 2 && event[iRecNow].acol()  == colTag)
      || (i > 1 && event[iRecNow].col() == colTag) ) { 
      iRec = iRecNow;
      break;
    }
  }

  // If fail, then other end to nearest recoiler in final state,
  // by (p_i + p_j)^2 - (m_i + m_j)^2 = 2 (p_i p_j - m_i m_j).
  if (iRec == 0) {
    double ppMin = LARGEM2; 
    for (int j = 2; j < size; ++j) if (j != i) { 
      int iRecNow = event.getInSystem( iSys, j);
      double ppNow = event[iRecNow].p() * event[iRad].p() 
                   - event[iRecNow].m() * event[iRad].m();
      if (ppNow < ppMin) {
        iRec  = iRecNow;
        ppMin = ppNow;
      } 
    }     
  }

  // Store dipole colour end.
  if (iRec > 0) { 
    double pTmax = event[iRad].scale();
    if (iSys == 0) pTmax *= pTmaxFudge;
    int colType  = (event[iRad].id() == 21) ? 2 * colSign : colSign;
    int isrType  = (event[iRec].isFinal()) ? 0 : event[iRec].mother1();
    dipEnd.push_back( TimeDipoleEnd( iRad, iRec, pTmax, 
      colType, 0, 0, isrType, iSys, -1, -1, isOctetOnium) );
  }

}

//*********

// Setup a dipole end for a QED colour charge or a photon.

void TimeShower::setupQEDdip( int iSys, int i, int chgType, int gamType, 
  Event& event) {
 
  // Initial values. Find if allowed to hook up beams.
  int iRad = event.getInSystem( iSys, i);
  int iRec = 0;
  int size = event.sizeSystem( iSys);
  int jMin = ( allowBeamRecoil && (event.getInSystem( iSys, 0) > 0)
    && (event.getInSystem( iSys, 1) > 0) ) ? 0 : 2;

  // Find nearest recoiler, charge-squared-weighted 
  // (p_i + p_j)^2 - (m_i + m_j)^2 = 2 (p_i p_j - m_i m_j).
  double ppMin = LARGEM2; 
  for (int j = jMin; j < size; ++j) if (j != i) {
    int iRecNow = event.getInSystem( iSys, j);
    int chgTypeRecNow = event[iRecNow].chargeType(); 
    if (chgTypeRecNow != 0) {
      double ppNow = (event[iRecNow].p() * event[iRad].p() 
                   -  event[iRecNow].m() * event[iRad].m()) 
                   / pow2(chgTypeRecNow);
      if (ppNow < ppMin) {
        iRec  = iRecNow;
        ppMin = ppNow;
      } 
    }     
  }

  // If fail find any nearest recoiler in final state.
  if (iRec == 0) 
  for (int j = 2; j < size; ++j) if (j != i) {
    int iRecNow = event.getInSystem( iSys, j);
    double ppNow = event[iRecNow].p() * event[iRad].p() 
                 - event[iRecNow].m() * event[iRad].m(); 
    if (ppNow < ppMin) {
      iRec  = iRecNow;
      ppMin = ppNow;
    }     
  }
 
  // Fill charge-dipole or photon-dipole end.
  if (iRec > 0) { 
    double pTmax = event[iRad].scale();
    if (iSys == 0) pTmax *= pTmaxFudge;
    int isrType = (event[iRec].isFinal()) ? 0 : event[iRec].mother1();
    dipEnd.push_back( TimeDipoleEnd(iRad, iRec, pTmax, 
      0, chgType, gamType, isrType, iSys, -1) );
  }

}

//*********

// Select next pT in downwards evolution of the existing dipoles.

double TimeShower::pTnext( Event& event, double pTbegAll, double pTendAll) {

  // Begin loop over all possible radiating dipole ends.
  dipSel = 0;
  double pT2sel = pTendAll * pTendAll;
  for (int iDip = 0; iDip < int(dipEnd.size()); ++iDip) {
    TimeDipoleEnd& dip = dipEnd[iDip]; 
   
    // Dipole properties. (Could partly be moved up to prepare??)
    dip.mRad  = event[dip.iRadiator].m(); 
    dip.m2Rad = pow2(dip.mRad);
    dip.mRec  = event[dip.iRecoiler].m(); 
    dip.m2Rec = pow2(dip.mRec);
    dip.mDip  = m( event[dip.iRadiator], event[dip.iRecoiler] );
    dip.m2Dip = pow2(dip.mDip);

    // Find maximum evolution scale for dipole.
    dip.m2DipCorr    = pow2(dip.mDip - dip.mRec) - dip.m2Rad; 
    double pTbegDip  = min( pTbegAll, dip.pTmax ); 
    double pT2begDip = min( pow2(pTbegDip), 0.25 * dip.m2DipCorr);

    // Do QCD or QED evolution if it makes sense.
    dip.pT2 = 0.;
    if (pT2begDip > pT2sel) {
      if      (dip.colType != 0) 
        pT2nextQCD(pT2begDip, pT2sel, dip, event);
      else if (dip.chgType != 0 || dip.gamType != 0)                 
        pT2nextQED(pT2begDip, pT2sel, dip, event);

      // Update if found larger pT than current maximum.
      if (dip.pT2 > pT2sel) {
        pT2sel = dip.pT2;
        dipSel = &dip;
      }
    } 
  } 

  // Return nonvanishing value if found pT bigger than already found.
  return (dipSel == 0) ? 0. : sqrt(pT2sel); 

}

//*********

// Evolve a QCD dipole end. 

void TimeShower::pT2nextQCD(double pT2begDip, double pT2sel, 
  TimeDipoleEnd& dip, Event& event) { 

  // Lower cut for evolution. Return if no evolution range.
  double pT2endDip = max( pT2sel, pT2colCut );   
  if (pT2begDip < pT2endDip) return;   

  // Upper estimate for matrix element weighting and colour factor.
  // Special cases for triplet recoiling against gluino and octet onia.
  // Note that g -> g g and g -> q qbar are split on two sides.
  int    colTypeAbs = abs(dip.colType);
  double wtPSglue   = 2.;
  double colFac     = (colTypeAbs == 1) ? 4./3. : 3./2.;
  if (dip.MEgluinoRec)  colFac  = 3.;  
  if (dip.isOctetOnium) colFac *= 0.5 * octetOniumColFac;
  double wtPSqqbar  = (colTypeAbs == 2) ? 0.25 * nGluonToQuark : 0.;
  
  // Variables used inside evolution loop. (Mainly dummy start values.)
  dip.pT2              = pT2begDip;
  int    nFlavour      = 3;
  double zMinAbs       = 0.5;
  double pT2min        = pT2endDip;
  double b0            = 4.5;
  double Lambda2       = Lambda3flav2; 
  double emitCoefGlue  = 0.;
  double emitCoefQqbar = 0.; 
  double emitCoefTot   = 0.; 
  double wt            = 0.; 
  bool   mustFindRange = true;
  
  // Begin evolution loop towards smaller pT values.
  do { 

    // Initialize evolution coefficients at the beginning and
    // reinitialize when crossing c and b flavour thresholds.
    if (mustFindRange) {

      // Determine overestimated z range; switch at c and b masses.
      if (dip.pT2 > m2b) {
        nFlavour = 5;
        pT2min   = m2b;
        b0       = 23./6.;
        Lambda2  = Lambda5flav2;
      } else if (dip.pT2 > m2c) {
        nFlavour  = 4;
        pT2min    = m2c;
        b0        = 25./6.;
        Lambda2   = Lambda4flav2;
      } else { 
        nFlavour  = 3;
        pT2min    = pT2endDip;
        b0        = 27./6.;
        Lambda2   = Lambda3flav2;
      }
      zMinAbs = 0.5 - sqrtpos( 0.25 - pT2min / dip.m2DipCorr );
      if (zMinAbs < SIMPLIFYROOT) zMinAbs = pT2min / dip.m2DipCorr;

      // Find emission coefficients for X -> X g and g -> q qbar.
      emitCoefGlue = wtPSglue * colFac * log(1. / zMinAbs - 1.);
      emitCoefTot  = emitCoefGlue;
      if (colTypeAbs == 2 && event[dip.iRadiator].id() == 21) {
        emitCoefQqbar = wtPSqqbar * (1. - 2. * zMinAbs);
        emitCoefTot  += emitCoefQqbar;
      }

      // Initialization done for current range.
      mustFindRange = false;
    } 

    // Pick pT2 (in overestimated z range) for fixed alpha_strong.
    if (alphaSorder == 0) {
      dip.pT2 = dip.pT2 * pow( Rndm::flat(), 
        1. / (alphaS2pi * emitCoefTot) );

    // Ditto for first-order alpha_strong.
    } else if (alphaSorder == 1) {
      dip.pT2 = Lambda2 * pow( dip.pT2 / Lambda2, 
        pow( Rndm::flat(), b0 / emitCoefTot) );

      // For second order reject by second term in alpha_strong expression.
    } else {
      do dip.pT2 = Lambda2 * pow( dip.pT2 / Lambda2, 
        pow( Rndm::flat(), b0 / emitCoefTot) );
      while (alphaS.alphaS2OrdCorr(dip.pT2) < Rndm::flat() 
        && dip.pT2 > pT2min);
    }
    wt = 0.;
  
    // If crossed c or b thresholds: continue evolution from threshold.
    if (nFlavour == 5 && dip.pT2 < m2b) {  
      mustFindRange = true;
      dip.pT2       = m2b;
    } else if ( nFlavour == 4 && dip.pT2 < m2c) { 
      mustFindRange = true;
      dip.pT2       = m2c;

    // Abort evolution if below cutoff scale, or below another branching.
    } else {
      if ( dip.pT2 < pT2endDip) { dip.pT2 = 0.; return; }

      // Pick kind of branching: X -> X g or g -> q qbar. 
      dip.flavour  = 21;
      dip.mFlavour = 0.;
      if (colTypeAbs == 2 && emitCoefQqbar > Rndm::flat() 
        * emitCoefTot) dip.flavour = 0; 

      // Pick z: either dz/(1-z) or flat dz.
      if (dip.flavour == 21) {
        dip.z = 1. - zMinAbs * pow( 1. / zMinAbs - 1., Rndm::flat() );
      } else { 
        dip.z = zMinAbs + (1. - 2. * zMinAbs) * Rndm::flat();   
      }
  
      // Do not accept branching if outside allowed z range.
      double zMin = 0.5 - sqrtpos( 0.25 - dip.pT2 / dip.m2DipCorr ); 
      if (zMin < SIMPLIFYROOT) zMin = dip.pT2 / dip.m2DipCorr;
      dip.m2 = dip.m2Rad + dip.pT2 / (dip.z * (1. - dip.z));
      if (dip.z > zMin && dip.z < 1. - zMin 
        && dip.m2 * dip.m2Dip < dip.z * (1. - dip.z) 
        * pow2(dip.m2Dip + dip.m2 - dip.m2Rec) ) {

        // Flavour choice for g -> q qbar.
        if (dip.flavour == 0) {
          dip.flavour  = min(5, 1 + int(nGluonToQuark * Rndm::flat())); 
          dip.mFlavour = ParticleDataTable::m0(dip.flavour);
	}

        // No z weight, except threshold, if to do ME corrections later on.
        if (dip.MEtype > 0) { 
          wt = 1.;
          if (dip.flavour < 10 && dip.m2 < THRESHM2 * pow2(dip.mFlavour))
            wt = 0.; 

        // z weight for X -> X g.
        } else if (dip.flavour == 21 && colTypeAbs == 1) {
          wt = (1. + pow2(dip.z)) / wtPSglue;
	} else if (dip.flavour == 21) {     
          wt = (1. + pow3(dip.z)) / wtPSglue;
           
        // z weight for g -> q qbar.
        } else {
          double beta  = sqrtpos( 1. - 4. * pow2(dip.mFlavour) / dip.m2 );
          wt = beta * ( pow2(dip.z) + pow2(1. - dip.z) );
        }

        // For dipole to beam remnant reduce by PDF ratio (approximate!??).
        if (dip.isrType != 0) {
          BeamParticle& beam = (dip.isrType == 1) ? *beamAPtr : *beamBPtr;
          int iSys    = dip.system;
          double xOld = beam[iSys].x();
          double xNew = xOld * (1. + (dip.m2 - dip.m2Rad) / 
            (dip.m2Dip - dip.m2Rad));
          if (xNew > beam.xMax(iSys)) wt = 0.;              
          else {
            int idRec     = event[dip.iRecoiler].id();
            double pdfOld = max ( TINYPDF, 
                            beam.xfISR( iSys, idRec, xOld, dip.pT2) ); 
            double pdfNew = beam.xfISR( iSys, idRec, xNew, dip.pT2); 
            wt *= min( 1., pdfNew / pdfOld); 
	  }
        }
      }
    }

  // Iterate until acceptable pT (or have fallen below pTmin).
  } while (wt < Rndm::flat());

}

//*********

// Evolve a QED dipole end, either charged or photon. 

void TimeShower::pT2nextQED(double pT2begDip, double pT2sel, 
  TimeDipoleEnd& dip, Event& event) { 

  // Lower cut for evolution. Return if no evolution range.
  double pT2chgCut = (dip.chgType != 0 && abs(dip.chgType) != 3)
    ? pT2chgQCut : pT2chgLCut;
  double pT2endDip = max( pT2sel, pT2chgCut ); 
  if (pT2begDip < pT2endDip) return;   

  // Emission of photon or photon branching.
  bool hasCharge = (dip.chgType != 0);

  // Default values.
  double wtPSgam     = 0.;
  double chg2Sum     = 0.;
  double chg2SumL    = 0.;
  double chg2SumQ    = 0.;
  double zMinAbs     = 0.; 
  double emitCoefTot = 0.;

  // alpha_em at maximum scale provides upper estimate.
  double alphaEMmax  = alphaEM.alphaEM(pT2begDip);
  double alphaEM2pi  = alphaEMmax / (2. * M_PI);

  // Emission: upper estimate for matrix element weighting; charge factor.
  if (hasCharge) {
    wtPSgam     = 2.;
    double chg2 = pow2(dip.chgType / 3.);

    // Determine overestimated z range. Find evolution coefficient.
    zMinAbs = 0.5 - sqrtpos( 0.25 - pT2endDip / dip.m2DipCorr );
    if (zMinAbs < SIMPLIFYROOT) zMinAbs = pT2endDip / dip.m2DipCorr;
    emitCoefTot = alphaEM2pi * chg2 * wtPSgam * log(1. / zMinAbs - 1.);

  // Branching: sum of squared charge factors for lepton and quark daughters.
  } else { 
    chg2SumL = max(0, min(3, nGammaToLepton));
    if      (nGammaToQuark > 4) chg2SumQ = 11. / 9.;
    else if (nGammaToQuark > 3) chg2SumQ = 10. / 9.;
    else if (nGammaToQuark > 2) chg2SumQ =  6. / 9.;
    else if (nGammaToQuark > 1) chg2SumQ =  5. / 9.;
    else if (nGammaToQuark > 0) chg2SumQ =  1. / 9.;

    // Total sum of squared charge factors. Find evolution coefficient. 
    chg2Sum     = chg2SumL + 3. * chg2SumQ; 
    emitCoefTot = alphaEM2pi * chg2Sum;
  }
  
  // Variables used inside evolution loop.
  dip.pT2 = pT2begDip;
  double wt; 
  
  // Begin evolution loop towards smaller pT values.
  do { 
 
    // Pick pT2 (in overestimated z range).
    dip.pT2 = dip.pT2 * pow(Rndm::flat(), 1. / emitCoefTot);
    wt = 0.;

    // Abort evolution if below cutoff scale, or below another branching.
    if ( dip.pT2 < pT2endDip) { dip.pT2 = 0.; return; }

    // Pick z according to dz/(1-z) or flat.
    if (hasCharge) dip.z = 1. - zMinAbs 
      * pow( 1. / zMinAbs - 1., Rndm::flat() );
    else           dip.z = Rndm::flat();
  
    // Do not accept branching if outside allowed z range.
    double zMin = 0.5 - sqrtpos( 0.25 - dip.pT2 / dip.m2DipCorr ); 
    if (zMin < SIMPLIFYROOT) zMin = dip.pT2 / dip.m2DipCorr;
    dip.m2 = dip.m2Rad + dip.pT2 / (dip.z * (1. - dip.z));
    if (dip.z > zMin && dip.z < 1. - zMin 
      && dip.m2 * dip.m2Dip < dip.z * (1. - dip.z) 
        * pow2(dip.m2Dip + dip.m2 - dip.m2Rec) 
      // For gamma -> f fbar also impose maximum mass.
      && (hasCharge || dip.m2 < m2MaxGamma) ) {

      // Photon emission: unique flavour choice.
      if (hasCharge) {
        dip.flavour = 22;
        dip.mFlavour = 0.;

      // Photon branching: either lepton or quark flavour choice.   
      } else {
        if (Rndm::flat() * chg2Sum < chg2SumL)  
          dip.flavour  = 9 + 2 * min(3, 1 + int(chg2SumL * Rndm::flat()));
        else { 
          double rndmQ = 9. * chg2SumQ * Rndm::flat();
          if      (rndmQ <  1.) dip.flavour = 1;
          else if (rndmQ <  5.) dip.flavour = 2;
          else if (rndmQ <  6.) dip.flavour = 3;
          else if (rndmQ < 10.) dip.flavour = 4;
          else                  dip.flavour = 5;
        }
        dip.mFlavour = ParticleDataTable::m0(dip.flavour);
      }
                      

      // No z weight, except threshold, if to do ME corrections later on.
      if (dip.MEtype > 0) { 
        wt = 1.;
        if (dip.flavour < 20 && dip.m2 < THRESHM2 * pow2(dip.mFlavour))
          wt = 0.; 

      // z weight for X -> X gamma.
      } else if (hasCharge) {
        wt = (1. + pow2(dip.z)) / wtPSgam;

      // z weight for gamma -> f fbar.
      } else {
        double beta  = sqrtpos( 1. - 4. * pow2(dip.mFlavour) / dip.m2 );
        wt = beta * ( pow2(dip.z) + pow2(1. - dip.z) );
      }

      // Correct to current value of alpha_EM.
      double alphaEMnow = alphaEM.alphaEM(dip.pT2);
      wt *= (alphaEMnow / alphaEMmax);

      // For dipole to beam remnant reduce by PDF ratio (approximate!??).
      if (dip.isrType != 0) {
        BeamParticle& beam = (dip.isrType == 1) ? *beamAPtr : *beamBPtr;
        int iSys    = dip.system;
        double xOld = beam[iSys].x();
        double xNew = xOld * (1. + (dip.m2 - dip.m2Rad) / 
          (dip.m2Dip - dip.m2Rad));
        if (xNew > beam.xMax(iSys)) wt = 0.;
        else {
          int idRec     = event[dip.iRecoiler].id();
          double pdfOld = max ( TINYPDF, 
                          beam.xfISR( iSys, idRec, xOld, dip.pT2) ); 
          double pdfNew = beam.xfISR( iSys, idRec, xNew, dip.pT2); 
          wt *= min( 1., pdfNew / pdfOld); 
        }
      }
    }

  // Iterate until acceptable pT (or have fallen below pTmin).
  } while (wt < Rndm::flat());  

}

//*********

// ME corrections and kinematics that may give failure.
// Notation: radBef, recBef = radiator, recoiler before emission,
//           rad, rec, emt = radiator, recoiler, emitted efter emission.
//           (rad, emt distinguished by colour flow for g -> q qbar.) 

bool TimeShower::branch( Event& event) {

  // Find initial particles in dipole branching.
  int iRadBef      = dipSel->iRadiator;
  int iRecBef      = dipSel->iRecoiler;
  Particle& radBef = event[iRadBef]; 
  Particle& recBef = event[iRecBef];

  // Default flavours and colour tags for new particles in dipole branching. 
  int idRad   = radBef.id();
  int idEmt   = dipSel->flavour; 
  int colRad  = radBef.col();
  int acolRad = radBef.acol();
  int colEmt  = 0;
  int acolEmt = 0;
  iSysSel     = dipSel->system;

  // Default OK for photon emission.
  if (dipSel->flavour == 22) { 
  // New colour tag required for gluon emission.
  } else if (dipSel->flavour == 21 && dipSel->colType > 0) { 
    colEmt  = colRad;  
    colRad  = event.nextColTag();   
    acolEmt = colRad;
  } else if (dipSel->flavour == 21) { 
    acolEmt = acolRad;  
    acolRad = event.nextColTag();   
    colEmt  = acolRad;
  // New flavours for g -> q qbar; split colours.
  } else if (dipSel->colType > 0) {
    idEmt   = dipSel->flavour ;
    idRad   = -idEmt;
    colEmt  = colRad;
    colRad  = 0; 
  } else if (dipSel->colType < 0) {
    idEmt   = -dipSel->flavour ;
    idRad   = -idEmt;
    acolEmt = acolRad;
    acolRad = 0; 
  // New flavours for gamma -> f fbar, and maybe also colours.
  } else if (dipSel->gamType == 1 && Rndm::flat() > 0.5) {   
    idEmt   = -dipSel->flavour ;
    idRad   = -idEmt;
    if (idRad < 10) colRad = event.nextColTag(); 
    acolEmt = colRad;
  } else if (dipSel->gamType == 1) {   
    idEmt   = dipSel->flavour ;
    idRad   = -idEmt;
    if (idEmt < 10) colEmt = event.nextColTag(); 
    acolRad = colEmt;
  }

  // Construct kinematics in dipole rest frame: 
  // begin simple (like g -> g g).
  double eRadPlusEmt = 0.5 * (dipSel->m2Dip + dipSel->m2 - dipSel->m2Rec) 
    / dipSel->mDip;
  double e2RadPlusEmt = pow2(eRadPlusEmt);
  double pzRadPlusEmt = 0.5 * sqrtpos( pow2(dipSel->m2Dip - dipSel->m2 
    - dipSel->m2Rec) - 4. * dipSel->m2 * dipSel->m2Rec ) / dipSel->mDip;
  double pT2corr = dipSel->m2 * (e2RadPlusEmt * dipSel->z * (1. - dipSel->z)
    - 0.25 * dipSel->m2) / pow2(pzRadPlusEmt);
  double pTcorr = sqrtpos( pT2corr );
  double pzRad = (e2RadPlusEmt * dipSel->z - 0.5 * dipSel->m2) 
    / pzRadPlusEmt;
  double pzEmt = (e2RadPlusEmt * (1. - dipSel->z) - 0.5 * dipSel->m2) 
    / pzRadPlusEmt;
  double mRad = dipSel->mRad;
  double mEmt = 0.;

  // Kinematics reduction for q -> q g or q -> q gamma when m_q > 0. 
  if (abs(dipSel->colType) == 1 || dipSel->chgType != 0) { 
    pTcorr *= 1. - dipSel->m2Rad / dipSel->m2; 
    pzRad += pzEmt * dipSel->m2Rad / dipSel->m2;
    pzEmt *= 1. - dipSel->m2Rad / dipSel->m2;  
  // Kinematics reduction for g -> q qbar or gamma -> f fbar when m_f > 0;
  } else if (abs(dipSel->flavour) < 20) {
    mEmt = dipSel->mFlavour;
    mRad = mEmt;
    double beta = sqrtpos( 1. - 4. * pow2(mEmt) / dipSel->m2 );   
    pTcorr *= beta;
    pzRad = 0.5 * ( (1. + beta) * pzRad + (1. - beta) * pzEmt );
    pzEmt = pzRadPlusEmt - pzRad;
  } 

  // Find rest frame and angles of original dipole.
  RotBstMatrix M;
  M.fromCMframe(radBef.p(), recBef.p());

  // Evaluate coefficient of azimuthal asymmetry from gluon polarization.
  findAsymPol( event, dipSel);

  // Begin construction of new dipole kinematics: pick azimuthal angle.
  Vec4 pRad, pEmt, pRec;
  double wtPhi = 1.;
  do { 
    double phi = 2. * M_PI * Rndm::flat();

    // Define kinematics of branching in dipole rest frame.
    pRad = Vec4( pTcorr * cos(phi), pTcorr * sin(phi), pzRad, 
      sqrt( pow2(pTcorr) + pow2(pzRad) + pow2(mRad) ) );
    pEmt = Vec4( -pRad.px(), -pRad.py(), pzEmt,
      sqrt( pow2(pTcorr) + pow2(pzEmt) + pow2(mEmt) ) );
    pRec = Vec4( 0., 0., -pzRadPlusEmt, sqrt( pow2(pzRadPlusEmt) 
      + dipSel->m2Rec ) );

    // Rotate and boost dipole products to the event frame.
    pRad.rotbst(M);
    pEmt.rotbst(M);
    pRec.rotbst(M);

    // Azimuthal phi weighting: loop to new phi value if required.
    if (dipSel->asymPol != 0.) {
      Vec4 pRadBef = event[iRadBef].p();
      Vec4 pAunt = event[dipSel->iAunt].p();
      double cosPhi = cosphi( pRad, pAunt, pRadBef );
      wtPhi = ( 1. + dipSel->asymPol * (2. * pow2(cosPhi) - 1.) )
        / ( 1. + abs(dipSel->asymPol) );
    } 
  } while (wtPhi < Rndm::flat()) ;

  // Kinematics when recoiler is initial-state parton.
  int isrTypeNow = dipSel->isrType;
  if (isrTypeNow != 0) pRec = 2. * recBef.p() - pRec;

  // Define new particles from dipole branching.
  double pTsel = sqrt(dipSel->pT2);
  Particle rad = Particle(idRad, 51, iRadBef, 0, 0, 0, 
    colRad, acolRad, pRad, mRad, pTsel); 
  Particle emt = Particle(idEmt, 51, iRadBef, 0, 0, 0,
    colEmt, acolEmt, pEmt, mEmt, pTsel);

  // Recoiler either in final or in initial state
  Particle rec = (isrTypeNow == 0)
    ? Particle(recBef.id(),  52, iRecBef, iRecBef, 0, 0, 
      recBef.col(), recBef.acol(), pRec, dipSel->mRec, pTsel) 
    : Particle(recBef.id(), -53, 0, 0, iRecBef, iRecBef, 
      recBef.col(), recBef.acol(), pRec, 0., 0.); 

  // ME corrections can lead to branching being rejected.
  if (dipSel->MEtype > 0) {
    Particle& partner = (dipSel->iMEpartner == iRecBef) 
      ? rec : event[dipSel->iMEpartner];
    if ( findMEcorr( dipSel, rad, partner, emt) < Rndm::flat() ) 
      return false;
  }

  // Put new particles into the event record.
  int iRad = event.append(rad);
  int iEmt = event.append(emt);
  int iRec = event.append(rec);

  // Mark original dipole partons as branched and set daughters/mothers.
  event[dipSel->iRadiator].statusNeg();
  event[dipSel->iRadiator].daughters( iRad, iEmt); 
  if (isrTypeNow == 0) {
    event[dipSel->iRecoiler].statusNeg();
    event[dipSel->iRecoiler].daughters( iRec, iRec);
  } else {
    int mother1 = event[dipSel->iRecoiler].mother1();  
    int mother2 = event[dipSel->iRecoiler].mother2();  
    event[dipSel->iRecoiler].mothers( iRec, iRec);
    event[iRec].mothers( mother1, mother2);  
    if (mother1 == 1) event[1].daughter1( iRec);  
    if (mother1 == 2) event[2].daughter1( iRec);  
    // For initial-state recoiler also update beam info.
    BeamParticle& beamRec = (mother1 == 1) ? *beamAPtr : *beamBPtr;
    double xRec = pRec.e() / beamRec.e(); 
    beamRec[iSysSel].iPos( iRec);
    beamRec[iSysSel].x( xRec); 
  }

  // Photon emission: update to new dipole ends; add new photon "dipole".
  if (dipSel->flavour == 22) { 
    dipSel->iRadiator = iRad;
    dipSel->iRecoiler = iRec;
    dipSel->pTmax = pTsel;
    if (doQEDshowerByGamma) dipEnd.push_back( TimeDipoleEnd(iEmt, iRad, 
      pTsel, 0, 0, 1, 0, iSysSel, 0));
 
  // Gluon emission: update both dipole ends and add two new ones.
  } else if (dipSel->flavour == 21) { 
    dipSel->iRadiator = iRad;
    dipSel->iRecoiler = iEmt;
    dipSel->isrType   = 0;
    dipSel->pTmax     = pTsel;
    for (int i = 0; i < int(dipEnd.size()); ++i) {
      if (dipEnd[i].iRadiator == iRecBef && dipEnd[i].iRecoiler == iRadBef 
        && dipEnd[i].colType != 0) {
        dipEnd[i].iRadiator = iRec;
        dipEnd[i].iRecoiler = iEmt;
        // Strive to match colour to anticolour inside closed system.
        if (dipEnd[i].colType * dipSel->colType > 0) 
          dipEnd[i].iRecoiler = iRad;
        dipEnd[i].pTmax = pTsel;
      }
    }
    int colType = (dipSel->colType > 0) ? 2 : -2 ;
    dipEnd.push_back( TimeDipoleEnd(iEmt, iRec, pTsel,  
       colType, 0, 0, isrTypeNow, iSysSel, 0));
    dipEnd.push_back( TimeDipoleEnd(iEmt, iRad, pTsel, 
      -colType, 0, 0, 0, iSysSel, 0));

  // Gluon branching to q qbar: update current dipole and other of gluon.
  } else if (dipSel->colType != 0) {
    for (int i = 0; i < int(dipEnd.size()); ++i) {
      if (dipEnd[i].iRadiator == iRadBef && abs(dipEnd[i].colType) == 2) {
        dipEnd[i].colType /= 2;
        // Note: gluino -> quark + squark gives a deeper radiation dip than
        // the more obvious alternative photon decay, so is more realistic.
        dipEnd[i].MEtype = 66;
        if (&dipEnd[i] == dipSel) dipEnd[i].iMEpartner = iRad;
        else                      dipEnd[i].iMEpartner = iEmt;
      }
      // Strive to match colour to anticolour inside closed system.
      if ( dipEnd[i].iRecoiler == iRadBef 
        && dipEnd[i].colType * dipSel->colType < 0 ) 
        dipEnd[i].iRecoiler = iEmt;
    }
    dipSel->iRadiator = iEmt;
    dipSel->iRecoiler = iRec;
    dipSel->pTmax     = pTsel;

    // Gluon branching to q qbar: also add two charge dipole ends.
    // Note: gluino -> quark + squark gives a deeper radiation dip than
    // the more obvious alternative photon decay, so is more realistic.
    if (doQEDshowerByQ) {
      int chgType = event[iRad].chargeType(); 
      dipEnd.push_back( TimeDipoleEnd(iRad, iEmt, pTsel, 
        0,  chgType, 0, 0, iSysSel, 66, iEmt));
      dipEnd.push_back( TimeDipoleEnd(iEmt, iRad, pTsel, 
        0, -chgType, 0, 0, iSysSel, 66, iRad));
    }

  // Photon branching to f fbar: inactivate photon "dipole";
  // optionally add new charge and colour dipole ends. 
  } else if (dipSel->gamType != 0) {
    dipSel->gamType = 0;
    int chgType = event[iRad].chargeType(); 
    int colType = event[iRad].colType();
    // MEtype = 102 for charge in vector decay.
    if ( chgType != 0 && ( ( doQEDshowerByQ && colType != 0 )  
      || ( doQEDshowerByL && colType == 0 ) ) ) { 
      dipEnd.push_back( TimeDipoleEnd(iRad, iEmt, pTsel, 
        0,  chgType, 0, 0, iSysSel, 102, iEmt));
      dipEnd.push_back( TimeDipoleEnd(iEmt, iRad, pTsel, 
        0, -chgType, 0, 0, iSysSel, 102, iRad));
    }
    // MEtype = 11 for colour in vector decay.
    if (colType != 0 && doQCDshower) {
      dipEnd.push_back( TimeDipoleEnd(iRad, iEmt, pTsel, 
         colType, 0, 0, 0, iSysSel, 11, iEmt));
      dipEnd.push_back( TimeDipoleEnd(iEmt, iRad, pTsel, 
        -colType, 0, 0, 0, iSysSel, 11, iRad));
    }
  }

  // Now update other dipoles that also involved the radiator or recoiler.
  for (int i = 0; i < int(dipEnd.size()); ++i) {
    if (dipEnd[i].iRadiator  == iRadBef) dipEnd[i].iRadiator  = iRad;
    if (dipEnd[i].iRadiator  == iRecBef) dipEnd[i].iRadiator  = iRec;
    if (dipEnd[i].iRecoiler  == iRadBef) dipEnd[i].iRecoiler  = iRad;
    if (dipEnd[i].iRecoiler  == iRecBef) dipEnd[i].iRecoiler  = iRec;
    if (dipEnd[i].iMEpartner == iRadBef) dipEnd[i].iMEpartner = iRad;
    if (dipEnd[i].iMEpartner == iRecBef) dipEnd[i].iMEpartner = iRec;
  }

  // Finally update the list of all partons in all systems.
  event.replaceInSystem(iSysSel, iRadBef, iRad);  
  event.addToSystem(iSysSel, iEmt);
  event.replaceInSystem(iSysSel, iRecBef, iRec); 

  // Done. 
  return true;

}

//*********

// Find class of QCD ME correction.
// MEtype classification follow codes in Norrbin article,
// additionally -1 = try to find type, 0 = no ME corrections.
// Warning: not yet tried out to do a correct assignment in 
// arbitrary multiparton configurations! ??

void TimeShower::findMEtype( Event& event, TimeDipoleEnd& dip) {

  // Initial value. Mark if no ME corrections to be applied.
  bool setME = true;
  if (!doMEcorrections) setME = false; 

  // No ME corrections in 2 -> n processes.
  int iMother  = event[dip.iRadiator].mother1();
  int iMother2 = event[dip.iRadiator].mother2();
  if (iMother2 != iMother && iMother2 != 0) setME = false;
  if (event[dip.iRecoiler].mother1() != iMother)  setME = false;    
  if (event[dip.iRecoiler].mother2() != iMother2) setME = false;    

  // No ME corrections for recoiler in initial state.
  if (event[dip.iRecoiler].status() < 0) setME = false;  

  // Done if no ME to be set.
  if (!setME) {
    dip.MEtype = 0;
    return;
  } 

  // If no ME partner set, assume it is the recoiler.
  if (dip.iMEpartner < 0) dip.iMEpartner = dip.iRecoiler;

  // Now begin processing of colour dipole.
  if (dip.colType != 0) {

    // Find daughter types (may or may not be used later on).
    int idDau1      = event[dip.iRadiator].id();
    int idDau2      = event[dip.iMEpartner].id();
    int dau1Type    = findMEparticle(idDau1);
    int dau2Type    = findMEparticle(idDau2);
    int minDauType  = min(dau1Type, dau2Type);
    int maxDauType  = max(dau1Type, dau2Type);

    // Reorder dipole ends in kinematics. Split ME expression in two sides.
    dip.MEorder     = (dau2Type >= dau1Type);
    dip.MEsplit     = (maxDauType <= 6); 
    dip.MEgluinoRec = false;
 
    // If type already set (or set not to have) then done.
    if (minDauType == 0 && dip.MEtype < 0) dip.MEtype = 0;
    if (dip.MEtype >= 0) return;
    dip.MEtype = 0;

    // For H -> gg -> ggg we found that DGLAP kernels do better than eikonal.
    if (dau1Type == 4 && dau2Type == 4) return; 

    // Find mother type. 
    int idMother = 0;
    if ( event[dip.iRecoiler].mother1() == iMother && iMother >= 0) 
      idMother = event[iMother].id();
    int motherType = (idMother != 0) ? findMEparticle(idMother) : 0;

    // When a mother if not known then use colour and spin content to guess.
    if (motherType == 0) {
      int col1  = event[dip.iRadiator].col();
      int acol1 = event[dip.iRadiator].acol();
      int col2  = event[dip.iMEpartner].col();
      int acol2 = event[dip.iMEpartner].acol();
      // spinT = 0/1 = integer or half-integer.
      int spinT = ( event[dip.iRadiator].spinType() 
                + event[dip.iMEpartner].spinType() )%2;
      // Colour singlet mother.
      if ( col1 == acol2 && acol1 == col2 ) 
        motherType = (spinT == 0) ? 7 : 9;
      // Colour octet mother.
      else if ( (col1 == acol2 && acol1 != 0 && col2 != 0)
        || (acol1 == col2 && col1 != 0 && acol2 != 0) )
        motherType = (spinT == 0) ? 4 : 5; 
      // Colour triplet mother.
      else if ( (col1 == acol2 && acol1 != col2)  
        || (acol1 == col2 && col1 != acol2) ) 
        motherType = (spinT == 0) ? 2 : 1;
      // If no colours are matched then cannot have common mother, so done.  
      else return;      
    }

    // Now start from default, which is eikonal ME corrections, 
    // and try to find matching ME cases below.
    int MEkind = 0;
    int MEcombi = 4;
    dip.MEmix = 0.5;

    // Triplet recoiling against gluino needs enhanced radiation
    // to match to matrix elements.
    dip.MEgluinoRec = (dau1Type >= 1 && dau1Type <= 3 && dau2Type == 5);

    // Vector/axial vector -> q + qbar.
    if (minDauType == 1 && maxDauType == 1 && 
      (motherType == 4 || motherType == 7) ) {
      MEkind = 2;
      if (idMother == 21 || idMother == 22) MEcombi = 1;
      else if (idMother == 23 || idDau1 + idDau2 == 0) {
        MEcombi = 3; 
        dip.MEmix = gammaZmix( event, iMother, dip.iRadiator, dip.iRecoiler );
      }
      else if (idMother == 24) MEcombi = 4;
    }
    // For chi -> chi q qbar, use V/A -> q qbar as first approximation.
    else if (minDauType == 1 && maxDauType == 1 && motherType == 9)
      MEkind = 2;

    // q -> q + V.
    else if (minDauType == 1 && maxDauType == 7 && motherType == 1) 
      MEkind = 3;
      if (idDau1 == 22 || idDau2 == 22) MEcombi = 1;
 
    // Scalar/pseudoscalar -> q + qbar; q -> q + S.
    else if (minDauType == 1 && maxDauType == 1 && motherType == 8) {
      MEkind = 4;
      if (idMother == 25 || idMother == 35 || idMother == 37) MEcombi = 1;
      else if (idMother == 36) MEcombi = 2;
    } 
    else if (minDauType == 1 && maxDauType == 8 && motherType == 1)
      MEkind = 5;
 
    // V -> ~q + ~qbar; ~q -> ~q + V; S -> ~q + ~qbar; ~q -> ~q + S.
    else if (minDauType == 2 && maxDauType == 2 && (motherType == 4 
      || motherType == 7) ) MEkind = 6;
    else if (minDauType == 2 && (maxDauType == 4 || maxDauType == 7) 
      && motherType == 2) MEkind = 7;
    else if (minDauType == 2 && maxDauType == 2 && motherType == 8)
      MEkind = 8;
    else if (minDauType == 2 && maxDauType == 8 && motherType == 2)
      MEkind = 9;
 
    // chi -> q + ~qbar; ~q -> q + chi; q -> ~q + chi.
    else if (minDauType == 1 && maxDauType == 2 && motherType == 9) 
      MEkind = 10;
    else if (minDauType == 1 && maxDauType == 9 && motherType == 2) 
      MEkind = 11;
    else if (minDauType == 2 && maxDauType == 9 && motherType == 1) 
      MEkind = 12;
 
    // ~g -> q + ~qbar; ~q -> q + ~g; q -> ~q + ~g.
    else if (minDauType == 1 && maxDauType == 2 && motherType == 5)
      MEkind = 13;
    else if (minDauType == 1 && maxDauType == 5 && motherType == 2) 
      MEkind = 14;
    else if (minDauType == 2 && maxDauType == 5 && motherType == 1) 
      MEkind = 15;

    // g (+V, S) -> ~g + ~g (eikonal approximation).
    else if (minDauType == 5 && maxDauType == 5) MEkind = 16;

    // Save ME type and gamma_5 admixture. 
    dip.MEtype = 5 * MEkind + MEcombi; 

  // Now begin processing of charge dipole - still primitive.
  } else if (dip.chgType != 0) {

    // Set defaults for QED case; then possibly done.
    dip.MEorder = true;
    dip.MEsplit = true; 
    if (dip.MEtype >= 0) return;

    // So far only ME corrections for q qbar or l lbar.
    int idDau1 = event[dip.iRadiator].id();
    int idDau2 = event[dip.iMEpartner].id();
    if (abs(idDau1) < 9 && abs(idDau2) < 9 && idDau1 * idDau2 < 0) ;
    else if (abs(idDau1) > 10 && abs(idDau1) < 19 && abs(idDau2) > 10
      && abs(idDau2) < 19 && idDau1 * idDau2 < 0) ;
    else { dip.MEtype = 0; return; }

    // Distinguish charge sum != 0 or = 0; in latter assume vector source.
    dip.MEtype = 101;
    if (idDau1 + idDau2 == 0) dip.MEtype = 102; 
    dip.MEmix = 1.;
  }

}

//*********
 
// Find type of particle for ME type: 0 = unknown, 1 = quark, 2 = squark,
// 3 = spare triplet, 4 = gluon, 5 = gluino, 6 = spare octet, 
// 7 = vector boson, 8 = colourless scalar, 9 = colourless spin 1/2.

int TimeShower::findMEparticle( int id) {

  // find colour and spin of particle.
  int type = 0;
  int colType = abs(ParticleDataTable::colType(id)); 
  int spinType = ParticleDataTable::spinType(id);

  // Find particle type from colour and spin.
  if      (colType == 1 && spinType == 2) type = 1;
  else if (colType == 1 && spinType == 1) type = 2;
  else if (colType == 1)                  type = 3;
  else if (colType == 2 && spinType == 3) type = 4;
  else if (colType == 2 && spinType == 2) type = 5;
  else if (colType == 2)                  type = 6;
  else if (colType == 0 && spinType == 3) type = 7;
  else if (colType == 0 && spinType == 1) type = 8;
  else if (colType == 0 && spinType == 2) type = 9;

  // Done.
  return type;

}  

//*********

// Find mixture of V and A in gamma/Z: energy- and flavour-dependent. 

double TimeShower::gammaZmix( Event& event, int iRes, int iDau1, int iDau2) {

  // Try to identify initial flavours; use e+e- as default.
  int idIn1 = -11;
  int idIn2 = 11;
  int iIn1  = (iRes >= 0) ? event[iRes].mother1() : -1;
  int iIn2  = (iRes >= 0) ? event[iRes].mother2() : -1;
  if (iIn1 >=0) idIn1 = event[iIn1].id();
  if (iIn2 >=0) idIn2 = event[iIn1].id();
         
  // In processes f + g/gamma -> f + Z only need find one fermion.
  if (idIn1 == 21 || idIn1 == 22) idIn1 = -idIn2;
  if (idIn2 == 21 || idIn2 == 22) idIn2 = -idIn1;
 
  // Initial flavours and couplings; return if don't make sense.
  if (idIn1 + idIn2 != 0 ) return 0.5;
  int idInAbs = abs(idIn1);
  if (idInAbs == 0 || idInAbs > 18 ) return 0.5; 
  double ei = CoupEW::ef(idInAbs);
  double vi = CoupEW::vf(idInAbs);
  double ai = CoupEW::af(idInAbs);

  // Final flavours and couplings; return if don't make sense.
  if (event[iDau1].id() + event[iDau2].id() != 0) return 0.5;
  int idOutAbs = abs(event[iDau1].id());
  if (idOutAbs == 0 || idOutAbs >18 ) return 0.5; 
  double ef = CoupEW::ef(idOutAbs);
  double vf = CoupEW::vf(idOutAbs);
  double af = CoupEW::af(idOutAbs);

  // Calculate prefactors for interference and resonance part.
  Vec4 psum = event[iDau1].p() + event[iDau2].p();
  double sH = psum.m2Calc();
  double intNorm = 2. * thetaWRat * sH * (sH - mZ*mZ)
    / ( pow2(sH - mZ*mZ) + pow2(sH * gammaZ / mZ) );
  double resNorm = pow2(thetaWRat * sH) 
    / ( pow2(sH - mZ*mZ) + pow2(sH * gammaZ / mZ) );

  // Calculate vector and axial expressions and find mix.
  double vect = ei*ei * ef*ef + ei*vi * intNorm * ef*vf
    + (vi*vi + ai*ai) * resNorm * vf*vf;
  double axiv = (vi*vi + ai*ai) * resNorm * af*af;
  return vect / (vect + axiv);
}

//*********

// Set up to calculate QCD ME correction with calcMEcorr.
// Normally for primary particles, but also from g/gamma -> f fbar.
  
double TimeShower::findMEcorr(TimeDipoleEnd* dip, Particle& rad, 
  Particle& partner, Particle& emt) {
  
  // Initial values and matrix element kind.
  //cout << "\n enter findMEcorr " << dip->MEtype << "  " << rad.id() 
  //     << "  " << partner.id() << "  " << emt.id() << endl;
  double wtME    = 1.;
  double wtPS    = 1.; 
  int    MEkind  = dip->MEtype / 5;
  int    MEcombi = dip->MEtype % 5;

  // Construct ME variables.
  Vec4   sum     = rad.p() + partner.p() + emt.p();
  double eCMME   = sum.mCalc();
  double x1      = 2. * (sum * rad.p()) / pow2(eCMME);
  double x2      = 2. * (sum * partner.p()) / pow2(eCMME); 
  double r1      = rad.m() / eCMME;
  double r2      = partner.m() / eCMME; 

  // Derived ME variables, suitably protected.
  double x1minus = max(XMARGIN, 1. + r1*r1 - r2*r2 - x1);
  double x2minus = max(XMARGIN, 1. + r2*r2 - r1*r1 - x2) ;
  double x3      = max(XMARGIN, 2. - x1 - x2);
  //cout << scientific << setprecision(6) << "x_i = " << x1 << "  " << x2 
  //     << " " << x3 << "  " << r1 << "  " << r2 << endl; 

  // Begin processing of QCD dipoles.
  if (dip->colType !=0) {

    // Evaluate normal ME, for proper order of particles.
    if (dip->MEorder) 
         wtME = calcMEcorr(MEkind, MEcombi, dip->MEmix, x1, x2, r1, r2);
    else wtME = calcMEcorr(MEkind, MEcombi, dip->MEmix, x2, x1, r2, r1);
    //cout << " ME direct " << dip->MEorder << "  " << wtME << endl;

    // Split up total ME when two radiating particles.
    if (dip->MEsplit) wtME = wtME * x1minus / x3; 
    //cout << " ME modif " << dip->MEsplit << "  " << wtME << endl;

    // Evaluate shower rate to be compared with.
    wtPS = 2. / ( x3 * x2minus );
    if (dip->MEgluinoRec) wtPS *= 9./4.;
    //cout << " PS " << dip->MEgluinoRec << "  " << wtPS << endl;
  
  // For generic charge combination currently only massless expression.
  // (Masses included only to respect phase space boundaries.)
  } else if (dip->chgType !=0 && dip->MEtype == 101) {
    double chg1 = ParticleDataTable::charge(rad.id());
    double chg2 = ParticleDataTable::charge(partner.id());
    wtME = (x1*x1 + x2*x2) * pow2( chg1 * x1minus / x3 
      - chg2 * x2minus / x3 );
    wtPS = 2. * ( chg1*chg1 * x1minus / x3 + chg2*chg2 * x2minus / x3 ); 

  // For flavour neutral system assume vector source and include masses.
  } else if (dip->chgType !=0 && dip->MEtype == 102) {
    wtME = calcMEcorr(2, 1, dip->MEmix, x1, x2, r1, r2) * x1minus / x3;
    wtPS = 2. / ( x3 * x2minus );
  }
  if (wtME > wtPS) infoPtr->errorMsg("Warning in TimeShower::findMEcorr: "
    "ME weight above PS one");
       
  // Return ratio of actual ME to assumed PS rate of emission.
  return wtME / wtPS; 
}

//*********

// Matrix elements for gluon (or photon) emission from
// a two-body state; to be used by the parton shower routine.
// Here x_i = 2 E_i/E_cm, r_i = m_i/E_cm and
// 1/sigma_0 d(sigma)/d(x_1)d(x_2) = (alpha-strong/2 pi) * C_F * (this),
// i.e. normalization is such that one recovers the familiar
// (x_1^2 + x_2^2)/((1-x_1)*(1-x_2)) for the massless case.
// Coupling structure:
// kind =  1 : eikonal soft-gluon expression (spin-independent)
//      =  2 : V -> q qbar (V = vector/axial vector colour singlet)
//      =  3 : q -> q V
//      =  4 : S -> q qbar (S = scalar/pseudoscalar colour singlet)
//      =  5 : q -> q S
//      =  6 : V -> ~q ~qbar (~q = squark)
//      =  7 : ~q -> ~q V
//      =  8 : S -> ~q ~qbar
//      =  9 : ~q -> ~q S
//      = 10 : chi -> q ~qbar (chi = neutralino/chargino)
//      = 11 : ~q -> q chi
//      = 12 : q -> ~q chi
//      = 13 : ~g -> q ~qbar
//      = 14 : ~q -> q ~g
//      = 15 : q -> ~q ~g
//      = 16 : (9/4)*(eikonal) for gg -> ~g ~g
// Note that the order of the decay products is important.
// combi = 1 : pure non-gamma5, i.e. vector/scalar/...
//       = 2 : pure gamma5, i.e. axial vector/pseudoscalar/....
//       = 3 : mixture mix*(combi=1) + (1-mix)*(combi=2)
//       = 4 : mixture (combi=1) +- (combi=2)

double TimeShower::calcMEcorr( int kind, int combiIn, double mixIn, 
  double x1, double x2, double r1, double r2) {

  // Frequent variable combinations.
  double x3     = 2. - x1 - x2;
  double x1s    = x1 * x1;
  double x2s    = x2 * x2;
  double x3s    = x3 * x3;
  double x1c    = x1 * x1s;
  double x2c    = x2 * x2s;
  double x3c    = x3 * x3s;
  double r1s    = r1 * r1;
  double r2s    = r2 * r2;
  double r1c    = r1 * r1s;
  double r2c    = r2 * r2s;
  double r1q    = r1s * r1s;
  double r2q    = r2s * r2s;
  double prop1  = 1. + r1s - r2s - x1; 
  double prop2  = 1. + r2s - r1s - x2;
  double prop1s = prop1 * prop1;
  double prop2s = prop2 * prop2;
  double prop12 = prop1 * prop2;
  double prop13 = prop1 * x3;
  double prop23 = prop2 * x3;

  // Check input values. Return zero outside allowed phase space.
  if (x1 - 2.*r1 < XMARGIN || prop1 < XMARGIN) return 0.;
  if (x2 - 2.*r2 < XMARGIN || prop2 < XMARGIN) return 0.;
  if (x1 + x2 - 1. - pow2(r1+r2) < XMARGIN) return 0.;
  // Note: equivalent rewritten form 4. * ( (1. - x1) * (1. - x2) 
  // * (1. - r1s - r2s - x3) + r1s * (1. - x2s - x3) + r2s 
  // * (1. - x1s - x3) - pow2(r1s - r2s) ) gives abot same result.
  if ( (x1s - 4.*r1s) * (x2s - 4.*r2s) 
    - pow2( 2. * (1. - x1 - x2 + r1s + r2s) + x1*x2 ) 
    < XMARGIN * (XMARGINCOMB + r1 + r2) ) return 0.;

  // Initial values; phase space.
  int combi   = max(1, min(4, combiIn) ); 
  double mix  = max(0., min(1., mixIn) );
  bool isSet1 = false;
  bool isSet2 = false;
  bool isSet4 = false;
  double ps = sqrtpos( pow2(1. - r1*r1 - r2*r2) - pow2(2. * r1 * r2) );
  double rLO = 0., rFO = 0., rLO1 = 0., rFO1 = 0., rLO2 = 0., 
    rFO2 = 0., rLO4 = 0., rFO4 = 0.;
  double offset = 0;
 
  // Select which kind of ME to use.
  switch (kind) {

    // case 1 is equal to default, i.e. eikonal expression.

    // V -> q qbar (V = gamma*/Z0/W+-/...).
    case 2:
      if (combi == 1 || combi == 3) {
        rLO1 = ps*(2.-r1s-r1q+6.*r1*r2-r2s+2.*r1s*r2s-r2q)/2.;
        rFO1 = -(3.+6.*r1s+r1q-6.*r1*r2+6.*r1c*r2-2.*r2s-6.*r1s*r2s
        +6.*r1*r2c+r2q-3.*x1+6.*r1*r2*x1+2.*r2s*x1+x1s-2.*r1s*x1s
        +3.*r1s*x3+6.*r1*r2*x3-r2s*x3-2.*x1*x3-5.*r1s*x1*x3
        +r2s*x1*x3+x1s*x3-3.*x3s-3.*r1s*x3s+r2s*x3s
        +2.*x1*x3s+x3c-x2)
        /prop2s
        -2.*(-3.+r1s-6.*r1*r2+6.*r1c*r2+3.*r2s-4.*r1s*r2s
        +6.*r1*r2c+2.*x1+3.*r1s*x1+r2s*x1-x1s-r1s*x1s
        -r2s*x1s+4.*x3+2.*r1s*x3+3.*r1*r2*x3-r2s*x3-3.*x1*x3
        -2.*r1s*x1*x3+x1s*x3-x3s-r1s*x3s+r1*r2*x3s+x1*x3s)
        /prop12
        -(-1.+2.*r1s+r1q+6.*r1*r2+6.*r1c*r2-2.*r2s-6.*r1s*r2s
        +6.*r1*r2c+r2q-x1-2.*r1s*x1-6.*r1*r2*x1+8.*r2s*x1+x1s
        -2.*r2s*x1s-r1s*x3+r2s*x3-r1s*x1*x3+r2s*x1*x3+x1s*x3+x2)
        /prop1s;
        rFO1 = rFO1/2.;
        isSet1 = true;
      }
      if (combi == 2 || combi == 3) {
        rLO2 = ps*(2.-r1s-r1q-6.*r1*r2-r2s+2.*r1s*r2s-r2q)/2.;
        rFO2 = -(3.+6.*r1s+r1q+6.*r1*r2-6.*r1c*r2-2.*r2s-6.*r1s*r2s    
        -6.*r1*r2c+r2q-3.*x1-6.*r1*r2*x1+2.*r2s*x1+x1s-2.*r1s*x1s
        +3.*r1s*x3-6.*r1*r2*x3-r2s*x3-2.*x1*x3-5.*r1s*x1*x3
        +r2s*x1*x3+x1s*x3-3.*x3s-3.*r1s*x3s+r2s*x3s+2.*x1*x3s+x3c-x2)
        /prop2s
        -2.*(-3+r1s+6.*r1*r2-6.*r1c*r2+3.*r2s-4.*r1s*r2s-6.*r1*r2c
        +2.*x1+3.*r1s*x1+r2s*x1-x1s-r1s*x1s-r2s*x1s+4.*x3+2.*r1s*x3
        -3.*r1*r2*x3-r2s*x3-3.*x1*x3-2.*r1s*x1*x3+x1s*x3-x3s-r1s*x3s
        -r1*r2*x3s+x1*x3s)
        /prop12
        -(-1.+2.*r1s+r1q-6.*r1*r2-6.*r1c*r2-2.*r2s-6.*r1s*r2s
        -6.*r1*r2c+r2q-x1-2.*r1s*x1+6.*r1*r2*x1+8.*r2s*x1+x1s
        -2.*r2s*x1s-r1s*x3+r2s*x3-r1s*x1*x3+r2s*x1*x3+x1s*x3+x2)
        /prop1s;
        rFO2 = rFO2/2.;
        isSet2 = true;
      }
      if (combi == 4) {
        rLO4 = ps*(2.-r1s-r1q-r2s+2.*r1s*r2s-r2q)/2.;
        rFO4 = (1.-r1q+6.*r1s*r2s-r2q+x1+3.*r1s*x1-9.*r2s*x1-3.*x1s
        -r1s*x1s+3.*r2s*x1s+x1c-x2-r1s*x2+r2s*x2-r1s*x1*x2+r2s*x1*x2
        +x1s*x2)
        /prop1s 
        -2.*(1.+r1s+r2s-4.*r1s*r2s+r1s*x1+2.*r2s*x1-x1s-r2s*x1s
        +2.*r1s*x2+r2s*x2-3.*x1*x2+x1s*x2-x2s-r1s*x2s+x1*x2s)
        /prop12
        +(1.-r1q+6.*r1s*r2s-r2q-x1+r1s*x1-r2s*x1+x2-9.*r1s*x2
        +3.*r2s*x2+r1s*x1*x2-r2s*x1*x2-3.*x2s+3.*r1s*x2s-r2s*x2s
        +x1*x2s+x2c)
        /prop2s;
        rFO4 = rFO4/2.;
        isSet4 = true;
      }
      break; 
 
    // q -> q V.
    case 3:
      if (combi == 1 || combi == 3) {
        rLO1 = ps*(1.-2.*r1s+r1q+r2s-6.*r1*r2s+r1s*r2s-2.*r2q);
        rFO1 = -2.*(-1.+r1-2.*r1s+2.*r1c-r1q+pow5(r1)-r2s+r1*r2s
        -5.*r1s*r2s+r1c*r2s-2.*r1*r2q+2.*x1-2.*r1*x1+2.*r1s*x1
        -2.*r1c*x1+2.*r2s*x1+5.*r1*r2s*x1+r1s*r2s*x1+2.*r2q*x1
        -x1s+r1*x1s-r2s*x1s+3.*x2+4.*r1s*x2+r1q*x2+2.*r2s*x2
        +2.*r1s*r2s*x2-4.*x1*x2-2.*r1s*x1*x2-r2s*x1*x2+x1s*x2
        -2.*x2s-2.*r1s*x2s+x1*x2s)
        /prop23
        +(2.*r2s+6.*r1*r2s-6.*r1s*r2s+6.*r1c*r2s+2.*r2q+6.*r1*r2q
        -r2s*x1+r1s*r2s*x1-r2q*x1+x2-r1q*x2-3.*r2s*x2-6.*r1*r2s*x2
        +9.*r1s*r2s*x2-2.*r2q*x2-x1*x2+r1s*x1*x2-x2s-3.*r1s*x2s
        +2.*r2s*x2s+x1*x2s)
        /prop2s
        +(-4.-8.*r1s-4.*r1q+4.*r2s-4.*r1s*r2s+8.*r2q+9.*x1+10.*r1s*x1
        +r1q*x1-3.*r2s*x1+6.*r1*r2s*x1+r1s*r2s*x1-2.*r2q*x1-6.*x1s-
        2.*r1s*x1s+x1c+7.*x2+8.*r1s*x2+r1q*x2-7.*r2s*x2+6.*r1*r2s*x2
        +r1s*r2s*x2-2.*r2q*x2-9.*x1*x2-3.*r1s*x1*x2+2.*r2s*x1*x2
        +2.*x1s*x2-3.*x2s-r1s*x2s+2.*r2s*x2s+x1*x2s)
	/x3s;
        isSet1 = true;
      }
      if (combi == 2 || combi == 3) {
        rLO2 = ps*(1.-2.*r1s+r1q+r2s+6.*r1*r2s+r1s*r2s-2.*r2q);
        rFO2 = 2*(1.+r1+2.*r1s+2.*r1c+r1q+pow5(r1)+r2s+r1*r2s
        +5.*r1s*r2s+r1c*r2s-2.*r1*r2q-2.*x1-2.*r1*x1-2.*r1s*x1
        -2.*r1c*x1-2.*r2s*x1+5.*r1*r2s*x1-r1s*r2s*x1-2.*r2q*x1+x1s
        +r1*x1s+r2s*x1s-3.*x2-4.*r1s*x2-r1q*x2-2.*r2s*x2
        -2.*r1s*r2s*x2+4.*x1*x2+2.*r1s*x1*x2+r2s*x1*x2-x1s*x2
        +2.*x2s+2.*r1s*x2s-x1*x2s)
        /prop23
        +(2.*r2s-6.*r1*r2s-6.*r1s*r2s-6.*r1c*r2s+2.*r2q-6.*r1*r2q
        -r2s*x1+r1s*r2s*x1-r2q*x1+x2-r1q*x2-3.*r2s*x2+6.*r1*r2s*x2
        +9.*r1s*r2s*x2-2.*r2q*x2-x1*x2+r1s*x1*x2-x2s-3.*r1s*x2s
        +2.*r2s*x2s+x1*x2s)
        /prop2s
        +(-4.-8.*r1s-4.*r1q+4.*r2s-4.*r1s*r2s+8.*r2q+9.*x1+10.*r1s*x1
        +r1q*x1-3.*r2s*x1-6.*r1*r2s*x1+r1s*r2s*x1-2.*r2q*x1-6.*x1s
        -2.*r1s*x1s+x1c+7.*x2+8.*r1s*x2+r1q*x2-7.*r2s*x2-6.*r1*r2s*x2
        +r1s*r2s*x2-2.*r2q*x2-9.*x1*x2-3.*r1s*x1*x2+2.*r2s*x1*x2
        +2.*x1s*x2-3.*x2s-r1s*x2s+2.*r2s*x2s+x1*x2s)
	/x3s;
        isSet2 = true;
      }
      if (combi == 4) {
        rLO4 = ps*(1.-2.*r1s+r1q+r2s+r1s*r2s-2.*r2q);
        rFO4 = 2*(1.+2.*r1s+r1q+r2s+5.*r1s*r2s-2.*x1-2.*r1s*x1
        -2.*r2s*x1-r1s*r2s*x1-2.*r2q*x1+x1s+r2s*x1s-3.*x2-4.*r1s*x2
        -r1q*x2-2.*r2s*x2-2.*r1s*r2s*x2+4.*x1*x2+2.*r1s*x1*x2+r2s*x1*x2
        -x1s*x2+2.*x2s+2.*r1s*x2s-x1*x2s)
        /prop23
        +(2.*r2s-6.*r1s*r2s+2.*r2q-r2s*x1+r1s*r2s*x1-r2q*x1+x2-r1q*x2
        -3.*r2s*x2+9.*r1s*r2s*x2-2.*r2q*x2-x1*x2+r1s*x1*x2-x2s-3.*r1s*x2s
        +2.*r2s*x2s+x1*x2s)
        /prop2s
        +(-4.-8.*r1s-4.*r1q+4.*r2s-4.*r1s*r2s+8.*r2q+9.*x1+10.*r1s*x1
        +r1q*x1-3.*r2s*x1+r1s*r2s*x1-2.*r2q*x1-6.*x1s-2.*r1s*x1s+x1c
        +7.*x2+8.*r1s*x2+r1q*x2-7.*r2s*x2+r1s*r2s*x2-2.*r2q*x2-9.*x1*x2
        -3.*r1s*x1*x2+2.*r2s*x1*x2+2.*x1s*x2-3.*x2s-r1s*x2s+2.*r2s*x2s
        +x1*x2s)
        /x3s;
        isSet4 = true;
      }
      break; 
 
    // S -> q qbar    (S = h0/H0/A0/H+-/...).
    case 4:
      if (combi == 1 || combi == 3) {
        rLO1 = ps*(1.-r1s-r2s-2.*r1*r2);
        rFO1 = -(-1.+r1q-2.*r1*r2-2.*r1c*r2-6.*r1s*r2s-2.*r1*r2c+r2q+x1
        -r1s*x1+2.*r1*r2*x1+3.*r2s*x1+x2+r1s*x2-r2s*x2-x1*x2)
        /prop1s
        -2.*(r1s+r1q-2.*r1c*r2+r2s-6.*r1s*r2s-2.*r1*r2c+r2q-r1s*x1
        +r1*r2*x1+2.*r2s*x1+2.*r1s*x2+r1*r2*x2-r2s*x2-x1*x2)
        /prop12
        -(-1.+r1q-2.*r1*r2-2.*r1c*r2-6.*r1s*r2s-2.*r1*r2c+r2q+x1-r1s*x1
        +r2s*x1+x2+3.*r1s*x2+2.*r1*r2*x2-r2s*x2-x1*x2)
        /prop2s;
        isSet1 = true;
      }
      if (combi == 2 || combi == 3) {
        rLO2 = ps*(1.-r1s-r2s+2.*r1*r2);
        rFO2 = -(-1.+r1q+2.*r1*r2+2.*r1c*r2-6.*r1s*r2s+2.*r1*r2c+r2q+x1
        -r1s*x1-2.*r1*r2*x1+3.*r2s*x1+x2+r1s*x2-r2s*x2-x1*x2)
        /prop1s
        -(-1.+r1q+2.*r1*r2+2.*r1c*r2-6.*r1s*r2s+2.*r1*r2c+r2q+x1
        -r1s*x1+r2s*x1+x2+3.*r1s*x2-2.*r1*r2*x2-r2s*x2-x1*x2)
        /prop2s
        +2.*(-r1s-r1q-2.*r1c*r2-r2s+6.*r1s*r2s-2.*r1*r2c-r2q+r1s*x1
        +r1*r2*x1-2.*r2s*x1-2.*r1s*x2+r1*r2*x2+r2s*x2+x1*x2)
        /prop12;
        isSet2 = true;
      }
      if (combi == 4) {
        rLO4 = ps*(1.-r1s-r2s);
        rFO4 = -(-1.+r1q-6.*r1s*r2s+r2q+x1-r1s*x1+3.*r2s*x1+x2
        +r1s*x2-r2s*x2-x1*x2)
        /prop1s
        -2.*(r1s+r1q+r2s-6.*r1s*r2s+r2q-r1s*x1
        +2.*r2s*x1+2.*r1s*x2-r2s*x2-x1*x2)
        /prop12
        -(-1.+r1q-6.*r1s*r2s+r2q+x1-r1s*x1+r2s*x1
        +x2+3.*r1s*x2-r2s*x2-x1*x2)
        /prop2s;
        isSet4 = true;
      }
      break; 
 
    // q -> q S.
    case 5:
      if (combi == 1 || combi == 3) {
        rLO1 = ps*(1.+r1s-r2s+2.*r1);
        rFO1 = (4.-4.*r1s+4.*r2s-3.*x1-2.*r1*x1+r1s*x1-r2s*x1-5.*x2
        -2.*r1*x2+r1s*x2-r2s*x2+x1*x2+x2s)
        /x3s
        -2.*(3.-r1-5.*r1s-r1c+3.*r2s+r1*r2s-2.*x1-r1*x1
        +r1s*x1-4.*x2+2.*r1s*x2-r2s*x2+x1*x2+x2s)
        /prop23
        +(2.-2.*r1-6.*r1s-2.*r1c+2.*r2s-2.*r1*r2s-x1+r1s*x1
        -r2s*x1-3.*x2+2.*r1*x2+3.*r1s*x2-r2s*x2+x1*x2+x2s)
        /prop2s;
        isSet1 = true;
      }
      if (combi == 2 || combi == 3) {
        rLO2 = ps*(1.+r1s-r2s-2.*r1);
        rFO2 = (4.-4.*r1s+4.*r2s-3.*x1+2.*r1*x1+r1s*x1-r2s*x1-5.*x2
        +2.*r1*x2+r1s*x2-r2s*x2+x1*x2+x2s)
        /x3s
        -2.*(3.+r1-5.*r1s+r1c+3.*r2s-r1*r2s-2.*x1+r1*x1
        +r1s*x1-4.*x2+2.*r1s*x2-r2s*x2+x1*x2+x2s)
        /prop23
        +(2.+2.*r1-6.*r1s+2.*r1c+2.*r2s+2.*r1*r2s-x1+r1s*x1
        -r2s*x1-3.*x2-2.*r1*x2+3.*r1s*x2-r2s*x2+x1*x2+x2s)
        /prop2s;
        isSet2 = true;
      }
      if (combi == 4) {
        rLO4 = ps*(1.+r1s-r2s);
        rFO4 = (4.-4.*r1s+4.*r2s-3.*x1+r1s*x1-r2s*x1-5.*x2+r1s*x2
        -r2s*x2+x1*x2+x2s)
        /x3s
        -2.*(3.-5.*r1s+3.*r2s-2.*x1+r1s*x1-4.*x2+2.*r1s*x2
        -r2s*x2+x1*x2+x2s)
        /prop23
        +(2.-6.*r1s+2.*r2s-x1+r1s*x1-r2s*x1-3.*x2+3.*r1s*x2
        -r2s*x2+x1*x2+x2s)
        /prop2s;
        isSet4 = true;
      }
      break; 
 
    // V -> ~q ~qbar  (~q = squark).
    case 6:
      rLO1 = ps*(1.-2.*r1s+r1q-2.*r2s-2.*r1s*r2s+r2q);
      rFO1 = 2.*3.+(1.+r1s+r2s-x1)*(4.*r1s-x1s)
      /prop1s
      +2.*(-1.-3.*r1s-r2s+x1+x1s*0.5+x2-x1*x2*0.5)
      /prop1
      +(1.+r1s+r2s-x2)*(4.*r2s-x2s)
      /prop2s
      +2.*(-1.-r1s-3.*r2s+x1+x2-x1*x2*0.5+x2s*0.5)
      /prop2
      -(-4.*r1s-4.*r1q-4.*r2s-8.*r1s*r2s-4.*r2q+2.*x1+6.*r1s*x1
      +6.*r2s*x1-2.*x1s+2.*x2+6.*r1s*x2+6.*r2s*x2-4.*x1*x2
      -2.*r1s*x1*x2-2.*r2s*x1*x2+x1s*x2-2.*x2s+x1*x2s)
      /prop12;
      isSet1 = true;
      break; 
 
    // ~q -> ~q V.
    case 7:
      rLO1 = ps*(1.-2.*r1s+r1q-2.*r2s-2.*r1s*r2s+r2q);
      rFO1 = 16.*r2s-8.*(4.*r2s+2.*r2s*x1+x2+r1s*x2+r2s*x2-x1*x2
      -2.*x2s)
      /(3.*prop2)
      +8.*(1.+r1s+r2s-x2)*(4.*r2s-x2s)
      /(3.*prop2s)
      +8.*(x1+x2)*(-1.-2.*r1s-r1q-2.*r2s+2.*r1s*r2s-r2q+2.*x1
      +2.*r1s*x1+2.*r2s*x1-x1s+2.*x2+2.*r1s*x2+2.*r2s*x2-2.*x1*x2-x2s)
      /(3.*x3s)
      +8.*(-1.-r1s+r2s-x1)*(2.*r2s*x1+x2+r1s*x2+r2s*x2-x1*x2-x2s)
      /(3.*prop2*x3)
      -8.*(1.+2.*r1s+r1q+2.*r2s-2.*r1s*r2s+r2q-2.*x1-2.*r1s*x1
      -4.*r2s*x1+x1s-3.*x2-3.*r1s*x2-3.*r2s*x2+3.*x1*x2+2.*x2s)
      /(3.*x3);
      rFO1 = 3.*rFO1/8.;
      isSet1 = true;
      break; 
 
    // S -> ~q ~qbar.
    case 8:
      rLO1 = ps;
      rFO1 = (-1.-2.*r1s-r1q-2.*r2s+2.*r1s*r2s-r2q+2.*x1+2.*r1s*x1
      +2.*r2s*x1-x1s-r2s*x1s+2.*x2+2.*r1s*x2+2.*r2s*x2-3.*x1*x2
      -r1s*x1*x2-r2s*x1*x2+x1s*x2-x2s-r1s*x2s+x1*x2s)
      /(prop1s*prop2s);
      rFO1 = 2.*rFO1;
      isSet1 = true;
      break; 
 
    // ~q -> ~q S.
    case 9:
      rLO1 = ps;
      rFO1 = (-1.-r1s-r2s+x2)
      /prop2s
      +(1.+r1s-r2s+x1)
      /prop23
      -(x1+x2)
      /x3s;
      isSet1 = true;
      break; 
 
    // chi -> q ~qbar   (chi = neutralino/chargino).
    case 10:
      if (combi == 1 || combi == 3) {
        rLO1 = ps*(1.+r1s-r2s+2.*r1);
        rFO1 = (2.*r1+x1)*(-1.-r1s-r2s+x1)
        /prop1s
        +2.*(-1.-r1s-2.*r1c-r2s-2.*r1*r2s+3.*x1*0.5+r1*x1
        -r1s*x1*0.5-r2s*x1*0.5+x2+r1*x2+r1s*x2-x1*x2*0.5)
        /prop12
        +(2.-2.*r1-6.*r1s-2.*r1c+2.*r2s-2.*r1*r2s-x1+r1s*x1
        -r2s*x1-3.*x2+2.*r1*x2+3.*r1s*x2-r2s*x2+x1*x2+x2s)
        /prop2s;
        isSet1 = true;
      }
      if (combi == 2 || combi == 3) {
        rLO2 = ps*(1.-2.*r1+r1s-r2s);
        rFO2 = (2.*r1-x1)*(1.+r1s+r2s-x1)
        /prop1s
        +2.*(-1.-r1s+2.*r1c-r2s+2.*r1*r2s+3.*x1*0.5-r1*x1
        -r1s*x1*0.5-r2s*x1*0.5+x2-r1*x2+r1s*x2-x1*x2*0.5)
        /prop12
        +(2.+2.*r1-6.*r1s+2.*r1c+2.*r2s+2.*r1*r2s-x1+r1s*x1
        -r2s*x1-3.*x2-2.*r1*x2+3.*r1s*x2-r2s*x2+x1*x2+x2s)/
        prop2s;
        isSet2 = true;
      }
      if (combi == 4) {
        rLO4 = ps*(1.+r1s-r2s);
        rFO4 = x1*(-1.-r1s-r2s+x1)
        /prop1s
        +2.*(-1.-r1s-r2s+3.*x1*0.5-r1s*x1*0.5-r2s*x1*0.5
        +x2+r1s*x2-x1*x2*0.5)
        /prop12
        +(2.-6.*r1s+2.*r2s-x1+r1s*x1-r2s*x1-3.*x2+3.*r1s*x2
        -r2s*x2+x1*x2+x2s)
        /prop2s;
        isSet4 = true;
      }
      break; 
 
    // ~q -> q chi.
    case 11:
      if (combi == 1 || combi == 3) {
        rLO1 = ps*(1.-pow2(r1+r2));
        rFO1 = (1.+r1s+2.*r1*r2+r2s-x1-x2)*(x1+x2)
        /x3s
        -(-1.+r1q-2.*r1*r2-2.*r1c*r2-6.*r1s*r2s-2.*r1*r2c+r2q+x1
        -r1s*x1+r2s*x1+x2+3.*r1s*x2+2.*r1*r2*x2-r2s*x2-x1*x2)
        /prop2s
        +(-1.-2.*r1s-r1q-2.*r1*r2-2.*r1c*r2+2.*r1*r2c+r2q+x1+r1s*x1
        -2.*r1*r2*x1-3.*r2s*x1+2.*r1s*x2-2.*r2s*x2+x1*x2+x2s)
        /prop23;
        isSet1 = true;
      }
      if (combi == 2 || combi == 3) {
        rLO2 = ps*(1.-pow2(r1-r2));
        rFO2 = (1.+r1s-2.*r1*r2+r2s-x1-x2)*(x1+x2)
        /x3s
        -(-1.+r1q+2.*r1*r2+2.*r1c*r2-6.*r1s*r2s+2.*r1*r2c+r2q+x1
        -r1s*x1+r2s*x1+x2+3.*r1s*x2-2.*r1*r2*x2-r2s*x2-x1*x2)
        /prop2s
        +(-1.-2.*r1s-r1q+2.*r1*r2+2.*r1c*r2-2.*r1*r2c+r2q+x1+r1s*x1
        +2.*r1*r2*x1-3.*r2s*x1+2.*r1s*x2-2.*r2s*x2+x1*x2+x2s)
        /prop23;
        isSet2 = true;
      }
      if (combi == 4) {
        rLO4 = ps*(1.-r1s-r2s);
        rFO4 = (1.+r1s+r2s-x1-x2)*(x1+x2)
        /x3s
        -(-1.+r1q-6.*r1s*r2s+r2q+x1-r1s*x1+r2s*x1+x2
        +3.*r1s*x2-r2s*x2-x1*x2)
        /prop2s
        +(-1.-2.*r1s-r1q+r2q+x1+r1s*x1-3.*r2s*x1
        +2.*r1s*x2-2.*r2s*x2+x1*x2+x2s)
        /prop23;
        isSet4 = true;
      }
      break; 
 
    // q -> ~q chi.
    case 12:
      if (combi == 1 || combi == 3) {
        rLO1 = ps*(1.-r1s+r2s+2.*r2);
        rFO1 = (2.*r2+x2)*(-1.-r1s-r2s+x2)
        /prop2s
        +(4.+4.*r1s-4.*r2s-5.*x1-r1s*x1-2.*r2*x1+r2s*x1+x1s
        -3.*x2-r1s*x2-2.*r2*x2+r2s*x2+x1*x2)
        /x3s
        +2.*(-1.-r1s+r2+r1s*r2-r2s-r2c+x1+r2*x1+r2s*x1+2.*x2
        +r1s*x2-x1*x2*0.5-x2s*0.5)
        /prop23;
        isSet1 = true;
      }
      if (combi == 2 || combi == 3) {
        rLO2 = ps*(1.-r1s+r2s-2.*r2);
        rFO2 = (2.*r2-x2)*(1.+r1s+r2s-x2)
        /prop2s
        +(4.+4.*r1s-4.*r2s-5.*x1-r1s*x1+2.*r2*x1+r2s*x1+x1s
        -3.*x2-r1s*x2+2.*r2*x2+r2s*x2+x1*x2)
        /x3s
        +2.*(-1.-r1s-r2-r1s*r2-r2s+r2c+x1-r2*x1+r2s*x1+2.*x2
        +r1s*x2-x1*x2*0.5-x2s*0.5)
        /prop23;
        isSet2 = true;
      }
      if (combi == 4) {
        rLO4 = ps*(1.-r1s+r2s);
        rFO4 = x2*(-1.-r1s-r2s+x2)
        /prop2s
        +(4.+4.*r1s-4.*r2s-5.*x1-r1s*x1+r2s*x1+x1s
        -3.*x2-r1s*x2+r2s*x2+x1*x2)
        /x3s
        +2.*(-1.-r1s-r2s+x1+r2s*x1+2.*x2
        +r1s*x2-x1*x2*0.5-x2s*0.5)
        /prop23;
        isSet4 = true;
      }
      break; 
 
    // ~g -> q ~qbar.
    case 13:
      if (combi == 1 || combi == 3) {
        rLO1 = ps*(1.+r1s-r2s+2.*r1);
        rFO1 = 4.*(2.*r1+x1)*(-1.-r1s-r2s+x1)
        /(3.*prop1s)
        -(-1.-r1s-2.*r1c-r2s-2.*r1*r2s+3.*x1*0.5+r1*x1-r1s*x1*0.5
        -r2s*x1*0.5+x2+r1*x2+r1s*x2-x1*x2*0.5)
        /(3.*prop12)
        +3.*(-1.+r1-r1s-r1c-r2s+r1*r2s+2.*x1+r2s*x1-x1s*0.5+x2+r1*x2
        +r1s*x2-x1*x2*0.5)
        /prop13
        +3.*(4.-4.*r1s+4.*r2s-3.*x1-2.*r1*x1+r1s*x1-r2s*x1-5.*x2
        -2.*r1*x2+r1s*x2-r2s*x2+x1*x2+x2s)
        /x3s
        -3.*(3.-r1-5.*r1s-r1c+3.*r2s+r1*r2s-2.*x1-r1*x1+r1s*x1
        -4.*x2+2.*r1s*x2-r2s*x2+x1*x2+x2s)
        /prop23
        +4.*(2.-2.*r1-6.*r1s-2.*r1c+2.*r2s-2.*r1*r2s-x1+r1s*x1-r2s*x1
        -3.*x2+2.*r1*x2+3.*r1s*x2-r2s*x2+x1*x2+x2s)
        /(3.*prop2s);
        rFO1 = 3.*rFO1/4.;
        isSet1 = true;
      }
      if (combi == 2 || combi == 3) {
        rLO2 = ps*(1.+r1s-r2s-2.*r1);
        rFO2 = 4.*(2.*r1-x1)*(1.+r1s+r2s-x1)
        /(3.*prop1s)
        +3.*(-1.-r1-r1s+r1c-r2s-r1*r2s+2.*x1+r2s*x1-x1s*0.5
        +x2-r1*x2+r1s*x2-x1*x2*0.5)
        /prop13
        +(2.+2.*r1s-4.*r1c+2.*r2s-4.*r1*r2s-3.*x1+2.*r1*x1
        +r1s*x1+r2s*x1-2.*x2+2.*r1*x2-2.*r1s*x2+x1*x2)
        /(6.*prop12)
        +3.*(4.-4.*r1s+4.*r2s-3.*x1+2.*r1*x1+r1s*x1-r2s*x1-5.*x2
        +2.*r1*x2+r1s*x2-r2s*x2+x1*x2+x2s)
        /x3s
        -3.*(3.+r1-5.*r1s+r1c+3.*r2s-r1*r2s-2.*x1+r1*x1+r1s*x1-4.*x2
        +2.*r1s*x2-r2s*x2+x1*x2+x2s)
        /prop23
        +4.*(2.+2.*r1-6.*r1s+2.*r1c+2.*r2s+2.*r1*r2s-x1+r1s*x1-r2s*x1
        -3.*x2-2.*r1*x2+3.*r1s*x2-r2s*x2+x1*x2+x2s)
        /(3.*prop2s);
        rFO2 = 3.*rFO2/4.;
        isSet2 = true;
      }
      if (combi == 4) {
        rLO4 = ps*(1.+r1s-r2s);
        rFO4 = 8.*x1*(-1.-r1s-r2s+x1)
        /(3.*prop1s)
        +6.*(-1-r1s-r2s+2.*x1+r2s*x1-x1s*0.5+x2+r1s*x2-x1*x2*0.5)
        /prop13
        +(2.+2.*r1s+2.*r2s-3.*x1+r1s*x1+r2s*x1-2.*x2-2.*r1s*x2+x1*x2)
        /(3.*prop12)
        +6.*(4.-4.*r1s+4.*r2s-3.*x1+r1s*x1-r2s*x1-5.*x2+r1s*x2-r2s*x2
        +x1*x2+x2s)
        /x3s
        -6.*(3.-5.*r1s+3.*r2s-2.*x1+r1s*x1-4.*x2+2.*r1s*x2-r2s*x2+x1*x2+x2s)
        /prop23
        +8.*(2.-6.*r1s+2.*r2s-x1+r1s*x1-r2s*x1-3.*x2+3.*r1s*x2-r2s*x2
        +x1*x2+x2s)
        /(3.*prop2s);
        rFO4 = 3.*rFO4/8.;
        isSet4 = true;
      }
      break; 
 
    // ~q -> q ~g.
    case 14:
      if (combi == 1 || combi == 3) {
        rLO1 = ps*(1.-r1s-r2s-2.*r1*r2);
        rFO1 = 64.*(1.+r1s+2.*r1*r2+r2s-x1-x2)*(x1+x2)
        /(9.*x3s)
        -16.*(-1.+r1q-2.*r1*r2-2.*r1c*r2-6.*r1s*r2s-2.*r1*r2c+r2q
        +x1-r1s*x1+2.*r1*r2*x1+3.*r2s*x1+x2+r1s*x2-r2s*x2-x1*x2)
        /prop1s
        -16.*(r1s+r1q-2.*r1c*r2+r2s-6.*r1s*r2s-2.*r1*r2c+r2q-r1s*x1
        +r1*r2*x1+2.*r2s*x1+2.*r1s*x2+r1*r2*x2-r2s*x2-x1*x2)
        /prop12
        -64.*(-1.+r1q-2.*r1*r2-2.*r1c*r2-6.*r1s*r2s-2.*r1*r2c+r2q+x1
        -r1s*x1+r2s*x1+x2+3.*r1s*x2+2.*r1*r2*x2-r2s*x2-x1*x2)
        /(9.*prop2s)
        +8.*(-1.+r1q-2.*r1*r2+2.*r1c*r2-2.*r2s-2.*r1*r2c-r2q-2.*r1s*x1
        +2.*r2s*x1+x1s+x2-3.*r1s*x2-2.*r1*r2*x2+r2s*x2+x1*x2)
        /prop13
        -8.*(-1.-2.*r1s-r1q-2.*r1*r2-2.*r1c*r2+2.*r1*r2c+r2q+x1+r1s*x1
        -2.*r1*r2*x1-3.*r2s*x1+2.*r1s*x2-2.*r2s*x2+x1*x2+x2s)
        /(9.*prop23);
        rFO1 = 9.*rFO1/64.;
        isSet1 = true;
      }
      if (combi == 2 || combi == 3) {
        rLO2 = ps*(1.-r1s-r2s+2.*r1*r2);
        rFO2 = 64.*(1.+r1s-2.*r1*r2+r2s-x1-x2)*(x1+x2)
        /(9.*x3s)
        -16.*(-1.+r1q+2.*r1*r2+2.*r1c*r2-6.*r1s*r2s+2.*r1*r2c+r2q+x1
        -r1s*x1-2.*r1*r2*x1+3.*r2s*x1+x2+r1s*x2-r2s*x2-x1*x2)
        /prop1s
        -64.*(-1.+r1q+2.*r1*r2+2.*r1c*r2-6.*r1s*r2s+2.*r1*r2c+r2q+x1
        -r1s*x1+r2s*x1+x2+3.*r1s*x2-2.*r1*r2*x2-r2s*x2-x1*x2)
        /(9.*prop2s)
        +16.*(-r1s-r1q-2.*r1c*r2-r2s+6.*r1s*r2s-2.*r1*r2c-r2q+r1s*x1
        +r1*r2*x1-2.*r2s*x1-2.*r1s*x2+r1*r2*x2+r2s*x2+x1*x2)
        /prop12
        +8.*(-1.+r1q+2.*r1*r2-2.*r1c*r2-2.*r2s+2.*r1*r2c-r2q-2.*r1s*x1
        +2.*r2s*x1+x1s+x2-3.*r1s*x2+2.*r1*r2*x2+r2s*x2+x1*x2)
        /prop13
        -8.*(-1.-2.*r1s-r1q+2.*r1*r2+2.*r1c*r2-2.*r1*r2c+r2q+x1+r1s*x1+
        2.*r1*r2*x1-3.*r2s*x1+2.*r1s*x2-2.*r2s*x2+x1*x2+x2s)
        /(9.*prop23);
        rFO2 = 9.*rFO2/64.;
        isSet2 = true;
      }
      if (combi == 4) {
        rLO4 = ps*(1.-r1s-r2s);
        rFO4 = 128.*(1.+r1s+r2s-x1-x2)*(x1+x2)
        /(9.*x3s)
        -32*(-1.+r1q-6.*r1s*r2s+r2q+x1-r1s*x1+3.*r2s*x1+x2
        +r1s*x2-r2s*x2-x1*x2)
        /prop1s
        -32.*(r1s+r1q+r2s-6.*r1s*r2s+r2q-r1s*x1+2.*r2s*x1+2.*r1s*x2
        -r2s*x2-x1*x2)
        /prop12
        -128.*(-1.+r1q-6.*r1s*r2s+r2q+x1-r1s*x1+r2s*x1+x2+3.*r1s*x2
        -r2s*x2-x1*x2)
        /(9.*prop2s)
        +16.*(-1.+r1q-2.*r2s-r2q-2.*r1s*x1+2.*r2s*x1+x1s
        +x2-3.*r1s*x2+r2s*x2+x1*x2)
        /prop13
        -16.*(-1.-2.*r1s-r1q+r2q+x1+r1s*x1-3.*r2s*x1
        +2.*r1s*x2-2.*r2s*x2+x1*x2+x2s)
        /(9.*prop23);
        rFO4 = 9.*rFO4/128.;
        isSet4 = true;
      }
      break; 
 
    // q -> ~q ~g.
    case 15:
      if (combi == 1 || combi == 3) {
        rLO1 = ps*(1.-r1s+r2s+2.*r2);
        rFO1 = 32*(2.*r2+x2)*(-1.-r1s-r2s+x2)
        /(9.*prop2s)
        +8.*(-1.-r1s-2.*r1s*r2-r2s-2.*r2c+x1+r2*x1+r2s*x1
        +3.*x2*0.5-r1s*x2*0.5+r2*x2-r2s*x2*0.5-x1*x2*0.5)
        /prop12
        +8.*(2.+2.*r1s-2.*r2-2.*r1s*r2-6.*r2s-2.*r2c-3.*x1-r1s*x1
        +2.*r2*x1+3.*r2s*x1+x1s-x2-r1s*x2+r2s*x2+x1*x2)
        /prop1s
        +32.*(4.+4.*r1s-4.*r2s-5.*x1-r1s*x1-2.*r2*x1+r2s*x1+x1s
        -3.*x2-r1s*x2-2.*r2*x2+r2s*x2+x1*x2)
        /(9.*x3s)
        -8.*(3.+3.*r1s-r2+r1s*r2-5.*r2s-r2c-4.*x1-r1s*x1
        +2.*r2s*x1+x1s-2.*x2-r2*x2+r2s*x2+x1*x2)
        /prop13
        -8.*(-1.-r1s+r2+r1s*r2-r2s-r2c+x1+r2*x1+r2s*x1+2.*x2+r1s*x2
        -x1*x2*0.5-x2s*0.5)
        /(9.*prop23);
        rFO1 = 9.*rFO1/32.;
        isSet1 = true;
      }
      if (combi == 2 || combi == 3) {
        rLO2 = ps*(1.-r1s+r2s-2.*r2);
        rFO2 = 32*(2.*r2-x2)*(1.+r1s+r2s-x2)
        /(9.*prop2s)
        +8.*(-1.-r1s+2.*r1s*r2-r2s+2.*r2c+x1-r2*x1+r2s*x1
        +3.*x2*0.5-r1s*x2*0.5-r2*x2-r2s*x2*0.5-x1*x2*0.5)
        /prop12
        +8.*(2.+2.*r1s+2.*r2+2.*r1s*r2-6.*r2s+2.*r2c-3.*x1-r1s*x1
        -2.*r2*x1+3.*r2s*x1+x1s-x2-r1s*x2+r2s*x2+x1*x2)
        /prop1s
        -8.*(3.+3.*r1s+r2-r1s*r2-5.*r2s+r2c-4.*x1-r1s*x1+2.*r2s*x1+x1s
        -2.*x2+r2*x2+r2s*x2+x1*x2)
        /prop13
        +32*(4.+4.*r1s-4.*r2s-5.*x1-r1s*x1+2.*r2*x1+r2s*x1
        +x1s-3.*x2-r1s*x2+2.*r2*x2+r2s*x2+x1*x2)
        /(9.*x3s)
        -8.*(-1.-r1s-r2-r1s*r2-r2s+r2c+x1-r2*x1+r2s*x1+2.*x2+r1s*x2
        -x1*x2*0.5-x2s*0.5)
        /(9.*prop23);
        rFO2 = 9.*rFO2/32.;
        isSet2 = true;
      }
      if (combi == 4) {
        rLO4 = ps*(1.-r1s+r2s);
        rFO4 = 64.*x2*(-1.-r1s-r2s+x2)
        /(9.*prop2s)
        +16.*(-1.-r1s-r2s+x1+r2s*x1+3.*x2*0.5-r1s*x2*0.5
        -r2s*x2*0.5-x1*x2*0.5)
        /prop12
        -16.*(3.+3.*r1s-5.*r2s-4.*x1-r1s*x1+2.*r2s*x1+x1s-2.*x2+r2s*x2
        +x1*x2)
        /prop13
        +64.*(4.+4.*r1s-4.*r2s-5.*x1-r1s*x1+r2s*x1+x1s-3.*x2
        -r1s*x2+r2s*x2+x1*x2)
        /(9.*x3s)
        +16.*(2.+2.*r1s-6.*r2s-3.*x1-r1s*x1+3.*r2s*x1+x1s
        -x2-r1s*x2+r2s*x2+x1*x2)
        /prop1s
        -16.*(-1.-r1s-r2s+x1+r2s*x1+2.*x2+r1s*x2-x1*x2*0.5-x2s*0.5)
        /(9.*prop23);
        rFO4 = 9.*rFO4/64.;
        isSet4 = true;
      }
      break; 
 
    // g -> ~g ~g. Use (9/4)*eikonal. May be changed in the future.
    case 16:
      rLO = ps;
      if      (combi == 2) offset = x3s;
      else if (combi == 3) offset = mix * x3s;
      else if (combi == 4) offset = 0.5 * x3s;
      rFO = ps * 4.5 * ( (x1+x2-1.+offset-r1s-r2s)/prop12 
      - r1s/prop2s - r2s/prop1s );
      break; 

    // Eikonal expression for kind == 1; also acts as default.
    default:
      rLO = ps;
      if      (combi == 2) offset = x3s;
      else if (combi == 3) offset = mix * x3s;
      else if (combi == 4) offset = 0.5 * x3s;
      rFO = ps * 2. * ( (x1+x2-1.+offset-r1s-r2s)/prop12 
      - r1s/prop2s - r2s/prop1s );
      break;

  // End of ME cases. 
  }

  // Find relevant leading and first order expressions.
  if      (combi == 1 && isSet1) {
    rLO = rLO1; 
    rFO = rFO1; }     
  else if (combi == 2 && isSet2) {
    rLO = rLO2; 
    rFO = rFO2; }     
  else if (combi == 3 && isSet1 && isSet2) {
    rLO = mix * rLO1 + (1.-mix) * rLO2; 
    rFO = mix * rFO1 + (1.-mix) * rFO2; }
  else if (isSet4) {
    rLO = rLO4; 
    rFO = rFO4; }     
  else if (combi == 4 && isSet1 && isSet2) {
    rLO = 0.5 * (rLO1 + rLO2);
    rFO = 0.5 * (rFO1 + rFO2); }
  else if (isSet1) {
    rLO = rLO1; 
    rFO = rFO1; } 

  // Return ratio of first to leading order cross section.     
  return rFO / rLO;
}  

//*********

// Find coefficient of azimuthal asymmetry from gluon polarization.

void TimeShower::findAsymPol( Event& event, TimeDipoleEnd* dip) {

  // Default is no asymmetry. Only gluons are studied.
  dip->asymPol = 0.;
  dip->iAunt = 0;
  int iRad = dip->iRadiator;
  if (!doPhiPolAsym || event[iRad].id() != 21) return;

  // Trace grandmother via possibly intermediate recoil copies.
  int iMother = event.iTopCopy(iRad);
  int iGrandM = event[iMother].mother1();

  // Check grandmother flavour and set aunt.
  if (!event[iGrandM].isQuark() && !event[iGrandM].isGluon()) return;
  dip->iAunt = (event[iGrandM].daughter1() == iMother) 
    ? event[iGrandM].daughter2() : event[iGrandM].daughter1();

  // Coefficient from gluon production (approximate z by energy).
  double zProd = event[iRad].e() / (event[iRad].e() 
    + event[dip->iAunt].e());
  if (event[iGrandM].id() != 21) dip->asymPol = 2. * (1. - zProd) 
    / (1. + pow2(1. - zProd) );
  else dip->asymPol = pow2( (1. - zProd) / (1. - zProd * (1. - zProd) ) );

  // Coefficients from gluon decay.
  if (dip->flavour == 21) dip->asymPol *= pow2( (1. - dip->z) 
    / (1. - dip->z * (1. - dip->z) ) );
  else  dip->asymPol *= -2. * dip->z *( 1. - dip->z ) 
    / (1. - 2. * dip->z * (1. - dip->z) );

}

//*********

// Print the list of dipoles.

void TimeShower::list(ostream& os) {

  // Header.
  os << "\n --------  PYTHIA TimeShower Dipole Listing  ----------------"
     << "----------------------------------- \n \n    i    rad    rec   "
     << "    pTmax  col  chg  gam  oni  isr  sys type  MErec     mix  or"
     << "d  spl  ~gR \n" << fixed << setprecision(3);
  
  // Loop over dipole list and print it.
  for (int i = 0; i < int(dipEnd.size()); ++i) 
  os << setw(5) << i << setw(7) << dipEnd[i].iRadiator 
     << setw(7) << dipEnd[i].iRecoiler << setw(12) << dipEnd[i].pTmax 
     << setw(5) << dipEnd[i].colType << setw(5) << dipEnd[i].chgType
     << setw(5) << dipEnd[i].gamType << setw(5) << dipEnd[i].isOctetOnium 
     << setw(5) << dipEnd[i].isrType << setw(5) << dipEnd[i].system  
     << setw(5) << dipEnd[i].MEtype << setw(7) << dipEnd[i].iMEpartner 
     << setw(8) << dipEnd[i].MEmix << setw(5) << dipEnd[i].MEorder 
     << setw(5) << dipEnd[i].MEsplit << setw(5) << dipEnd[i].MEgluinoRec 
     << "\n";
 
  // Done.
  os << "\n --------  End PYTHIA TimeShower Dipole Listing  ------------"
     << "-----------------------------------" << endl;
  
}

//**************************************************************************

} // end namespace Pythia8

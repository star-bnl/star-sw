// SpaceShower.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Function definitions (not found in the header) for the
// SpaceShower class.

#include "SpaceShower.h"

namespace Pythia8 {

//**************************************************************************

// The SpaceShower class.

//*********

// Constants: could be changed here if desired, but normally should not.
// These are of technical nature, as described for each.

// Leftover companion can give PDF > 0 at small Q2 where other PDF's = 0,
// and then one can end in infinite loop of impossible kinematics.
const int    SpaceShower::MAXLOOPTINYPDF = 10; 

// Switch to alternative (but equivalent) backwards evolution for
// g -> Q Qbar (Q = c or b) when below QTHRESHOLD * mQ2.
const double SpaceShower::CTHRESHOLD     = 2.0; 
const double SpaceShower::BTHRESHOLD     = 2.0; 

// Renew evaluation of PDF's when the pT2 step is bigger than this 
// (in addition to initial scale and c and b thresholds.)
const double SpaceShower::EVALPDFSTEP    = 0.1;

// Lower limit on PDF value in order to avoid division by zero.
const double SpaceShower::TINYPDF        = 1e-10;

// Lower limit on estimated evolution rate, below which stop.
const double SpaceShower::TINYKERNELPDF  = 1e-6;

// Lower limit on pT2, below which branching is rejected. 
const double SpaceShower::TINYPT2        = 0.25e-6;

// No attempt to do backwards evolution of a heavy (c or b) quark 
// if evolution starts at a scale pT2 < HEAVYPT2EVOL * mQ2.
const double SpaceShower::HEAVYPT2EVOL   = 1.1;

// No attempt to do backwards evolution of a heavy (c or b) quark 
// if evolution starts at a  x > HEAVYXEVOL * x_max, where 
// x_max is the largest possible x value for a g -> Q Qbar branching.
const double SpaceShower::HEAVYXEVOL     = 0.9;
  
// When backwards evolution Q -> g + Q creates a heavy quark Q,
// an earlier branching g -> Q + Qbar will restrict kinematics
// to  M_{Q Qbar}^2 > EXTRASPACEQ * 4 m_Q^2. (Smarter to be found??) 
const double SpaceShower::EXTRASPACEQ    = 2.0;

// Never pick pT so low that alphaS is evaluated too close to Lambda_3. 
const double SpaceShower::LAMBDA3MARGIN  = 1.1;

// Cutoff for f_e^e at x < 1 - 10^{-10} to be used in z selection.
// Note: the x_min quantity come from 1 - x_max.
const double SpaceShower::LEPTONXMIN     = 1e-10;
const double SpaceShower::LEPTONXMAX     = 1. - 1e-10;

// Stop l -> l gamma evolution slightly above m2l.
const double SpaceShower::LEPTONPT2MIN   = 1.2;

// Enhancement of l -> l gamma trial rate to compensate imperfect modelling.
const double SpaceShower::LEPTONFUDGE    = 10.;

//*********

// Initialize alphaStrong, alphaEM and related pTmin parameters.

void SpaceShower::init( BeamParticle* beamAPtrIn, 
  BeamParticle* beamBPtrIn) {

  // Store input pointers for future use. 
  beamAPtr        = beamAPtrIn;
  beamBPtr        = beamBPtrIn;

  // Main flags to switch on and off branchings.
  doQCDshower     = Settings::flag("SpaceShower:QCDshower");
  doQEDshowerByQ  = Settings::flag("SpaceShower:QEDshowerByQ");
  doQEDshowerByL  = Settings::flag("SpaceShower:QEDshowerByL");

  // Matching in pT of hard interaction to shower evolution.
  pTmaxMatch      = Settings::mode("SpaceShower:pTmaxMatch"); 
  pTdampMatch     = Settings::mode("SpaceShower:pTdampMatch"); 
  pTmaxFudge      = Settings::parm("SpaceShower:pTmaxFudge"); 
  pTdampFudge     = Settings::parm("SpaceShower:pTdampFudge"); 

  // Optionally force emissions to be ordered in rapidity/angle.
  doRapidityOrder = Settings::flag("SpaceShower:rapidityOrder");

  // Charm, bottom and lepton mass thresholds.
  mc              = ParticleDataTable::m0(4); 
  mb              = ParticleDataTable::m0(5); 
  m2c             = pow2(mc);
  m2b             = pow2(mb);

  // Parameters of alphaStrong generation.
  alphaSvalue     = Settings::parm("SpaceShower:alphaSvalue");
  alphaSorder     = Settings::mode("SpaceShower:alphaSorder");
  alphaS2pi       = 0.5 * alphaSvalue / M_PI;
  
  // Initialize alpha_strong generation.
  alphaS.init( alphaSvalue, alphaSorder); 
  
  // Lambda for 5, 4 and 3 flavours.
  Lambda5flav     = alphaS.Lambda5(); 
  Lambda4flav     = alphaS.Lambda4(); 
  Lambda3flav     = alphaS.Lambda3(); 
  Lambda5flav2    = pow2(Lambda5flav);
  Lambda4flav2    = pow2(Lambda4flav);
  Lambda3flav2    = pow2(Lambda3flav);
 
  // Regularization of QCD evolution for pT -> 0. Can be taken 
  // same as for multiple interactions, or be set separately.
  useSamePTasMI   = Settings::flag("SpaceShower:samePTasMI"); 
  if (useSamePTasMI) {
    pT0Ref        = Settings::parm("MultipleInteractions:pT0Ref");
    ecmRef        = Settings::parm("MultipleInteractions:ecmRef");
    ecmPow        = Settings::parm("MultipleInteractions:ecmPow");
    pTmin         = Settings::parm("MultipleInteractions:pTmin");
  } else {
    pT0Ref        = Settings::parm("SpaceShower:pT0Ref");
    ecmRef        = Settings::parm("SpaceShower:ecmRef");
    ecmPow        = Settings::parm("SpaceShower:ecmPow");
    pTmin         = Settings::parm("SpaceShower:pTmin");
  }

  // Calculate nominal invariant mass of events. Set current pT0 scale.
  sCM             = m2( beamAPtr->p(), beamBPtr->p());
  eCM             = sqrt(sCM);
  pT0             = pT0Ref * pow(eCM / ecmRef, ecmPow);

  // Restrict pTmin to ensure that alpha_s(pTmin^2 + pT_0^2) does not blow up. 
  pTmin = max( pTmin, sqrtpos(pow2(LAMBDA3MARGIN) * Lambda3flav2 - pT0*pT0) );

  // Parameters of alphaEM generation.
  alphaEMorder    = Settings::mode("SpaceShower:alphaEMorder");

  // Initialize alphaEM generation.
  alphaEM.init( alphaEMorder); 
 
  // Parameters of QED evolution.
  pTminChgQ       = Settings::parm("SpaceShower:pTminchgQ"); 
  pTminChgL       = Settings::parm("SpaceShower:pTminchgL"); 

  // Derived parameters of QCD evolution.
  pT20            = pow2(pT0);
  pT2min          = pow2(pTmin);
  pT2minChgQ      = pow2(pTminChgQ);
  pT2minChgL      = pow2(pTminChgL);

  // Various other parameters. 
  doMEcorrections = Settings::flag("SpaceShower:MEcorrections");
  doPhiPolAsym    = Settings::flag("SpaceShower:phiPolAsym");
  nQuarkIn        = Settings::mode("SpaceShower:nQuarkIn");

} 

//*********

// Find whether to limit maximum scale of emissions.

bool SpaceShower::limitPTmax( Event& event, double Q2Fac, double Q2Ren) {

  // Find whether to limit pT. Begin by user-set cases.
  bool dopTlimit = false;
  if      (pTmaxMatch == 1) dopTlimit = true;
  else if (pTmaxMatch == 2) dopTlimit = false;
   
  // Look if any quark (u, d, s, c, b), gluon or photon in final state. 
  else {
    for (int i = 5; i < event.size(); ++i) 
    if (event[i].status() != -21) {
      int idAbs = event[i].idAbs();
      if (idAbs <= 5 || idAbs == 21 || idAbs == 22) dopTlimit = true;
    }
  }

  // Dampening at factorization or renormalization scale.
  dopTdamp   = false;
  pT2damp    = 0.;
  if ( !dopTlimit && (pTdampMatch == 1 || pTdampMatch == 2) ) {
    dopTdamp = true;
    pT2damp  = pow2(pTdampFudge) * ((pTdampMatch == 1) ? Q2Fac : Q2Ren);
  }

  // Done.
  return dopTlimit;
 
}

//*********

// Prepare system for evolution; identify ME.
// Routine may be called after multiple interactions, for a new subystem.

void SpaceShower::prepare( int iSys, Event& event, bool limitPTmaxIn) {

  // Find positions of incoming colliding partons.
  int in1 = event.getInSystem( iSys, 0);
  int in2 = event.getInSystem( iSys, 1);

  // Reset dipole-ends list for first interaction.
  if (iSys == 0) dipEnd.resize(0);

  // Find matrix element corrections for system.
  int MEtype = findMEtype( iSys, event); 

  // Maximum pT scale for dipole ends.
  double pTmax1 = (limitPTmaxIn) ? event[in1].scale() : eCM;
  double pTmax2 = (limitPTmaxIn) ? event[in2].scale() : eCM;
  if (iSys == 0 && limitPTmaxIn) {
    pTmax1 *= pTmaxFudge;
    pTmax2 *= pTmaxFudge;
  }

  // Find dipole ends for QCD radiation.
  if (doQCDshower) {
    int colType1 = event[in1].colType();
    dipEnd.push_back( SpaceDipoleEnd( iSys,  1, in1, in2, pTmax1, 
      colType1, 0, MEtype) );
    int colType2 = event[in2].colType();
    dipEnd.push_back( SpaceDipoleEnd( iSys,  2, in2, in1, pTmax2, 
      colType2, 0, MEtype) );
  }

  // Find dipole ends for QED radiation.
  if (doQEDshowerByQ || doQEDshowerByL) {
    int chgType1 = ( (event[in1].isQuark() && doQEDshowerByQ)
      || (event[in1].isLepton() && doQEDshowerByL) )
      ? event[in1].chargeType() : 0;
    dipEnd.push_back( SpaceDipoleEnd( iSys, -1, in1, in2, pTmax1, 
      0, chgType1, MEtype) );
    int chgType2 = ( (event[in2].isQuark() && doQEDshowerByQ)
      || (event[in2].isLepton() && doQEDshowerByL) )
      ? event[in2].chargeType() : 0;
    dipEnd.push_back( SpaceDipoleEnd( iSys, -2, in2, in1, pTmax2, 
      0, chgType2, MEtype) );
  }

}

//*********
 
// Select next pT in downwards evolution of the existing dipoles.

double SpaceShower::pTnext( Event& , double pTbegAll, double pTendAll, 
  int nRadIn) {

  // Current cm energy, in case it varies between events.
  sCM           = m2( beamAPtr->p(), beamBPtr->p());

  // Starting values: no radiating dipole found.
  nRad          = nRadIn;
  double pT2sel = pow2(pTendAll);
  iDipSel       = 0;
  iSysSel       = 0;
  dipEndSel     = 0; 

  // Loop over all possible dipole ends.
  for (int iDipEnd = 0; iDipEnd < int(dipEnd.size()); ++iDipEnd) {
    iDipNow        = iDipEnd;
    dipEndNow      = &dipEnd[iDipEnd];        
    iSysNow        = dipEndNow->system;
    dipEndNow->pT2 = 0.;
   
    // Check whether dipole end should be allowed to shower. 
    double pT2begDip = pow2( min( pTbegAll, dipEndNow->pTmax ));
    if (pT2begDip > pT2sel 
      && ( dipEndNow->colType != 0 || dipEndNow->chgType != 0 ) ) {
      double pT2endDip = 0.;

      // Determine lower cut for evolution, for QCD or QED (q or l).      
      if (dipEndNow->colType != 0) pT2endDip = max( pT2sel, pT2min );   
      else if (abs(dipEndNow->chgType) != 3) pT2endDip 
        = max( pT2sel, pT2minChgQ );   
      else pT2endDip = max( pT2sel, pT2minChgL );  

      // Find properties of dipole and radiating dipole end.
      bool ordered = ( abs(dipEndNow->side) == 1 ); 
      BeamParticle& beamNow = (ordered) ? *beamAPtr : *beamBPtr;
      BeamParticle& beamRec = (ordered) ? *beamBPtr : *beamAPtr;
      iNow         = beamNow[iSysNow].iPos();
      iRec         = beamRec[iSysNow].iPos();
      idDaughter   = beamNow[iSysNow].id();
      xDaughter    = beamNow[iSysNow].x();
      x1Now        = (ordered) ? xDaughter : beamRec[iSysNow].x();
      x2Now        = (ordered) ? beamRec[iSysNow].x() : xDaughter;
      m2Dip        = x1Now * x2Now * sCM; 

      // Now do evolution in pT2, for QCD or QED 
      if (pT2begDip > pT2endDip) { 
        if (dipEndNow->colType != 0) pT2nextQCD( pT2begDip, pT2endDip);
        else                         pT2nextQED( pT2begDip, pT2endDip);
      }

      // Update if found larger pT than current maximum.
      if (dipEndNow->pT2 > pT2sel) {
        pT2sel    = dipEndNow->pT2;
        iDipSel   = iDipNow;
        iSysSel   = iSysNow;
        dipEndSel = dipEndNow;
      }

    // End loop over dipole ends.
    }
  } 

  // Return nonvanishing value if found pT is bigger than already found.
  return (dipEndSel == 0) ? 0. : sqrt(pT2sel); 
}

//*********

// Evolve a QCD dipole end. 

void SpaceShower::pT2nextQCD( double pT2begDip, double pT2endDip) { 

  // Some properties and kinematical starting values.
  BeamParticle& beam = ( abs(dipEndNow->side) == 1 ) 
                     ? *beamAPtr : *beamBPtr;
  bool   isGluon     = (idDaughter == 21);
  bool   isValence   = beam[iSysNow].isValence();
  int    MEtype      = dipEndNow->MEtype;
  double pT2         = pT2begDip;
  double xMaxAbs     = beam.xMax(iSysNow);
  double zMinAbs     = xDaughter / xMaxAbs;
  
  // Starting values for handling of massive quarks (c/b), if any.
  double idMassive   = 0;
  if ( abs(idDaughter) == 4 ) idMassive = 4;
  if ( abs(idDaughter) == 5 ) idMassive = 5;
  bool   isMassive   = (idMassive > 0);
  double m2Massive   = 0.;
  double mRatio      = 0.;
  double zMaxMassive = 1.;
  double m2Threshold = pT2;

  // Evolution below scale of massive quark or at large x is impossible.
  if (isMassive) { 
    m2Massive = (idMassive == 4) ? m2c : m2b;
    if (pT2 < HEAVYPT2EVOL * m2Massive) return;
    mRatio = sqrt( m2Massive / m2Dip );
    zMaxMassive = (1. -  mRatio) / ( 1. +  mRatio * (1. -  mRatio) ); 
    if (xDaughter > HEAVYXEVOL * zMaxMassive * xMaxAbs) return; 
  
    // Find threshold scale below which only g -> Q + Qbar will be allowed.
    m2Threshold = (idMassive == 4) ? min( pT2, CTHRESHOLD * m2c)
      : min( pT2, BTHRESHOLD * m2b); 
  }
  
  // Variables used inside evolution loop. (Mainly dummy starting values.)
  int    nFlavour       = 3; 
  double b0             = 4.5;
  double Lambda2        = Lambda3flav2;
  double pT2minNow      = pT2endDip; 
  int    idMother       = 0; 
  int    idSister       = 0;
  double z              = 0.;
  double zMaxAbs        = 0.;
  double zRootMax       = 0.;
  double zRootMin       = 0.;
  double g2gInt         = 0.; 
  double q2gInt         = 0.; 
  double q2qInt         = 0.;
  double g2qInt         = 0.;
  double g2Qenhance     = 0.;
  double xPDFdaughter   = 0.;
  double xPDFmother[21] = {0.};
  double xPDFgMother    = 0.;
  double xPDFmotherSum  = 0.;
  double kernelPDF      = 0.;
  double xMother        = 0.;
  double wt             = 0.;
  double Q2             = 0.;
  double mSister        = 0.;
  double m2Sister       = 0.;
  double pT2corr        = 0.;
  double phi            = 0.; 
  double pT2PDF         = pT2;
  bool   needNewPDF     = true;

  // Begin evolution loop towards smaller pT values.
  int    loopTinyPDFdau = 0;
  bool   hasTinyPDFdau  = false;
  do { 
    wt = 0.;

    // Bad sign if repeated looping with small daughter PDF, so fail.
    // (Example: if all PDF's = 0 below Q_0, except for c/b companion.)
    if (hasTinyPDFdau) ++loopTinyPDFdau;  
    if (loopTinyPDFdau > MAXLOOPTINYPDF) {
      infoPtr->errorMsg("Warning in SpaceShower::pT2nextQCD: "
      "small daughter PDF"); 
      return;
    }

    // Initialize integrals of splitting kernels and evaluate parton 
    // densities at the beginning. Reinitialize after long evolution 
    // in pT2 or when crossing c and b flavour thresholds.
    if (needNewPDF || pT2 < EVALPDFSTEP * pT2PDF) {
      pT2PDF        = pT2;
      hasTinyPDFdau = false;

      // Determine overestimated z range; switch at c and b masses.
      if (pT2 > m2b) {
        nFlavour  = 5;
        pT2minNow = m2b;
        b0        = 23./6.;
        Lambda2   = Lambda5flav2;
      } else if (pT2 > m2c) {
        nFlavour  = 4;
        pT2minNow = m2c;
        b0        = 25./6.;
        Lambda2   = Lambda4flav2;
      } else { 
        nFlavour  = 3;
        pT2minNow = pT2endDip;
        b0        = 27./6.;
        Lambda2   = Lambda3flav2;
      }
      zMaxAbs = 1. - 0.5 * (pT2minNow / m2Dip) *
        ( sqrt( 1. + 4. * m2Dip / pT2minNow ) - 1. );
      if (isMassive) zMaxAbs = min( zMaxAbs, zMaxMassive); 

      // Go to another z range with lower mass scale if current is closed.
      if (zMinAbs > zMaxAbs) { 
        if (nFlavour == 3 || (idMassive == 4 && nFlavour == 4) 
          || idMassive == 5) return;
        pT2 = (nFlavour == 4) ? m2c : m2b;
        continue;
      } 

      // Parton density of daughter at current scale. 
      xPDFdaughter = beam.xfISR(iSysNow, idDaughter, xDaughter, pT2);
      if (xPDFdaughter < TINYPDF) {
        xPDFdaughter  = TINYPDF;
        hasTinyPDFdau = true;
      }

      // Integrals of splitting kernels for gluons: g -> g, q -> g.
      if (isGluon) {
        g2gInt = 6. * log(zMaxAbs * (1.-zMinAbs) 
          / (zMinAbs * (1.-zMaxAbs)));
        if (doMEcorrections) g2gInt *= calcMEmax(MEtype, 21, 21);
        q2gInt = (16./3.) * (1./sqrt(zMinAbs) - 1./sqrt(zMaxAbs));
        if (doMEcorrections) q2gInt *= calcMEmax(MEtype, 1, 21);

        // Parton density of potential quark mothers to a g.
        xPDFmotherSum = 0.;
        for (int i = -nQuarkIn; i <= nQuarkIn; ++i) {
          if (i == 0) {
            xPDFmother[10] = 0.;
          } else {
            xPDFmother[i+10] = beam.xfISR(iSysNow, i, xDaughter, pT2); 
            xPDFmotherSum += xPDFmother[i+10]; 
          }
        } 

        // Total QCD evolution coefficient for a gluon.
        kernelPDF = g2gInt + q2gInt * xPDFmotherSum / xPDFdaughter;

      // For valence quark only need consider q -> q g branchings.
      // Introduce an extra factor sqrt(z) to smooth bumps.
      } else if (isValence) {
        zRootMin = (1. + sqrt(zMinAbs)) / (1. - sqrt(zMinAbs));
        zRootMax = (1. + sqrt(zMaxAbs)) / (1. - sqrt(zMaxAbs));
        q2qInt = (8./3.) * log( zRootMax / zRootMin );
        if (doMEcorrections) q2qInt *= calcMEmax(MEtype, 1, 1);
        g2qInt = 0.;
        kernelPDF = q2qInt; 

      // Integrals of splitting kernels for quarks: q -> q, g -> q.
      } else {
        q2qInt = (8./3.) * log( (1. - zMinAbs) / (1. - zMaxAbs) );
        if (doMEcorrections) q2qInt *= calcMEmax(MEtype, 1, 1);
        g2qInt = 0.5 * (zMaxAbs - zMinAbs);
        if (doMEcorrections) g2qInt *= calcMEmax(MEtype, 21, 1);

        // Increase estimated upper weight for g -> Q + Qbar.
        if (isMassive) {
          double m2log = log( m2Massive / Lambda2);
          g2Qenhance = log( log(pT2/Lambda2) / m2log ) 
            / log( log(m2Threshold/Lambda2) / m2log );
          g2qInt *= g2Qenhance;
	}

        // Parton density of a potential gluon mother to a q.
        xPDFgMother = beam.xfISR(iSysNow, 21, xDaughter, pT2);

        // Total QCD evolution coefficient for a quark.
        kernelPDF = q2qInt + g2qInt * xPDFgMother / xPDFdaughter;
      }

      // End evaluation of splitting kernels and parton densities.
      needNewPDF = false;
    }
    if (kernelPDF < TINYKERNELPDF) return;

    // Pick pT2 (in overestimated z range), for one of three different cases.
    // Assume form alphas(pT0^2 + pT^2) * dpT^2/(pT0^2 + pT^2).
    double Q2alphaS;

    // Fixed alpha_strong.
    if (alphaSorder == 0) {
      pT2 = (pT2 + pT20) * pow( Rndm::flat(), 
        1. / (alphaS2pi * kernelPDF)) - pT20;

    // First-order alpha_strong.
    } else if (alphaSorder == 1) {
      pT2 = Lambda2 * pow( (pT2 + pT20) / Lambda2, 
        pow(Rndm::flat(), b0 / kernelPDF) ) - pT20;

    // For second order reject by second term in alpha_strong expression.
    } else {
      do {
        pT2 = Lambda2 * pow( (pT2 + pT20) / Lambda2,
        pow(Rndm::flat(), b0 / kernelPDF) ) - pT20;
        Q2alphaS = max(pT2 + pT20, pow2(LAMBDA3MARGIN) * Lambda3flav2);
      } while (alphaS.alphaS2OrdCorr(Q2alphaS) < Rndm::flat()
        && pT2 > pT2minNow);
    }

    // Check for pT2 values that prompt special action.

    // If fallen into b threshold region, force g -> b + bbar.
    if (idMassive == 5 && pT2 < m2Threshold) {
      pT2nearQCDthreshold( beam, m2Massive, m2Threshold, zMinAbs, 
        zMaxMassive );
      return;

    // If crossed b threshold, continue evolution from this threshold.
    } else if (nFlavour == 5 && pT2 < m2b) {  
      needNewPDF = true;
      pT2 = m2b;
      continue;

    // If fallen into c threshold region, force g -> c + cbar.
    } else if (idMassive == 4 && pT2 < m2Threshold) {
      pT2nearQCDthreshold( beam, m2Massive, m2Threshold, zMinAbs, 
        zMaxMassive );
      return; 

    // If crossed c threshold, continue evolution from this threshold.
    } else if (nFlavour == 4 && pT2 < m2c) { 
      needNewPDF = true;
      pT2 = m2c;
      continue;

    // Abort evolution if below cutoff scale, or below another branching.
    } else if (pT2 < pT2endDip) return; 

    // Select z value of branching to g, and corrective weight.
    if (isGluon) {
      // g -> g (+ g). 
      if (Rndm::flat() * kernelPDF < g2gInt) {
        idMother = 21;
        idSister = 21;
        z = 1. / ( 1. + ((1. - zMinAbs) / zMinAbs) * pow( (zMinAbs * 
          (1. - zMaxAbs)) / (zMaxAbs * (1. - zMinAbs)), Rndm::flat() ) );
        wt = pow2( 1. - z * (1. - z));
      } else {
      // q -> g (+ q): also select flavour. 
        double temp = xPDFmotherSum * Rndm::flat();
        idMother = -nQuarkIn - 1;
        do { temp -= xPDFmother[(++idMother) + 10]; } 
        while (temp > 0. && idMother < nQuarkIn);  
        idSister = idMother;
        z = (zMinAbs * zMaxAbs) / pow2( sqrt(zMinAbs) + Rndm::flat() 
          * ( sqrt(zMaxAbs)- sqrt(zMinAbs) ));
        wt = 0.5 * (1. + pow2(1. - z)) * sqrt(z) 
          * xPDFdaughter / xPDFmother[idMother + 10];
      } 

    // Select z value of branching to q, and corrective weight.
    // Include massive kernel corrections for c and b quarks.
    } else {
      // q -> q (+ g). 
      if (isValence || Rndm::flat() * kernelPDF < q2qInt) {
        idMother = idDaughter;
        idSister = 21;
        // Valence more peaked at large z.
        if (isValence) {
          double zTmp = zRootMin * pow(zRootMax / zRootMin, Rndm::flat() );
          z = pow2( (1. - zTmp) / (1. + zTmp) );
        } else {
          z = 1. - (1. - zMinAbs) * pow( (1. - zMaxAbs) / (1. - zMinAbs),
            Rndm::flat() );
	} 
        if (!isMassive) { 
          wt = 0.5 * (1. + pow2(z));
	} else {
          wt = 0.5 * (1. + pow2(z) - z * pow2(1.-z) * m2Massive / pT2);
	}
        if (isValence) wt *= sqrt(z);
      // g -> q (+ qbar). 
      } else {
        idMother = 21;
        idSister = - idDaughter; 
        z = zMinAbs + Rndm::flat() * (zMaxAbs - zMinAbs);
        if (!isMassive) { 
          wt = (pow2(z) + pow2(1.-z)) * xPDFdaughter / xPDFgMother ;
	} else {
          wt = (pow2(z) + pow2(1.-z) + 2. * z * (1.-z) * m2Massive / pT2) 
            * xPDFdaughter / (xPDFgMother * g2Qenhance) ;
	}
      }
    }

    // Derive Q2 and x of mother from pT2 and z. 
    Q2 = pT2 / (1.- z);
    xMother = xDaughter / z;
 
    // Forbidden emission if outside allowed z range for given pT2.
    mSister = ParticleDataTable::m0(idSister);
    m2Sister = pow2(mSister);
    pT2corr = Q2 - z * (m2Dip + Q2) * (Q2 + m2Sister) / m2Dip;
    if(pT2corr < TINYPT2) { wt = 0.; continue; }

    // Optionally veto emissions not ordered in rapidity (= angle).
    if ( doRapidityOrder && dipEndNow->nBranch > 0
      && pT2 > pow2( (1. - z) / (z * (1. - dipEndNow->zOld)) ) 
      * dipEndNow->pT2Old ) { wt = 0.; continue; }

    // If creating heavy quark by Q -> g + Q then next need g -> Q + Qbar.
    // So minimum total mass2 is 4 * m2Sister, but use more to be safe.
    if ( isGluon && ( abs(idMother) == 4 || abs(idMother) == 5 )) {
      double m2QQsister =  EXTRASPACEQ * 4. * m2Sister;
      double pT2QQcorr = Q2 - z * (m2Dip + Q2) * (Q2 + m2QQsister) / m2Dip;
      if(pT2QQcorr < TINYPT2) { wt = 0.; continue; }
    }  

    // Select phi angle of branching at random.
    phi = 2. * M_PI * Rndm::flat();

    // Evaluation of ME correction.
    if (doMEcorrections) wt *= calcMEcorr(MEtype, idMother, idDaughter, 
      m2Dip, z, Q2) / calcMEmax(MEtype, idMother, idDaughter); 

    // Optional dampening of large pT values in first radiation.
    if (dopTdamp && iSysNow == 0 && MEtype == 0 && nRad == 0) 
      wt *= pT2damp / (pT2 + pT2damp);

    // Evaluation of new daughter and mother PDF's.
    double xPDFdaughterNew = max ( TINYPDF, 
      beam.xfISR(iSysNow, idDaughter, xDaughter, pT2) );
    double xPDFmotherNew = beam.xfISR(iSysNow, idMother, xMother, pT2);
    wt *= xPDFmotherNew / xPDFdaughterNew;

    // Check that valence step does not cause problem.
    if (wt > 1.) infoPtr->errorMsg("Warning in SpaceShower::"
      "pT2nextQCD: weight above unity"); 

  // Iterate until acceptable pT (or have fallen below pTmin).
  } while (wt < Rndm::flat()) ;

  // Save values for (so far) acceptable branching.
  dipEndNow->store( idDaughter,idMother, idSister, x1Now, x2Now, m2Dip,
    pT2, z, Q2, mSister, m2Sister, pT2corr, phi);  

}

//*********

// Evolve a QCD dipole end near threshold, with g -> Q + Qbar enforced.
// Note: No explicit Sudakov factor formalism here. Instead use that 
// df_Q(x, pT2) = (alpha_s/2pi) * (dT2/pT2) * ((gluon) * (splitting)).
// This implies that effects of Q -> Q + g are neglected in this range. 

void SpaceShower::pT2nearQCDthreshold( BeamParticle& beam, 
  double m2Massive, double m2Threshold, double zMinAbs, 
  double zMaxMassive) {

  // Initial values, to be used in kinematics and weighting.
  double Lambda2       = (abs(idDaughter) == 4) ? Lambda4flav2 : Lambda5flav2;
  double logM2Lambda2  = log( m2Massive / Lambda2 );
  double xPDFmotherOld = beam.xfISR(iSysNow, 21, xDaughter, m2Threshold);

  // Variables used inside evolution loop. (Mainly dummy start values.)
  int    loop    = 0;
  double wt      = 0.;
  double pT2     = 0.; 
  double z       = 0.; 
  double Q2      = 0.; 
  double pT2corr = 0.;

  // Begin loop over tries to find acceptable g -> Q + Qbar branching. 
  do { 
    wt = 0.;

    // Check that not caught in infinite loop with impossible kinematics.
    if (++loop > 100) { 
      infoPtr->errorMsg("Error in SpaceShower::pT2nearQCDthreshold: "
        "stuck in loop"); 
      return; 
    }

    // Pick dpT2/pT2 in range [m2Massive,thresholdRatio * m2Massive]. 
    pT2 = m2Massive * pow( m2Threshold / m2Massive, Rndm::flat() ); 

    // Pick z flat in allowed range.
    z = zMinAbs + Rndm::flat() * (zMaxMassive - zMinAbs);

    // Check that kinematically possible choice.
    Q2 = pT2 / (1.-z) - m2Massive;
    pT2corr = Q2 - z * (m2Dip + Q2) * (Q2 + m2Massive) / m2Dip;
    if(pT2corr < TINYPT2) continue;
    
    // Correction factor for running alpha_s.  ??
    wt = logM2Lambda2 / log( pT2 / Lambda2 ); 

    // Correction factor for splitting kernel.
    wt *= pow2(z) + pow2(1.-z) + 2. * z * (1.-z) * m2Massive / pT2;

    // Correction factor for gluon density.
    double xPDFmotherNew = beam.xfISR(iSysNow, 21, xDaughter/z, pT2);
    wt *= xPDFmotherNew / xPDFmotherOld;

  // Iterate until acceptable pT and z.
  } while (wt < Rndm::flat()) ;

  // Select phi angle of branching at random.
  double phi = 2. * M_PI * Rndm::flat();

  // Save values for (so far) acceptable branching.
  double mSister = (abs(idDaughter) == 4) ? mc : mb;  
  dipEndNow->store( idDaughter, 21, -idDaughter, x1Now, x2Now, m2Dip,
    pT2, z, Q2, mSister, pow2(mSister), pT2corr, phi);  

}

//*********

// Evolve a QED dipole end. 

void SpaceShower::pT2nextQED( double pT2begDip, double pT2endDip) { 

  // Type of dipole and starting values.
  BeamParticle& beam  = ( abs(dipEndNow->side) == 1 ) 
                      ? *beamAPtr : *beamBPtr;
  bool   isLeptonBeam = beam.isLepton();
  int    MEtype       = dipEndNow->MEtype;
  bool   isPhoton     = (idDaughter == 22);
  double pT2          = pT2begDip;
  double m2Lepton = (isLeptonBeam) ? pow2(beam.m()) : 0.; 
  if (isLeptonBeam && pT2begDip < m2Lepton) return;

  // Currently no f -> gamma branching implemented. ??
  if (isPhoton) return;

  // alpha_em at maximum scale provides upper estimate.
  double alphaEMmax  = alphaEM.alphaEM(pT2begDip);
  double alphaEM2pi  = alphaEMmax / (2. * M_PI);

  // Maximum x of mother implies minimum z = xDaughter / xMother.
  double xMaxAbs  = (isLeptonBeam) ? LEPTONXMAX : beam.xMax(iSysNow);
  double zMinAbs  = xDaughter / xMaxAbs;

  // Maximum z from minimum pT and, for lepton, from minimum x_gamma.
  double zMaxAbs = 1. - 0.5 * (pT2endDip / m2Dip) *
    ( sqrt( 1. + 4. * m2Dip / pT2endDip ) - 1. );
  if (isLeptonBeam) {
    double zMaxLepton = xDaughter / (xDaughter + LEPTONXMIN);
    if (zMaxLepton < zMaxAbs) zMaxAbs = zMaxLepton;
  }
  if (zMaxAbs < zMinAbs) return;

  // Integrals of splitting kernels for fermions: f -> f. Use 1 + z^2 < 2. 
  // Ansatz f(z) = 2 / (1 - z), with + 2 / (z - xDaughter) for lepton.
  double f2fInt  = 0.;
  double f2fIntA = 2. * log( (1. - zMinAbs) / (1. - zMaxAbs) );
  double f2fIntB = 0.;
  if (isLeptonBeam) {
    f2fIntB      = 2. * log( (zMaxAbs - xDaughter) / (zMinAbs - xDaughter) );
    f2fInt       = f2fIntA + f2fIntB; 
  } else f2fInt  = pow2(dipEndNow->chgType / 3.) * f2fIntA;

  // Upper estimate for evolution equation, including fudge factor. 
  if (doMEcorrections) f2fInt *= calcMEmax(MEtype, 1, 1);
  double kernelPDF = alphaEM2pi * f2fInt;
  double fudge = (isLeptonBeam) ? LEPTONFUDGE * log(m2Dip/m2Lepton) : 1.;
  kernelPDF *= fudge;
  if (kernelPDF < TINYKERNELPDF) return;
  
  // Variables used inside evolution loop. (Mainly dummy start values.)
  int    idMother = 0;
  double z        = 0.; 
  double xMother  = 0.; 
  double wt       = 0.; 
  double Q2       = 0.;
  double mSister  = 0.; 
  double m2Sister = 0.;
  double pT2corr  = 0.;
  double phi      = 0.;
  
  // Begin evolution loop towards smaller pT values.
  do { 
    wt = 0.;

    // Pick pT2 (in overestimated z range).
    // For l -> l gamma include extrafactor 1 / ln(pT2 / m2l) in evolution.
    double shift = pow(Rndm::flat(), 1. / kernelPDF);
    if (isLeptonBeam) pT2 = m2Lepton * pow( pT2 / m2Lepton, shift);
    else              pT2 = pT2 * shift; 

    // Abort evolution if below cutoff scale, or below another branching.
    if (pT2 < pT2endDip) return; 
    if (isLeptonBeam && pT2 < LEPTONPT2MIN * m2Lepton) return; 

    // Select z value of branching f -> f + gamma, and corrective weight.
    idMother = idDaughter;
    wt = 0.5 * (1. + pow2(z));
    if (isLeptonBeam) {
      if (f2fIntA > Rndm::flat() * (f2fIntA + f2fIntB)) 
        z = 1. - (1. - zMinAbs) 
        * pow( (1. - zMaxAbs) / (1. - zMinAbs), Rndm::flat() );
      else z = xDaughter + (zMinAbs - xDaughter) 
        * pow( (zMaxAbs - xDaughter) / (zMinAbs - xDaughter), Rndm::flat() );  
      wt *= (z - xDaughter) / (1. - xDaughter); 
    } else {
      z = 1. - (1. - zMinAbs) 
        * pow( (1. - zMaxAbs) / (1. - zMinAbs), Rndm::flat() ); 
    }
  
    // Derive Q2 and x of mother from pT2 and z. 
    Q2      = pT2 / (1. - z);
    xMother = xDaughter / z;
 
    // Forbidden emission if outside allowed z range for given pT2.
    mSister  = 0.;
    m2Sister = 0.;
    pT2corr  = Q2 - z * (m2Dip + Q2) * (Q2 + m2Sister) / m2Dip;
    if(pT2corr < TINYPT2) { wt = 0.; continue; }

    // Select phi angle of branching at random.
    phi = 2. * M_PI * Rndm::flat();
 
    // Correct by ln(pT2 / m2l) and fudge factor.  
    if (isLeptonBeam) wt *= log(pT2 / m2Lepton) / fudge;

    // Evaluation of ME correction.
    if (doMEcorrections) wt *= calcMEcorr(MEtype, idMother, idDaughter, 
      m2Dip, z, Q2) / calcMEmax(MEtype, idMother, idDaughter);

    // Optional dampening of large pT values in first radiation.
    if (dopTdamp && iSysNow == 0 && MEtype == 0 && nRad == 0) 
      wt *= pT2damp / (pT2 + pT2damp);

    // Correct to current value of alpha_EM.
    double alphaEMnow = alphaEM.alphaEM(pT2);
    wt *= (alphaEMnow / alphaEMmax);

    // Evaluation of new daughter and mother PDF's.
    double xPDFdaughterNew = max ( TINYPDF, 
      beam.xfISR(iSysNow, idDaughter, xDaughter, pT2) );
    double xPDFmotherNew   = beam.xfISR(iSysNow, idMother, xMother, pT2);
    wt *= xPDFmotherNew / xPDFdaughterNew;

  // Iterate until acceptable pT (or have fallen below pTmin).
  } while (wt < Rndm::flat()) ;

  // Save values for (so far) acceptable branching.
  dipEndNow->store( idDaughter,idMother, 22, x1Now, x2Now, m2Dip,
    pT2, z, Q2, mSister, m2Sister, pT2corr, phi);  

}

//*********

// Kinematics of branching.
// Construct mother -> daughter + sister, with recoiler on other side. 

bool SpaceShower::branch( Event& event) {

  // Side on which branching occured.
  int side          = abs(dipEndSel->side);
  double sideSign   = (side == 1) ? 1. : -1.;

  // Read in flavour and colour variables.
  int iDaughter     = event.getInSystem( iSysSel, side - 1);
  int iRecoiler     = event.getInSystem( iSysSel, 2 - side);
  int idDaughterNow = dipEndSel->idDaughter;
  int idMother      = dipEndSel->idMother;
  int idSister      = dipEndSel->idSister;
  int colDaughter   = event[iDaughter].col();
  int acolDaughter  = event[iDaughter].acol();

  // Read in kinematical variables.
  double x1         = dipEndSel->x1;
  double x2         = dipEndSel->x2;
  double m2         = dipEndSel->m2Dip;
  double m          = sqrt(m2);
  double pT2        = dipEndSel->pT2;
  double z          = dipEndSel->z;
  double Q2         = dipEndSel->Q2; 
  double mSister    = dipEndSel->mSister;
  double m2Sister   = dipEndSel->m2Sister;
  double pT2corr    = dipEndSel->pT2corr;
  double phi        = dipEndSel->phi;

  // Take copy of existing system, to be given modified kinematics.
  int eventSizeOld  = event.size();
  int systemSizeOld = event.sizeSystem(iSysSel);
  for ( int iCopy = 0; iCopy < systemSizeOld; ++iCopy) {
    int iOldCopy    = event.getInSystem( iSysSel, iCopy);
    int statusNew   = (iOldCopy == iDaughter 
      || iOldCopy == iRecoiler) ? event[iOldCopy].status() : 44;
    event.copy(iOldCopy, statusNew);
  }
 
  // Define colour flow in branching.
  // Default corresponds to f -> f + gamma.
  int colMother     = colDaughter;
  int acolMother    = acolDaughter;
  int colSister     = 0;
  int acolSister    = 0; 
  if (idSister == 22) ; 
  // q -> q + g and 50% of g -> g + g; need new colour.
  else if (idSister == 21 && ( (idMother > 0 && idMother < 9)
  || (idMother == 21 && Rndm::flat() < 0.5) ) ) {  
    colMother       = event.nextColTag();
    colSister       = colMother;
    acolSister      = colDaughter;
  // qbar -> qbar + g and other 50% of g -> g + g; need new colour.
  } else if (idSister == 21) {  
    acolMother      = event.nextColTag();
    acolSister      = acolMother;
    colSister       = acolDaughter;
  // q -> g + q.
  } else if (idDaughterNow == 21 && idMother > 0) { 
    colMother       = colDaughter;
    acolMother      = 0;
    colSister       = acolDaughter;
  // qbar -> g + qbar
  } else if (idDaughterNow == 21) {
    acolMother      = acolDaughter;
    colMother       = 0;
    acolSister      = colDaughter;
  // g -> q + qbar.
  } else if (idDaughterNow > 0 && idDaughterNow < 9) {
    acolMother      = event.nextColTag();
    acolSister      = acolMother;
  // g -> qbar + q.
  } else if (idDaughterNow < 0 && idDaughterNow > -9) {
    colMother       = event.nextColTag();
    colSister       = colMother;
  // q -> gamma + q.
  } else if (idDaughterNow == 22 && idMother > 0) {
    colMother       = event.nextColTag();
    colSister       = colMother; 
   // qbar -> gamma + qbar.
  } else if (idDaughterNow == 22) {
    acolMother      = event.nextColTag();
    acolSister      = acolMother;
  }   

  // Construct kinematics of mother, sister and recoiler in old rest frame.
  double pTbranch   = sqrt(pT2corr) * m2 / ( z * (m2 + Q2) );
  double pzMother   = sideSign * 0.5 * m * ( (m2 - Q2) / ( z * (m2 + Q2) )
    + (Q2 + m2Sister) / m2 ); 
  double eMother    = sqrt( pow2(pTbranch) + pow2(pzMother) );
  double pzSister   = pzMother - sideSign * 0.5 * (m2 + Q2) / m;
  double eSister    = sqrt( pow2(pTbranch) + pow2(pzSister) + m2Sister );
  double eNewRecoiler = 0.5 * (m2 + Q2) / m;
  Vec4 pMother( pTbranch, 0., pzMother, eMother );
  Vec4 pSister( pTbranch, 0., pzSister, eSister ); 
  Vec4 pNewRecoiler( 0., 0., -sideSign * eNewRecoiler, eNewRecoiler);

  // Indices of partons involved. Add new sister.
  int iMother       = eventSizeOld + side - 1;
  int iNewRecoiler  = eventSizeOld + 2 - side;
  int iSister       = event.append( idSister, 43, iMother, 0, 0, 0,
     colSister, acolSister, pSister, mSister, sqrt(pT2) );

  // References to the partons involved.
  Particle& daughter    = event[iDaughter];
  Particle& mother      = event[iMother];
  Particle& newRecoiler = event[iNewRecoiler];
  Particle& sister      = event.back();

  // Replace old by new mother; update new recoiler.
  mother.id( idMother );
  mother.status( -41);
  mother.cols( colMother, acolMother);
  mother.p( pMother);
  newRecoiler.status( -42);
  newRecoiler.p( pNewRecoiler);

  // Update mother and daughter pointers; also for beams.
  daughter.mothers( iMother, 0);
  mother.daughters( iSister, iDaughter); 
  if (iSysSel == 0) {
    event[1].daughter1( (side == 1) ? iMother : iNewRecoiler ); 
    event[2].daughter1( (side == 2) ? iMother : iNewRecoiler ); 
  }

  // Find boost to old rest frame, and rotation -phi.
  RotBstMatrix Mtot;
  Mtot.bst(0., 0., (x2 - x1) / (x1 + x2) );
  Mtot.rot(0., -phi); 

  // Find boost from old rest frame to event cm frame.
  RotBstMatrix MfromRest;
  // The boost to the new rest frame.
  Vec4 sumNew       = pMother + pNewRecoiler;
  double betaX      = sumNew.px() / sumNew.e();
  double betaZ      = sumNew.pz() / sumNew.e();
  MfromRest.bst( -betaX, 0., -betaZ);
  // Alignment of  radiator + recoiler to +- z axis, and rotation +phi.
  pMother.rotbst(MfromRest);  
  double theta = pMother.theta();
  if (side == 2) theta += M_PI;
  MfromRest.rot(-theta, phi); 
  // Longitudinal boost to radiator + recoiler in event cm frame.
  double x1New = (side == 1) ? x1 / z : x1;
  double x2New = (side == 2) ? x2 / z : x2;
  MfromRest.bst(0., 0., (x1New - x2New) / (x1New + x2New) );
  Mtot.rotbst(MfromRest);

  // Perform cumulative rotation/boost operation.
  // Mother, recoiler and sister from old rest frame to event cm frame.
  mother.rotbst(MfromRest);
  newRecoiler.rotbst(MfromRest);
  sister.rotbst(MfromRest);
  // The rest from (and to) event cm frame.
  for ( int i = eventSizeOld + 2; i < eventSizeOld + systemSizeOld; ++i) 
    event[i].rotbst(Mtot);  
 
  // Update list of partons in system; adding newly produced one.
  for ( int iCopy = 0; iCopy < systemSizeOld; ++iCopy) 
    event.setInSystem( iSysSel, iCopy, eventSizeOld + iCopy);
  event.addToSystem( iSysSel, eventSizeOld + systemSizeOld);

  // Update info on dipole ends (QCD or QED).
  for (int iDip = 0; iDip < int(dipEnd.size()); ++iDip)
  if ( dipEnd[iDip].system == iSysSel && abs(dipEnd[iDip].side) == side ) {    
    dipEnd[iDip].iRadiator = iMother;
    dipEnd[iDip].iRecoiler = iNewRecoiler;
    if (dipEnd[iDip].side > 0) 
      dipEnd[iDip].colType = mother.colType();
    else {
      dipEnd[iDip].chgType = 0;
      if ( (mother.isQuark() && doQEDshowerByQ)
        || (mother.isLepton() && doQEDshowerByL) ) 
        dipEnd[iDip].chgType = mother.chargeType();
    }
    // Kill ME corrections after first emission. 
    dipEnd[iDip].MEtype = 0;
  }

  // Update info on beam remnants.
  BeamParticle& beamNow = (side == 1) ? *beamAPtr : *beamBPtr;
  double xNew = (side == 1) ? x1New : x2New;
  beamNow[iSysSel].update( iMother, idMother, xNew);
  // Redo choice of companion kind whenever new flavour.
  if (idMother != idDaughterNow) {
    beamNow.xfISR( iSysSel, idMother, xNew, pT2);
    beamNow.pickValSeaComp();
  }
  BeamParticle& beamRec = (side == 1) ? *beamBPtr : *beamAPtr;
  beamRec[iSysSel].iPos( iNewRecoiler);

  // Store branching values of current dipole. (For rapidity ordering.)
  ++dipEndSel->nBranch;
  dipEndSel->pT2Old = pT2;
  dipEndSel->zOld   = z;


  // Done without any errors.
  return true;

}

//*********

// Find class of ME correction.

int SpaceShower::findMEtype( int iSys, Event& event) {

  // Default values and no action.
  int MEtype = 0; 
  if (!doMEcorrections) ;

  // Identify systems producing a single resonance.
  else if (event.sizeSystem( iSys) == 3) {
    int idIn1 = event[event.getInSystem( iSys, 0)].id();
    int idIn2 = event[event.getInSystem( iSys, 1)].id();
    int idRes = event[event.getInSystem( iSys, 2)].id();

    // f + fbar -> vector boson. 
    if ( (idRes == 23 || abs(idRes) == 24 || idRes == 32 
      || idRes == 33 || abs(idRes) == 34 || abs(idRes) == 41)
      && abs(idIn1) < 20 && abs(idIn2) < 20 ) MEtype = 1;

    // g + g, gamma + gamma  -> Higgs boson.
    if ( (idRes == 25 || idRes == 35 || idRes == 36) 
       && ( ( idIn1 == 21 && idIn2 == 21 ) 
       || ( idIn1 == 22 && idIn2 == 22 ) ) ) MEtype = 2; 

    // f + fbar  -> Higgs boson.
    if ( (idRes == 25 || idRes == 35 || idRes == 36) 
       && abs(idIn1) < 20 && abs(idIn2) < 20 ) MEtype = 3; 
  }

  // Done.
  return MEtype;

}

//*********

// Provide maximum of expected ME weight; for preweighting of evolution.

double SpaceShower::calcMEmax( int MEtype, int idMother, int idDaughterIn) {

  // Currently only one non-unity case: g(gamma) f -> V f'.
  if (MEtype == 1 && idMother > 20 && idDaughterIn < 20) return 3.;
  return 1.;

}  

//*********

// Provide actual ME weight for current branching.
// Note: currently ME corrections are only allowed for first branching 
// on each side, so idDaughter is essentially known and checks overkill.

double SpaceShower::calcMEcorr(int MEtype, int idMother, int idDaughterIn,
  double M2, double z, double Q2) {

  // Convert to Mandelstam variables. Sometimes may need to swap later.
  double sH = M2 / z;
  double tH = -Q2;
  double uH = Q2 - M2 * (1. - z) / z;
  int idMabs = abs(idMother);
  int idDabs = abs(idDaughterIn);

  // Corrections for f + fbar -> s-channel vector boson.
  if (MEtype == 1) {
    if (idMabs < 20 && idDabs < 20) {
      return (tH*tH + uH*uH + 2. * M2 * sH) / (sH*sH + M2*M2); 
    } else if (idDabs < 20) {
      // g(gamma) f -> V f': -Q2 = (p_g - p_f')^2 in PS while 
      // tHat = (p_f - p_f')^2 in ME so need to swap tHat <-> uHat.  
      swap( tH, uH); 
      return (sH*sH + uH*uH + 2. * M2 * tH) / (pow2(sH - M2) + M2*M2); 
    }

  // Corrections for g + g -> Higgs boson.
  } else if (MEtype == 2) {
    if (idMabs < 20 && idDabs > 20) {
      return (sH*sH + uH*uH) / (sH*sH + pow2(sH - M2)); 
    } else if (idDabs > 20) {
      return 0.5 * (pow4(sH) + pow4(tH) + pow4(uH) + pow4(M2)) 
        / pow2(sH*sH - M2 * (sH - M2)); 
    }    

  // Corrections for f + fbar -> Higgs boson (f = b mainly).
  } else if (MEtype == 3) {
    if (idMabs < 20 && idDabs < 20) {
      // The PS and ME answers agree for f fbar -> H g/gamma. 
      return 1.; 
    } else if (idDabs < 20) {
      // Need to swap tHat <-> uHat, cf. vector-boson production above. 
      swap( tH, uH); 
      return (sH*sH + uH*uH + 2. * (M2 - uH) * (M2 - sH)) 
             / (pow2(sH - M2) + M2*M2); 
    }    
  }

  return 1.;

}

//*********

// Print the list of dipoles.

void SpaceShower::list(ostream& os) {

  // Header.
  os << "\n --------  PYTHIA SpaceShower Dipole Listing  ---------- \n"
     << "\n    i  syst  side   rad   rec       pTmax  col  chg type \n" 
     << fixed << setprecision(3);
  
  // Loop over dipole list and print it.
  for (int i = 0; i < int(dipEnd.size()); ++i) 
  os << setw(5) << i << setw(6) << dipEnd[i].system 
     << setw(6) << dipEnd[i].side << setw(6) << dipEnd[i].iRadiator 
     << setw(6) << dipEnd[i].iRecoiler << setw(12) << dipEnd[i].pTmax 
     << setw(5) << dipEnd[i].colType << setw(5) << dipEnd[i].chgType
     << setw(5) << dipEnd[i].MEtype << "\n";

  // Done.
  os << "\n --------  End PYTHIA SpaceShower Dipole Listing  ------"
     << endl;
  

}
 
//**************************************************************************

} // end namespace Pythia8

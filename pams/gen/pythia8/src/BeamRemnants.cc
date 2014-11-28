// BeamRemnants.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Function definitions (not found in the header) for the 
// BeamRemnants class.

#include "BeamRemnants.h"

namespace Pythia8 {

//**************************************************************************

// The BeamDipole class is purely internal to reconnectColours.

class BeamDipole {

public:

  // Constructor.
  BeamDipole( int colIn = 0, int iColIn = 0, int iAcolIn = 0) 
    : col(colIn), iCol(iColIn), iAcol(iAcolIn) {}  

  // Members.
  int    col, iCol, iAcol;
  double p1p2;
 
};

//**************************************************************************

// The BeamRemnants class.

//*********

// Constants: could be changed here if desired, but normally should not.
// These are of technical nature, as described for each.

// Maximum number of tries to match colours and kinematics in the event.
const int    BeamRemnants::NTRYCOLMATCH   = 10; 
const int    BeamRemnants::NTRYKINMATCH   = 10;  

//*********

// Initialization.

bool BeamRemnants::init( Info* infoPtrIn, BeamParticle* beamAPtrIn, 
  BeamParticle* beamBPtrIn) {

  // Save pointers.
  infoPtr             = infoPtrIn;
  beamAPtr            = beamAPtrIn;
  beamBPtr            = beamBPtrIn;

  // Width of primordial kT distribution.
  doPrimordialKT      = Settings::flag("BeamRemnants:primordialKT");
  primordialKTsoft    = Settings::parm("BeamRemnants:primordialKTsoft");
  primordialKThard    = Settings::parm("BeamRemnants:primordialKThard");
  primordialKTremnant = Settings::parm("BeamRemnants:primordialKTremnant");
  halfScaleForKT      = Settings::parm("BeamRemnants:halfScaleForKT");
  halfMassForKT       = Settings::parm("BeamRemnants:halfMassForKT");

  // Parameters for colour reconnection scenario, partly borrowed from
  // multiple interactions not to introduce too many new ones.
  doReconnect         = Settings::flag("BeamRemnants:reconnectColours");
  reconnectRange      = Settings::parm("BeamRemnants:reconnectRange");
  pT0Ref              = Settings::parm("MultipleInteractions:pT0Ref");
  ecmRef              = Settings::parm("MultipleInteractions:ecmRef");
  ecmPow              = Settings::parm("MultipleInteractions:ecmPow");

  // Total and squared CM energy at nominal energy.
  eCM                 = infoPtr->eCM();
  sCM                 = eCM * eCM;

  // The MI pT0 smoothening scale and its reconnection-strength combination.
  pT0                 = pT0Ref * pow(eCM / ecmRef, ecmPow);
  pT20Rec             = pow2(reconnectRange * pT0); 
  
  // Done.
  return true;

}

//*********

// Select the flavours/kinematics/colours of the two beam remnants. 
// Notation: iPar = all partons, iSys = matched systems of two beams,
//           iRem = additional partons in remnants.

bool BeamRemnants::add( Event& event) {

  // Update to current CM energy.
  eCM     = infoPtr->eCM();
  sCM     = eCM * eCM;

  // Number of scattering subsystems. Size of event record before treatment.
  nSys    = event.sizeSystems();
  oldSize = event.size();

  // Add required extra remnant flavour content. 
  // Start all over if fails (in option where junctions not allowed).
  if ( !beamAPtr->remnantFlavours(event) 
    || !beamBPtr->remnantFlavours(event) ) {
    infoPtr->errorMsg("Error in BeamRemnants::add:"
      " remnant flavour setup failed"); 
    return false;
  }

  // Do the kinematics of the collision subsystems and two beam remnants
  if (!setKinematics(event)) return false;

  // Allow colour reconnections.
  if (doReconnect) reconnectColours(event);

  // Save current modifiable colour configuration for fast restoration. 
  vector<int> colSave;
  vector<int> acolSave;
  for (int i = oldSize; i < event.size(); ++i) {
    colSave.push_back( event[i].col() );
    acolSave.push_back( event[i].acol() );
  }
  event.saveJunctionSize();
  
  // Allow several tries to match colours of initiators and remnants.
  // Frequent "failures" since shortcutting colours separately on
  // the two event sides may give "colour singlet gluons" etc.
  bool physical = true;
  for (int iTry = 0; iTry < NTRYCOLMATCH; ++iTry) {
    physical = true;

    // Reset list of colour "collapses" (transformations).
    colFrom.resize(0);
    colTo.resize(0);      

    // First process each set of beam colours on its own.
    if (!beamAPtr->remnantColours(event, colFrom, colTo)) 
      physical = false;
    if (!beamBPtr->remnantColours(event, colFrom, colTo)) 
      physical = false;

    // Then check that colours and anticolours are matched in whole event.
    if ( physical && !checkColours(event) ) physical = false;     

    // If no problems then done, else restore and loop.
    if (physical) break;
    for (int i = oldSize; i < event.size(); ++i) 
      event[i].cols( colSave[i - oldSize], acolSave[i - oldSize] );
    event.restoreJunctionSize();
  }

  // If no solution after several tries then failed.
  if (!physical) {
    infoPtr->errorMsg("Error in BeamRemnants::add:"
      " colour tracing failed"); 
    return false;
  }

  // Done.
  return true;

}

//*********

// Set up trial transverse and longitudinal kinematics for each beam 
// separately. Final decisions involve comparing the two beams.

bool BeamRemnants::setKinematics( Event& event) {

  // References to beams to simplify indexing.
  BeamParticle& beamA = *beamAPtr;  
  BeamParticle& beamB = *beamBPtr;  

  // Nothing to do for lepton-lepton scattering with all energy already used.
  if ( beamA.isUnresolvedLepton() && beamB.isUnresolvedLepton() ) 
    return true;  

  // Allow primordial kT reduction for small-mass and small-pT systems
  // (for hardest interaction pT -> renormalization scale so also 2 -> 1).
  vector<double> kTwidth;
  vector<double> kTcomp;
  double kTcompSumSys = 0.;
  for (int iSys = 0; iSys < nSys; ++iSys) { 
    double kTwidthNow = 0.;
    double mHatDamp   = 1.;
    if (doPrimordialKT) {    
      double mHat     = sqrtpos( beamA[iSys].x() * beamB[iSys].x() * sCM );
      mHatDamp        = mHat / (mHat + halfMassForKT);
      double scale    = (iSys == 0) ? infoPtr->QRen() : infoPtr->pTMI(iSys);
      kTwidthNow      = ( (halfScaleForKT * primordialKTsoft
      + scale * primordialKThard) / (halfScaleForKT + scale) ) * mHatDamp;
    }
    kTwidth.push_back( kTwidthNow );
    kTcomp.push_back( mHatDamp );
    kTcompSumSys += mHatDamp;
  } 
  double kTwidthNow = (doPrimordialKT) ? primordialKTremnant : 0.;    
  for (int iRem = nSys; iRem < max( beamA.size(), beamB.size() ); ++iRem) { 
    kTwidth.push_back( kTwidthNow );     
    kTcomp.push_back( 1. );
  }     

  // Allow ten tries to construct kinematics (but normally works first).
  bool physical;
  double xSum[2], xInvM[2], w2Beam[2], wPosRem, wNegRem, w2Rem;
  for (int iTry = 0; iTry < NTRYKINMATCH; ++iTry) {
    physical = true;

    // Loop over the two beams. Sum px and py separately within each. 
    for (int iBeam = 0; iBeam < 2; ++iBeam) {
      BeamParticle& beam = (iBeam == 0) ? beamA : beamB;
      int nPar = beam.size();
      double pxSum = 0.;
      double pySum = 0.;
 
      // Loop over the partons in a beam. Generate Gaussian pT for hadrons.
      for (int iPar = 0; iPar < nPar; ++iPar) { 
        double px = 0.;
        double py = 0.;
        if (beam.isHadron() && doPrimordialKT) {
          px = kTwidth[iPar] * Rndm::gauss();
          py = kTwidth[iPar] * Rndm::gauss();
	}
        beam[iPar].px(px);
        beam[iPar].py(py);
        pxSum += px;
        pySum += py;
      }  

      // Share recoil between all partons.
      if (doPrimordialKT) {    
        double kTcompSum = kTcompSumSys + (nPar - nSys);
        for (int iPar = 0; iPar < nPar; ++iPar) {
          beam[iPar].px( beam[iPar].px() - pxSum * kTcomp[iPar] / kTcompSum );
          beam[iPar].py( beam[iPar].py() - pySum * kTcomp[iPar] / kTcompSum );
        }
      }

      // Pick unrescaled x values for remnants. Sum up (unscaled) p+ and p-.
      xSum[iBeam] = 0.;
      xInvM[iBeam] = 0.;
      for (int iRem = nSys; iRem < nPar; ++iRem) {
        double xPrel = beam.xRemnant( iRem);
        beam[iRem].x(xPrel);
        xSum[iBeam] += xPrel;
        xInvM[iBeam] += beam[iRem].mT2()/xPrel;     
      }

      // Squared transverse mass for each beam, using lightcone x.
      w2Beam[iBeam] = xSum[iBeam] * xInvM[iBeam];
  
    // End separate treatment of the two beams. 
    } 

    // Recalculate kinematics of initiator systems with primordial kT.
    wPosRem = eCM;
    wNegRem = eCM;
    for (int iSys = 0; iSys < nSys; ++iSys) { 
      double sHat = beamA[iSys].x() * beamB[iSys].x() * sCM;
      double sHatT = sHat 
        + pow2( beamA[iSys].px() + beamB[iSys].px()) 
        + pow2( beamA[iSys].py() + beamB[iSys].py()); 
      double rescale = sqrt( sHatT / sHat);
      wPosRem -= rescale * beamA[iSys].x() * eCM;
      wNegRem -= rescale * beamB[iSys].x() * eCM;

      // Kinematics forbids too large primordial kT.
      if (sqrt(sHatT) < beamA[iSys].pT() + beamB[iSys].pT()) 
        physical = false;
    }

    // Check that remaining momentum is enough for remnants.
    if (wPosRem < 0. || wNegRem < 0.) physical = false;
    w2Rem = wPosRem * wNegRem;
    if (sqrtpos(w2Rem) < sqrt(w2Beam[0]) + sqrt(w2Beam[1])) 
      physical = false;

    // End of loop over ten tries. Do not loop when solution found.  
    if (physical) break;
  }

  // If no solution after ten tries then failed.
  if (!physical) {
    infoPtr->errorMsg("Error in BeamRemnants::add:"
      " kinematics construction failed"); 
    return false;
  }

  // Construct energy and pz for each initiator pair.
  for (int iSys = 0; iSys < nSys; ++iSys) { 
    double sHat = beamA[iSys].x() * beamB[iSys].x() * sCM;
    double sHatT = sHat + pow2( beamA[iSys].px() + beamB[iSys].px()) 
      + pow2( beamA[iSys].py() + beamB[iSys].py()); 
    double rescale = sqrt( sHatT / sHat);
    double wPos = rescale * beamA[iSys].x() * eCM;
    double wNeg = rescale * beamB[iSys].x() * eCM;
    double w2A = beamA[iSys].mT2();
    double w2B = beamB[iSys].mT2();
    double lambdaRoot = sqrtpos( pow2( sHatT - w2A - w2B) - 4. * w2A * w2B );
    double pPosA = 0.5 * (sHatT + w2A - w2B + lambdaRoot) / sHatT * wPos;
    beamA[iSys].e( 0.5 * (pPosA + w2A / pPosA) );
    beamA[iSys].pz( 0.5 * (pPosA - w2A / pPosA) );
    double pNegB = 0.5 * (sHatT + w2B - w2A + lambdaRoot) / sHatT * wNeg;
    beamB[iSys].e( 0.5 * (pNegB + w2B / pNegB) );
    beamB[iSys].pz( 0.5 * (w2B / pNegB - pNegB) );

    // Construct rotations and boosts caused by primordial kT.
    int iA = beamA[iSys].iPos();
    int iB = beamB[iSys].iPos();
    RotBstMatrix M;
    M.toCMframe( event[iA].p(), event[iB].p() );
    M.fromCMframe( beamA[iSys].p(), beamB[iSys].p() );
 
    // Copy initiators and their systems and boost them accordingly.
    // Update subsystem and beams info on new positions of partons.
    int iAcopy = event.copy(iA, -61);
    event[iAcopy].rotbst(M);
    event.setInSystem(iSys, 0, iAcopy);
    beamA[iSys].iPos( iAcopy);
    int iBcopy = event.copy(iB, -61);
    event[iBcopy].rotbst(M);
    event.setInSystem(iSys, 1, iBcopy);
    beamB[iSys].iPos( iBcopy);
    for (int iABsys = 2; iABsys < event.sizeSystem(iSys); ++iABsys) {
      int iAB = event.getInSystem(iSys, iABsys);
      int iABcopy = event.copy(iAB, 62);
      event[iABcopy].rotbst(M); 
      event.setInSystem(iSys, iABsys, iABcopy);
    }

    // Update daughter info of mothers, i.e. of beams, for hardest interaction.
    if (iSys == 0) { 
      int mother = event[iAcopy].mother1();
      event[mother].daughter1(iAcopy);      
      mother = event[iBcopy].mother1();
      event[mother].daughter1(iBcopy);      
    }    
  }

  // Construct x rescaling factors for the two remants.
  double lambdaRoot = sqrtpos( pow2(w2Rem - w2Beam[0] - w2Beam[1])
    - 4. * w2Beam[0] * w2Beam[1] );
  double rescaleA = (w2Rem + w2Beam[0] - w2Beam[1] + lambdaRoot)
    / (2. * w2Rem * xSum[0]) ;
  double rescaleB = (w2Rem + w2Beam[1] - w2Beam[0] + lambdaRoot)
    / (2. * w2Rem * xSum[1]) ;

  // Construct energy and pz for remnants in first beam.
  for (int iRem = nSys; iRem < beamA.size(); ++iRem) {
    double pPos = rescaleA * beamA[iRem].x() * wPosRem;
    double pNeg = beamA[iRem].mT2() / pPos;
    beamA[iRem].e( 0.5 * (pPos + pNeg) );
    beamA[iRem].pz( 0.5 * (pPos - pNeg) );  

    // Add these partons to the normal event record.
    int iNew = event.append( beamA[iRem].id(), 63, 1, 0, 0, 0, 
      beamA[iRem].col(), beamA[iRem].acol(), beamA[iRem].p(), 
      beamA[iRem].m() );  
    beamA[iRem].iPos( iNew);
  }

  // Construct energy and pz for remnants in second beam.
  for (int iRem = nSys; iRem < beamB.size(); ++iRem) {
    double pNeg = rescaleB * beamB[iRem].x() * wNegRem;
    double pPos = beamB[iRem].mT2() / pNeg;
    beamB[iRem].e( 0.5 * (pPos + pNeg) );
    beamB[iRem].pz( 0.5 * (pPos - pNeg) );  

    // Add these partons to the normal event record. 
    int iNew = event.append( beamB[iRem].id(), 63, 2, 0, 0, 0, 
      beamB[iRem].col(), beamB[iRem].acol(), beamB[iRem].p(), 
      beamB[iRem].m() );  
    beamB[iRem].iPos( iNew);
  }

  // Done.
  return true;

}

//*********

// Allow colour reconnections by mergings of collision subsystems.
// iRec is system that may be reconnected, by moving its gluons to iSys,   
// where minimal pT (or equivalently Lambda) is used to pick location.
// Therefore all dipoles in iSys have to be found, and all gluons in iRec.
// Matching q-qbar pairs are treated by analogy with gluons.

bool BeamRemnants::reconnectColours( Event&  event) {

  // Prepare record of which systems should be merged onto another.
  // The iSys system must have colour in final state to attach to it.
  vector<int>  iMerge;
  vector<bool> hasColour;
  for (int iSys = 0; iSys < nSys; ++iSys) {
    iMerge.push_back( iSys );
    bool hasCol = false;
    for (int iMem = 2; iMem < event.sizeSystem( iSys); ++iMem) {
      int iNow = event.getInSystem( iSys, iMem);
      if (event[iNow].col() > 0 || event[iNow].acol() > 0) {
        hasCol = true;
        break;
      }    
    }
    hasColour.push_back( hasCol );
  }

  // Loop over system that may be reconnected. 
  for (int iRec = nSys - 1; iRec > 0; --iRec) {

    // Determine reconnection strength from pT scale of system.
    double pT2Rec  = pow2( infoPtr->pTMI(iRec) );
    double probRec = pT20Rec / (pT20Rec + pT2Rec); 

    // Loop over other systems iSys at higher pT scale and 
    // decide whether to reconnect the iRec gluons onto one of them.
    for (int iSys = iRec - 1; iSys >= 0; --iSys)
    if (hasColour[iSys] && probRec > Rndm::flat()) { 

      // The iRec system and all merged with it to be merged with iSys.
      iMerge[iRec] = iSys;       
      for (int iRec2 = iRec + 1; iRec2 < nSys; ++iRec2)
      if (iMerge[iRec2] == iRec) iMerge[iRec2] = iSys;    

      // Once a system has been merged do not test it anymore.
      break;
    }
  }

  // Loop over systems. Check whether other systems to be merged with it.
  for (int iSys = 0; iSys < nSys; ++iSys) {

    int nMerge = 0;
    for (int iRec = iSys + 1; iRec < nSys; ++iRec)
    if (iMerge[iRec] == iSys) ++nMerge;
    if (nMerge == 0) continue; 

    // Begin find dipoles in iSys system.
    vector<BeamDipole> dipoles;
    int sizeSys = event.sizeSystem( iSys);
    int iInASys = event.getInSystem( iSys, 0);
    int iInBSys = event.getInSystem( iSys, 1);    
    for (int iMem = 2; iMem < sizeSys; ++iMem) {

      // Find colour dipoles to beam remnant.
      int iNow = event.getInSystem( iSys, iMem);
      int col = event[iNow].col();  
      if (col > 0) {
        if (event[iInASys].col() == col)
          dipoles.push_back( BeamDipole( col, iNow, iInASys ) );
        else if (event[iInBSys].col() == col)
          dipoles.push_back( BeamDipole( col, iNow, iInBSys ) );
 
        // Find colour dipole between final-state partons.
        else for (int iMem2 = 2; iMem2 < sizeSys; ++iMem2) 
        if (iMem2 != iMem) {
          int iNow2 = event.getInSystem( iSys, iMem2); 
          if (event[iNow2].acol() == col) {
            dipoles.push_back( BeamDipole( col, iNow, iNow2) );
            break;
          }
        }
      }

      // Find anticolour dipoles to beam remnant.
      int acol = event[iNow].acol();  
      if (acol > 0) {
        if (event[iInASys].acol() == acol)
          dipoles.push_back( BeamDipole( acol, iInASys, iNow ) );
        else if (event[iInBSys].acol() == acol)
          dipoles.push_back( BeamDipole( acol, iInBSys, iNow ) ); 
      }
    }

    // Find dipole sizes.
    for (int iDip = 0; iDip < int(dipoles.size()); ++iDip) 
      dipoles[iDip].p1p2 = event[dipoles[iDip].iCol].p() 
                         * event[dipoles[iDip].iAcol].p();
    
    // Loop over systems iRec to be merged with iSys.
    for (int iRec = iSys + 1; iRec < nSys; ++iRec) {
      if (iMerge[iRec] != iSys) continue;

      // Information on iRec. Vectors for gluons and anything else.
      int sizeRec = event.sizeSystem( iRec);
      int iInARec = event.getInSystem( iRec, 0);
      int iInBRec = event.getInSystem( iRec, 1);    
      int nGluRec = 0;
      vector<int>    iGluRec;
      vector<double> pT2GluRec;
      int nAnyRec = 0;
      vector<int>    iAnyRec;
      vector<bool>   freeAnyRec;

      // Copy of gluon positions in descending order. 
      for (int iMem = 2; iMem < sizeRec; ++iMem) {
        int iNow = event.getInSystem( iRec, iMem);
        if (event[iNow].isGluon()) {
          ++nGluRec;
          iGluRec.push_back( iNow );  
          pT2GluRec.push_back( event[iNow].pT2() );
          for (int i = nGluRec - 1; i > 1; --i) {
            if (pT2GluRec[i - 1] > pT2GluRec[i]) break;
            swap(   iGluRec[i - 1],   iGluRec[i] );    
            swap( pT2GluRec[i - 1], pT2GluRec[i] ); 
	  }  
        // Copy of anything else, mainly quarks, in no particular order. 
        } else {
          ++nAnyRec;
          iAnyRec.push_back( iNow ); 
          freeAnyRec.push_back( true ); 
	}
      }

      // For each gluon in iRec now find the dipole that gives the smallest
      // (pGlu * pI) (pGlu * pJ) / (pI * pJ), i.e. minimal pT (and Lambda). 
      for (int iGRec = 0; iGRec < nGluRec; ++iGRec) {
        int    iGlu      = iGluRec[iGRec];
        Vec4   pGlu      = event[iGlu].p();
        int    iDipMin   = 0;
        double pT2DipMin = sCM;
        for (int iDip = 0; iDip < int(dipoles.size()); ++iDip) {
          double pT2Dip = (pGlu * event[dipoles[iDip].iCol].p())
            * (pGlu * event[dipoles[iDip].iAcol].p()) / dipoles[iDip].p1p2;
	  if (pT2Dip < pT2DipMin) {
            iDipMin   = iDip;
            pT2DipMin = pT2Dip;
	  }
        }  

        // Attach the gluon to the dipole, i.e. split the dipole in two.
        int colGlu   = event[iGlu].col();
        int acolGlu  = event[iGlu].acol();
        int colDip   = dipoles[iDipMin].col;
        int iColDip  = dipoles[iDipMin].iCol;
        int iAcolDip = dipoles[iDipMin].iAcol;
        event[iGlu].acol( colDip );
        if (event[iAcolDip].acol() == colDip) 
             event[iAcolDip].acol( colGlu );
        else event[iAcolDip].col(  colGlu ); 
        dipoles[iDipMin].iAcol = iGlu;
        dipoles[iDipMin].p1p2 = event[iColDip].p() * pGlu;
        dipoles.push_back( BeamDipole( colGlu, iGlu, iAcolDip ) );
        dipoles.back().p1p2 = pGlu * event[iAcolDip].p();
     
        // Remove gluon from old system: reconnect colours. 
        if      (event[iInARec].col() == colGlu) 
          event[iInARec].col( acolGlu );
        else if (event[iInBRec].col() == colGlu) 
          event[iInBRec].col( acolGlu );
        else for (int i = iGRec + 1; i < nGluRec; ++i) 
        if (event[iGluRec[i]].acol() == colGlu) {
          event[iGluRec[i]].acol( acolGlu );
          break;
	}
        for (int i = 0; i < nAnyRec; ++i) 
        if (event[iAnyRec[i]].acol() == colGlu) {
          event[iAnyRec[i]].acol( acolGlu );
          break;
	}
      }

      // See if any matching quark-antiquark pairs among the rest.
      for (int iQRec = 0; iQRec < nAnyRec; ++iQRec) {
        int iQ  = iAnyRec[iQRec];
        int idQ = event[iQ].id();
        if (freeAnyRec[iQRec] && idQ > 0 && idQ < 6) 
        for (int iQbarRec = 0; iQbarRec < nAnyRec; ++iQbarRec) {
          int iQbar  = iAnyRec[iQbarRec];
          if (freeAnyRec[iQbarRec] && event[iQbar].id() == -idQ) {

            // Check that these can be traced back to same gluon splitting.
            int iTopQ    = event.iTopCopyId(iQ);
            int iTopQbar = event.iTopCopyId(iQbar);
            int iMother  = event[iTopQ].mother1();
            if (event[iTopQbar].mother1() == iMother 
              && event[iMother].isGluon()) {

              // Now find the dipole that gives the smallest
              // ((pQ + pQbar) * pI) ((pQ + pQbar) * pJ) / (pI * pJ). 
              Vec4   pGlu      = event[iQ].p() + event[iQbar].p();
              int    iDipMin   = 0;
              double pT2DipMin = sCM;
              for (int iDip = 0; iDip < int(dipoles.size()); ++iDip) {
                double pT2Dip = (pGlu * event[dipoles[iDip].iCol].p())
                  * (pGlu * event[dipoles[iDip].iAcol].p()) 
                  / dipoles[iDip].p1p2;
                if (pT2Dip < pT2DipMin) {
                  iDipMin   = iDip;
                  pT2DipMin = pT2Dip;
	        }
              }  

              // Attach the q-qbar pair to the dipole, i.e. split the dipole.
              int colGlu   = event[iQ].col();
              int acolGlu  = event[iQbar].acol();
              int colDip   = dipoles[iDipMin].col;
              int iColDip  = dipoles[iDipMin].iCol;
              int iAcolDip = dipoles[iDipMin].iAcol;
              event[iQbar].acol( colDip );
              if (event[iAcolDip].acol() == colDip) 
                   event[iAcolDip].acol( colGlu );
              else event[iAcolDip].col(  colGlu ); 
              dipoles[iDipMin].iAcol = iQbar;
              dipoles[iDipMin].p1p2 = event[iColDip].p() * event[iQbar].p();
              dipoles.push_back( BeamDipole( colGlu, iQ, iAcolDip ) );
              dipoles.back().p1p2 = event[iQ].p() * event[iAcolDip].p();
     
              // Remove q-qbar pair from old system: reconnect colours. 
              freeAnyRec[iQRec]    = false;
              freeAnyRec[iQbarRec] = false;
              if      (event[iInARec].col() == colGlu) 
                event[iInARec].col( acolGlu );
              else if (event[iInBRec].col() == colGlu) 
                event[iInBRec].col( acolGlu );
              else for (int i = 0; i < nAnyRec; ++i) 
              if (freeAnyRec[i] && event[iAnyRec[i]].acol() == colGlu) {
                event[iAnyRec[i]].acol( acolGlu );
                break;
	      }

            // Done with processing of q-qbar pairs.
	    }
	  }
	}
      }

      // If only two beam gluons left of system, set their colour = anticolour.
      // Used by BeamParticle::remnantColours to skip irrelevant gluons.
      if ( event[iInARec].isGluon() && event[iInBRec].isGluon() 
        && event[iInARec].col() == event[iInBRec].acol() 
        && event[iInARec].acol() == event[iInBRec].col() ) { 
          event[iInARec].acol( event[iInARec].col() );
          event[iInBRec].acol( event[iInBRec].col() );
      }

    // End of loops over iRec and iSys systems.
    }
  }

  // Done.
  return true;    

}

//*********

// Collapse colours and check that they are consistent.

bool BeamRemnants::checkColours( Event& event) {

  // No colours in lepton beams so no need to do anything.
  if (beamAPtr->isLepton() && beamBPtr->isLepton()) return true;

  // Remove ambiguities when one colour collapses two ways.
  // Resolve chains where one colour is mapped to another.
  for (int iCol = 1; iCol < int(colFrom.size()); ++iCol) 
  for (int iColRef = 0; iColRef < iCol; ++iColRef) { 
    if (colFrom[iCol] == colFrom[iColRef]) {
      colFrom[iCol] = colTo[iCol];
      colTo[iCol] = colTo[iColRef]; 
    }
    if (colTo[iCol] == colFrom[iColRef]) colTo[iCol] = colTo[iColRef];
  }   
  
  // Transform event record colours from beam remnant colour collapses.
  for (int i = oldSize; i < event.size(); ++i) { 
    int col = event[i].col();
    int acol = event[i].acol(); 
    for (int iCol = 0; iCol < int(colFrom.size()); ++iCol) {
      if (col == colFrom[iCol]) {col = colTo[iCol]; event[i].col(col);} 
      if (acol == colFrom[iCol]) {acol = colTo[iCol]; event[i].acol(acol);} 
    }
  }

  // Transform junction colours from beam remnant colour collapses.
  for (int iJun = 0; iJun < event.sizeJunction(); ++iJun)
  for (int leg = 0; leg < 3; ++leg) {
    int col = event.colJunction(iJun, leg); 
    for (int iCol = 0; iCol < int(colFrom.size()); ++iCol) 
    if (col == colFrom[iCol]) {
      col = colTo[iCol]; 
      event.colJunction(iJun, leg, col);
    } 
  }

  // Arrays for current colours and anticolours, and for singlet gluons.
  vector<int> colList;
  vector<int> acolList;
  vector<int> iSingletGluon;

  // Find current colours and anticolours in the event record.
  for (int i = oldSize; i < event.size(); ++i) 
  if (event[i].status() > 0) {
    int id   = event[i].id();
    int col  = event[i].col();
    int acol = event[i].acol(); 

    // Quarks must have colour set, antiquarks anticolour, gluons both.
    if ( (id > 0 && id < 9 && (col <= 0 || acol != 0) )
      || (id < 0 && id > -9 && (col != 0 || acol <= 0) )
      || (id == 21 && (col <= 0 || acol <= 0) ) ) {
      infoPtr->errorMsg("Error in BeamRemnants::checkColours: "
        "q/qbar/g has wrong colour slots set");
      return false;
    }

    // Save colours/anticolours, and position of colour singlet gluons.
    if ( col > 0)  colList.push_back(  col );
    if (acol > 0) acolList.push_back( acol );
    if (col > 0 && acol == col) iSingletGluon.push_back(i);
  }

  // Run though list of singlet gluons and put them on final-state dipole
  // (i,j) that offers smallest (p_g p_i) * (p_g p_j) / (p_i p_j).
  for (int iS = 0; iS < int(iSingletGluon.size()); ++iS) {
    int    iGlu      = iSingletGluon[iS];
    int    iAcolDip  = 0;
    double pT2DipMin = sCM;
    for (int iC = oldSize; iC < event.size(); ++iC) 
    if (iC != iGlu && event[iC].status() > 0) {
      int colDip = event[iC].col();
      if (colDip > 0 && event[iC].acol() !=colDip)
      for (int iA = oldSize; iA < event.size(); ++iA)
      if (iA != iGlu && iA != iC && event[iA].status() > 0 
        && event[iA].acol() == colDip && event[iA].col() !=colDip) {
        double pT2Dip = (event[iGlu].p() * event[iC].p()) 
          * (event[iGlu].p() * event[iA].p())
          / (event[iC].p() * event[iA].p());
	if (pT2Dip < pT2DipMin) {
          iAcolDip  = iA;
          pT2DipMin = pT2Dip; 
	}
      }
    }
    event[iGlu].acol( event[iAcolDip].acol() );
    event[iAcolDip].acol( event[iGlu].col() );
  }

  // Check that not the same colour or anticolour appears twice.
  for (int iCol = 0; iCol < int(colList.size()) - 1; ++iCol) {
    int col = colList[iCol];
    for (int iCol2 = iCol + 1; iCol2 < int(colList.size()); ++iCol2) 
      if (colList[iCol2] == col) return false;
  }
  for (int iAcol = 0; iAcol < int(acolList.size()) - 1; ++iAcol) {
    int acol = acolList[iAcol];
    for (int iAcol2 = iAcol + 1; iAcol2 < int(acolList.size()); ++iAcol2) 
      if (acolList[iAcol2] == acol) return false;
  }

  // Remove all matching colour-anticolour pairs.
  bool foundPair = true;
  while (foundPair && colList.size() > 0 && acolList.size() > 0) {
    foundPair = false;
    for (int iCol = 0; iCol < int(colList.size()); ++iCol) {
      for (int iAcol = 0; iAcol < int(acolList.size()); ++iAcol) {
	if (acolList[iAcol] == colList[iCol]) { 
          colList[iCol] = colList.back(); colList.pop_back();     
          acolList[iAcol] = acolList.back(); acolList.pop_back();     
          foundPair = true; break;
	}
      } if (foundPair) break;
    }
  } 

  // Check that remaining (anti)colours are accounted for by junctions.
  for (int iJun = 0; iJun < event.sizeJunction(); ++iJun) {
    int kindJun = event.kindJunction(iJun);
    for (int leg = 0; leg < 3; ++leg) {
      int colEnd = event.colJunction(iJun, leg); 

      // Junction connected to three colours.
      if (kindJun == 1) {
        bool foundCol = false;
        for (int iCol = 0; iCol < int(colList.size()); ++iCol) 
        if (colList[iCol] == colEnd) { 
          colList[iCol] = colList.back(); 
          colList.pop_back();     
          foundCol = true; 
          break;
        }  
      } 

      // Junction connected to three anticolours.
      else if (kindJun == 2) {
        bool foundCol = false;
        for (int iAcol = 0; iAcol < int(acolList.size()); ++iAcol) 
	if (acolList[iAcol] == colEnd) { 
          acolList[iAcol] = acolList.back(); 
          acolList.pop_back();     
          foundCol = true; 
          break;
	}
      }

    // End junction check. More junction cases to come??
    }
  }

  // Done.
  return (colList.size() == 0 && acolList.size() == 0);

}

//**************************************************************************

} // end namespace Pythia8

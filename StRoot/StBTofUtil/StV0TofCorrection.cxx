// $Id: StV0TofCorrection.cxx,v 1.1 2011/01/24 21:01:49 geurts Exp $
/*! \file StV0TofCorrection.cxx
    \brief cxx file of StV0TofCorrection class created by Patrick Huck (phuck@lbl.gov) [12/13/2010]

    A detailed class description and example analysis results can be found at 
    http://drupal.star.bnl.gov/STAR/system/files/stv0tofcorrection_v3.pdf
*/
// ---
// $Log: StV0TofCorrection.cxx,v $
// Revision 1.1  2011/01/24 21:01:49  geurts
// *** empty log message ***
//
// ---
#include "StV0TofCorrection.h"
#include "TError.h"

Float_t *PathAB; //!< array of path lengths
Float_t *TofAB; //!< array of time-of-flights
const Float_t low_lims[3] = {0.015,0.2,0.8085}; //!< hardcoded default lower m2 limits for pi,K,p
const Float_t upp_lims[3] = {0.025,0.3,1.15}; //!< hardcoded default upper m2 limits for pi,K,p

ClassImp(StV0TofCorrection)
StV0TofCorrection::StV0TofCorrection() {
  clight = 29.9792458; 
  NrDecays = 0;
  cleared = kTRUE;
  cleared2 = kTRUE;
  Vectors3D = NULL;
  tracks = NULL;
}
StV0TofCorrection::~StV0TofCorrection() { }

/*! calculates TOF of trackAB for AB -> A + B with Pathlength PathAB */
inline Float_t StV0TofCorrection::calcTof(StLorentzVectorD trackAB, Float_t PathAB) {
  StThreeVectorD mom = trackAB.vect();
  Float_t MomentumAB = TMath::Sqrt(mom.x()*mom.x()+mom.y()*mom.y()+mom.z()*mom.z()); 
  Float_t BetaAB = MomentumAB/trackAB.e();
  return PathAB / (BetaAB*clight);
}

/*! calculates pathlength on helixA from dca of point space_vec
    adapted from function written by A. Schmah */
Float_t StV0TofCorrection::calcPathOnHelixToPoint(StPhysicalHelixD helixA, StThreeVectorD space_vec) {
  Float_t pA[2] = {0.0,-100.0}; //!< the two start values for pathB, 0.0 is the origin of the helix at the first measured point
  Float_t distarray[2];
  StThreeVectorD testA;
  for(Int_t r = 0; r < 2; r++) {
    testA     = helixA.at(pA[r]); // 3D-vector of helixA at path pA[r]
    distarray[r] = (testA-space_vec).mag(); // dca between helixA and helixB
  }
  Int_t loopcounter = 0;
  Float_t scale = 1.0;
  Float_t flip  = 1.0; // checks if the minimization direction changed
  Float_t scale_length = 100.0;

  while(fabs(scale_length) > 0.05 && loopcounter < 100) {
    if(distarray[0] > distarray[1]) { // stops when the length is too small
      if(loopcounter != 0) {
        // if minimization direction changes -> go back, but only the way * 0.4
        if(flip == 1.0) scale = 0.4;
        else scale = 0.7; // go on in this direction but only by the way * 0.7 */
      }
      scale_length = (pA[1]-pA[0])*scale; // the next length interval
      pA[0]     = pA[1] + scale_length; // the new path
      testA     = helixA.at(pA[0]); // 3D-vector of helixA at path pA[0]
      distarray[0] = (testA-space_vec).mag(); // new dca
      flip = 1.0;
    }
    else {
      if(loopcounter != 0) {
        if(flip == -1.0) scale = 0.4;
        else scale = 0.7;
      }
      scale_length = (pA[0]-pA[1])*scale;
      pA[1]     = pA[0] + scale_length;
      testA     =  helixA.at(pA[1]); // 3D-vector of helixA at path pA[0]
      distarray[1] = (testA-space_vec).mag();
      flip = -1.0;
    }
    loopcounter++;
  }

  if(distarray[0] < distarray[1]) return pA[0];
  else return pA[1];
}


/*! calculates PathLength of charged trackA (helixA) from vectorAB to vectorTof */
Float_t StV0TofCorrection::calcPathCorr(StPhysicalHelixD helixA, StThreeVectorD vectorAB, StThreeVectorD vectorTof) {
  Float_t path_tof = calcPathOnHelixToPoint(helixA,vectorTof);
  Float_t path_dec = calcPathOnHelixToPoint(helixA,vectorAB);
  return path_tof-path_dec;
}

/*! check consistency of initialization */
inline Bool_t StV0TofCorrection::inputOk() {
  if ( Vectors3D != NULL ) {
    if ( tracks != NULL ) {
      if ( (Vectors3D->getNrArgs()-2) == (tracks->getNrArgs()) )
        return kTRUE;
      else {
        Error("StV0TofCorrection::correctBeta","number of 3d-vectors not consistent to number of tracks!");
        return kFALSE;
      }
    }
    else {
      Error("StV0TofCorrection::correctBeta","You forgot to call setMotherTracks()!");
      return kFALSE;
    }
  }
  else {
    Error("StV0TofCorrection::correctBeta","You forgot to call setVectors3D()!");
    return kFALSE;
  }
}  

/*! cuts only for final daughter particles !
    default cut on m2 for pi,K,p */
inline Bool_t StV0TofCorrection::cutOnMass2(Float_t Mass2, Int_t pidnr) {
 if ( Mass2 > low_lims[pidnr] && Mass2 < upp_lims[pidnr] )
    return kTRUE;
  else return kFALSE;
}

/*! user has to care for beta being corrected only if tof hit exists !! */
Bool_t StV0TofCorrection::correctBeta(StPhysicalHelixD helixA, Float_t TofA, Float_t& BetaCorr, Float_t MomentumA, Int_t pidnr) {
  if ( inputOk() ) {
    setNrDecays(Vectors3D->getNrArgs()-2);
    PathAB = new Float_t[NrDecays]; TofAB = new Float_t[NrDecays];
    Float_t TofCorr = TofA;
    for ( Int_t i=0; i < NrDecays; i++ ) {
      // assume straight tracks for all mother particles
      PathAB[i] = ( (*Vectors3D)[i+1] - (*Vectors3D)[i] ).mag();
      TofAB[i]  = calcTof((*tracks)[i], PathAB[i]);
      TofCorr -= TofAB[i]; // TOF CORRECTION
    }
    Float_t PathCorr = calcPathCorr(helixA,(*Vectors3D)[NrDecays],(*Vectors3D)[NrDecays+1]);
    BetaCorr = PathCorr / (TofCorr*clight);
    if ( pidnr > -1 ) {
      // cut on new particle mass
      Float_t Mass2Corr = MomentumA * MomentumA * (1./(BetaCorr*BetaCorr)-1.); 
      return cutOnMass2(Mass2Corr,pidnr);
    }
    else return kTRUE; 
  }
  else {
    Error("StV0TofCorrection::correctBeta","check your Input!! BetaCorr set to -998.!");
    BetaCorr = -998.;
    return kFALSE;
  }
}


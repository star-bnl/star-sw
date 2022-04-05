// $Id: StV0TofCorrection.cxx,v 1.3 2017/10/20 17:50:33 smirnovd Exp $
/*! \file StV0TofCorrection.cxx
    \brief cxx file of StV0TofCorrection class created by Patrick Huck (phuck@lbl.gov) [12/13/2010]

    A detailed class description and example analysis results can be found at 
    http://drupal.star.bnl.gov/STAR/system/files/stv0tofcorrection_v3.pdf
*/
// ---
// $Log: StV0TofCorrection.cxx,v $
// Revision 1.3  2017/10/20 17:50:33  smirnovd
// Squashed commit of the following:
//
//     StBTof: Remove outdated ClassImp macro
//
//     Prefer explicit namespace for std:: names in header files
//
//     Removed unnecessary specification of default std::allocator
//
// Frank signed-off
//
// Revision 1.2  2014/01/20 16:55:50  geurts
// Major update by Patrick Huck:
// - fixed memory leaks, clean-up
// - switch from a custom closest-points-of-approach-finder to the StHelix::pathLengths()
// - added a public helper function calcM2() to calculate mass^2 after TOF correction
//
// Revision 1.1  2011/01/24 21:01:49  geurts
// *** empty log message ***
//
// ---
#include "StV0TofCorrection.h"
#include "TError.h"


M2CutType StV0TofCorrection::mM2CutMap = StV0TofCorrection::createM2CutMap();

StV0TofCorrection::StV0TofCorrection()
: clight(29.9792458), cleared(kTRUE), cleared2(kTRUE),
  NrDecays(0), Vectors3D(NULL), tracks(NULL) { }
StV0TofCorrection::~StV0TofCorrection() { }

/*! calculates TOF of trackAB for AB -> A + B with Pathlength PathAB */
inline Float_t StV0TofCorrection::calcTof(
    const StLorentzVectorD& trackAB, const Float_t& PathAB
    ) {
  StThreeVectorD mom = trackAB.vect();
  Float_t BetaAB = mom.mag() / trackAB.e() * clight;
  return PathAB / BetaAB;
}

/*! check consistency of initialization */
inline Bool_t StV0TofCorrection::inputOk() {
  if ( Vectors3D == NULL ) {
    Error("StV0TofCorrection::correctBeta","setVectors3D() call missing!");
    return kFALSE;
  }
  if ( tracks == NULL ) {
    Error("StV0TofCorrection::correctBeta", "setMotherTracks() call missing!");
    return kFALSE;
  }
  if ( Vectors3D->getNrArgs()-2 != tracks->getNrArgs() ) {
    Error(
	"StV0TofCorrection::correctBeta",
	"number of 3d-vectors not consistent to number of tracks!"
	);
    return kFALSE;
  }
  return kTRUE;
}  

/*! cuts only for final daughter particles! Default cut on m2 for pi,K,p */
inline Bool_t StV0TofCorrection::cutOnMass2(
    const Float_t& Mass2, const std::string& particle
    ) {
  return (
      Mass2 > mM2CutMap[particle].first &&
      Mass2 < mM2CutMap[particle].second
      );
}

/*! user has to care for beta being corrected only if tof hit exists !! */
Bool_t StV0TofCorrection::correctBeta(
    const StPhysicalHelixD& helixA, const Float_t& TofA, Float_t& BetaCorr,
    const Float_t& mom, const std::string& particle
    ) {
  if ( inputOk() ) {
    NrDecays = Vectors3D->getNrArgs()-2;
    Float_t TofCorr = TofA;
    for ( Int_t i = 0; i < NrDecays; i++ ) {
      // assume straight tracks for all mother particles
      Float_t PathAB = ( (*Vectors3D)[i+1] - (*Vectors3D)[i] ).mag();
      TofCorr -= calcTof((*tracks)[i], PathAB); // TOF CORRECTION
    }
    // calculate pathlength on helixA from vectorAB to vectorTof
    Float_t PathCorr = helixA.pathLength( (*Vectors3D)[NrDecays+1] );
    PathCorr -= helixA.pathLength( (*Vectors3D)[NrDecays] );
    BetaCorr = PathCorr / (TofCorr*clight);
    if ( !particle.empty() ) { // cut on new particle mass
      Float_t m2corr = calcM2(mom, BetaCorr);
      return cutOnMass2(m2corr, particle);
    }
    return kTRUE;
  }
  Error("StV0TofCorrection::correctBeta", "check Input! BetaCorr set to -998!");
  BetaCorr = -998.;
  return kFALSE;
}


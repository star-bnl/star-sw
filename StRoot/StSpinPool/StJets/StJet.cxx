//////////////////////////////////////////////////////////////////////
//
// $Id: StJet.cxx,v 1.3 2009/09/05 22:15:16 pibero Exp $
// $Log: StJet.cxx,v $
// Revision 1.3  2009/09/05 22:15:16  pibero
// Add tracks and towers references to jet
//
// Revision 1.2  2009/09/05 18:18:05  pibero
// Add utility functions for trigger patches
//
// Revision 1.1  2008/06/01 03:41:44  tai
// moved StJet, StJets, and TrackToJetIndex to StSpinPool/StJets
//
// Revision 1.4  2008/03/27 02:25:04  tai
// moved the definitions of the construcors from .h to .cxx
// set jetEt and 3 other variables in a constructor
//
// Revision 1.3  2007/06/07 01:36:06  kocolosk
// fix minor AutoBuild warnings
//
// Revision 1.2  2007/06/05 21:56:13  kocolosk
// added data members for zVertex and geometric trigger associations, plus methods for detEta (barrel only)
//
// Revision 1.1  2004/07/08 15:41:03  mmiller
// First release of StJetMaker.  Mike's commit of full clean of StJetFinder, StJetMaker, and StSpinMaker.  builds and runs in dev.
//
// Revision 1.5  2003/09/11 18:14:18  thenry
// *** empty log message ***
//
// Revision 1.4  2003/09/02 17:59:01  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.3  2002/12/04 20:28:07  thenry
// StppuDstMaker was modified to allow multiple jet analysis modules to be
// run simultaneosly with various parameters while the Maker loads the events
// and analyses them.  Four different jet analyzers exist:
//
// Konstanin's Analyzers:
//     Kt type: StppKonstKtJetAnalyzer
//     Cone type: StppKonstConeJetAnalyzer
//
// Mike's Analyzers:
//     Kt type: StppMikeKtJetAnalyzer
//     Cone type: StppMikeConeJetAnalyzer
//
// These modules all require the StJetFinder modules.
//
// Revision 1.2  2002/06/24 13:22:59  akio
// numerous bug fix & updates
//
// Revision 1.1  2002/02/11 20:30:48  akio
// Many updates, including very first version of jet finder.
//
//
// Revision 1.0  2001/06/14 Akio Ogawa
//
//////////////////////////////////////////////////////////////////////

#include <iostream>
#include <stdio.h>

#include "TrackToJetIndex.h"
#include "StJet.h"

ClassImp(StJet);

StJet::StJet()
  : TLorentzVector(0,0,0,0)
  , nCell(0)
  , charge(0)
  , nTracks(0)
  , nBtowers(0)
  , nEtowers(0)
  , tpcEtSum(0.0)
  , btowEtSum(0.0)
  , etowEtSum(0.0)
  , jetEt(0.0)
  , jetPt(0.0)
  , jetEta(0.0)
  , jetPhi(0.0)
  , zVertex(-999)
{
}

StJet::StJet(double lE, double lpx, double lpy, double lpz, Int_t size, int c)
  : TLorentzVector(lpx, lpy, lpz, lE)
  , nCell(size)
  , charge(c)
  , nTracks(0)
  , nBtowers(0)
  , nEtowers(0)
  , tpcEtSum(0.0)
  , btowEtSum(0.0)
  , etowEtSum(0.0)
  , zVertex(-999)
{
  jetEt = Et();
  jetPt = Pt();
  jetEta = Eta();
  jetPhi = Phi();
}

TrackToJetIndex* StJet::leadingChargedParticle() const
{
  TrackToJetIndex* lcp = 0;
  for (int iTrack = 0; iTrack < numberOfTracks(); ++iTrack) {
    TrackToJetIndex* t = track(iTrack);
    if (!lcp || t->Pt() > lcp->Pt()) lcp = t;
  }
  return lcp;
}

Float_t StJet::detEta() const {
    if (zVertex > -998) return detEta(zVertex);
    return -999;
}

Float_t StJet::detEta(float vz, float r) const {
    float hold(0.),denom(0.);
    if (Theta()==TMath::PiOver2()) { // if Jet Theta = 90 then tan is undefined
        if (vz==0) {hold = TMath::PiOver2();}
        else {hold = atan2(r,vz);}
    }
    else
    {
        denom = (r/tan(Theta()))+vz;
        if (denom==0.) {hold = TMath::PiOver2();}
        if (denom!=0.) {hold = atan2(r,denom);}
    }
    return -TMath::Log(TMath::Tan(hold/2));
}

void StJet::addGeomTrigger(int trigId) {
    //check if the trigId is already in the vector -- not really necessary
    for(unsigned int i=0; i<mGeomTriggers.size(); i++) {
        if (mGeomTriggers[i] == trigId) return;
    }
    mGeomTriggers.push_back(trigId);
}

bool StJet::geomTrigger(int trigId) const {
    for(unsigned int i=0; i<mGeomTriggers.size(); i++) {
        if (mGeomTriggers[i] == trigId) return true;
    }
    return false;
}

bool StJet::getJetPatchEtaPhi(int jetPatch, float& eta, float& phi)
{
  //
  // Pibero Djawotho <pibero@tamu.edu>
  // Texas A&M University
  // 5 September 2009
  //

  // Sanity check
  if (jetPatch < 0 || jetPatch >= 12) return false;

  //
  // The jet patches are numbered starting with JP0 centered at 150 degrees
  // looking from the West into the IR (intersection region) and increasing
  // clockwise, i.e. JP1 at 90 degrees, JP2 at 30 degrees, etc. On the East
  // side the numbering picks up at JP6 centered again at 150 degrees and
  // increasing clockwise (again as seen from the *West* into the IR). Thus
  // JP0 and JP6 are in the same phi location in the STAR coordinate system.
  // So are JP1 and JP7, etc.
  //
  // Jet Patch#  Eta  Phi  Quadrant
  //
  //          0  0.5  150       10'
  //          1  0.5   90       12'
  //          2  0.5   30        2'
  //          3  0.5  -30        4'
  //          4  0.5  -90        6'
  //          5  0.5 -150        8'
  //          6 -0.5  150       10'
  //          7 -0.5   90       12'
  //          8 -0.5   30        2'
  //          9 -0.5  -30        4'
  //         10 -0.5  -90        6'
  //         11 -0.5 -150        8'
  //
  // http://www.nikhef.nl/~ogrebeny/emc/files/Towers%20Layout.pdf
  // http://www.nikhef.nl/~ogrebeny/emc/files/BEMC.pdf
  // http://drupal.star.bnl.gov/STAR/system/files/BEMC_y2004.pdf
  //
  eta = (jetPatch < 6) ? 0.5 : -0.5;
  phi = 150 - (jetPatch % 6) * 60; // Degrees

  // Degrees to radians
  phi *= TMath::DegToRad();

  // Map phi into [-pi,pi]
  phi = TVector2::Phi_mpi_pi(phi);

  return true;
}

bool StJet::getJetPatchId(float eta, float phi, int& id)
{
  //
  // Pibero Djawotho <pibero@tamu.edu>
  // Texas A&M University
  // 5 September 2009
  //

  // Jet patch id is left at -1 on failure
  id = -1;

  // Check range of eta
  if (eta < -1 || eta > 1) return false;

  // Map phi into [-pi,pi] if necessary
  if (phi < -M_PI || phi > M_PI) phi = TVector2::Phi_mpi_pi(phi);

  // Get jet patch id
  static const double PI_OVER_3 = M_PI/3;

  if (0 <= eta && eta <= 1) {
    if ( 2*PI_OVER_3 <= phi && phi <         M_PI) id = 0;
    if (   PI_OVER_3 <= phi && phi <  2*PI_OVER_3) id = 1;
    if (           0 <= phi && phi <    PI_OVER_3) id = 2;
    if (  -PI_OVER_3 <= phi && phi <            0) id = 3;
    if (-2*PI_OVER_3 <= phi && phi <   -PI_OVER_3) id = 4;
    if (       -M_PI <= phi && phi < -2*PI_OVER_3) id = 5;
  }

  if (-1 <= eta && eta < 0) {
    if ( 2*PI_OVER_3 <= phi && phi <         M_PI) id = 6;
    if (   PI_OVER_3 <= phi && phi <  2*PI_OVER_3) id = 7;
    if (           0 <= phi && phi <    PI_OVER_3) id = 8;
    if (  -PI_OVER_3 <= phi && phi <            0) id = 9;
    if (-2*PI_OVER_3 <= phi && phi <   -PI_OVER_3) id = 10;
    if (       -M_PI <= phi && phi < -2*PI_OVER_3) id = 11;
  }

  return (0 <= id && id < 12);
}

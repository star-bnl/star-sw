//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 3 September 2009
//

#include "StJetTrack.h"
#include "StJetTower.h"
#include "StJetCandidate.h"

ClassImp(StJetCandidate);

StJetCandidate::StJetCandidate(const TVector3& vertex, float pt, float eta, float phi, float E)
  : mPt(pt)
  , mEta(eta)
  , mPhi(phi)
  , mE(E)
{
  // Front plate of BEMC towers or BPRS layer (See StEmcGeom/geometry/StEmcGeom.cxx)
  // This only works for BEMC jets.
  static const float BEMC_RADIUS = 225.405;

  TVector3 pos = momentum();
  pos.SetPerp(BEMC_RADIUS);
  pos += vertex;
  mDetEta = pos.Eta();
}

StJetTrack* StJetCandidate::leadingChargedParticle() const
{
  StJetTrack* lcp = 0;
  for (int i = 0; i < numberOfTracks(); ++i) {
    StJetTrack* t = track(i);
    if (!lcp || t->pt() > lcp->pt()) lcp = t;
  }
  return lcp;
}

float StJetCandidate::sumTrackPt() const
{
  float sumPt = 0;
  for (int i = 0; i < numberOfTracks(); ++i) sumPt += track(i)->pt();
  return sumPt;
}

float StJetCandidate::sumTowerPt() const
{
  float sumPt = 0;
  for (int i = 0; i < numberOfTowers(); ++i) sumPt += tower(i)->pt();
  return sumPt;
}

bool StJetCandidate::getJetPatchEtaPhi(int jetPatch, float& eta, float& phi)
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

bool StJetCandidate::getJetPatchId(float eta, float phi, int& id)
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

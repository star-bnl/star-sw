//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 3 September 2009
//

#include "StJetTrack.h"
#include "StJetTower.h"
#include "StJetCandidate.h"

ClassImp(StJetCandidate);

StJetCandidate::StJetCandidate(const TVector3& vertex, const TLorentzVector& fourMomentum, float area, float area_error)
  : mPt(fourMomentum.Pt())
  , mEta(fourMomentum.Eta())
  , mPhi(fourMomentum.Phi())
  , mE(fourMomentum.E())
  , mArea(area)
  , mAreaError(area_error)
{
  mDetEta = detEta(vertex);
}

float StJetCandidate::detEta(const TVector3& vertex) const
{
  float DetEta;
  if (getBarrelDetectorEta(vertex,DetEta)) return DetEta;
  if (getEndcapDetectorEta(vertex,DetEta)) return DetEta;
  return -999;
}

bool StJetCandidate::getBarrelDetectorEta(const TVector3& vertex, float& detEta) const
{
  // Front plate of BEMC or BPRS layer
  // See StEmcGeom/geometry/StEmcGeom.cxx
  static const double BEMC_RADIUS = 225.405; // cm
  // EEMC preshower1 layer
  // See StEEmcUtil/EEmcGeom/EEmcGeomDefs.h
  static const double EEMC_Z = 270.190; // cm
  TVector3 pos = momentum();
  pos.SetMag(BEMC_RADIUS/pos.Unit().Perp());
  pos += vertex;
  detEta = pos.Eta();
  //  return fabs(detEta) < 1;
  return fabs(pos.Z()) < EEMC_Z;
}

bool StJetCandidate::getEndcapDetectorEta(const TVector3& vertex, float& detEta) const
{
  // EEMC preshower1 layer
  // See StEEmcUtil/EEmcGeom/EEmcGeomDefs.h
  static const double EEMC_Z = 270.190; // cm
  TVector3 pos = momentum();
  if (pos.z() > 0) {
    // Real endcap
    pos.SetMag((EEMC_Z-vertex.z())/pos.Unit().z());
    pos += vertex;
    detEta = pos.Eta();
  }
  else {
    // Mirror endcap (flip about xy-plane, solve and flip sign)
    TVector3 pos2(pos.x(),pos.y(),-pos.z());
    TVector3 vertex2(vertex.x(),vertex.y(),-vertex.z());
    pos2.SetMag((EEMC_Z-vertex2.z())/pos2.Unit().z());
    pos2 += vertex2;
    detEta = -pos2.Eta();
  }
  //  return fabs(detEta) > 1 && fabs(detEta) < 2;
  return fabs(detEta) < 2.5;
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

float StJetCandidate::deltaR(const StJetElement* element) const
{
  return momentum().DeltaR(element->momentum());
}

float StJetCandidate::sumTrackPt() const
{
  float s = 0;
  for (int i = 0; i < numberOfTracks(); ++i) s += track(i)->pt();
  return s;
}

float StJetCandidate::sumTrackPt(float radius) const
{
  float s = 0;
  for (int i = 0; i < numberOfTracks(); ++i) {
    const StJetTrack* t = track(i);
    if (deltaR(t) < radius) s += t->pt();
  }
  return s;
}

float StJetCandidate::sumTowerPt() const
{
  float s = 0;
  for (int i = 0; i < numberOfTowers(); ++i) s += tower(i)->pt();
  return s;
}

float StJetCandidate::sumTowerPt(float radius) const
{
  float s = 0;
  for (int i = 0; i < numberOfTowers(); ++i) {
    const StJetTower* t = tower(i);
    if (deltaR(t) < radius) s += t->pt();
  }
  return s;
}

StJetTrack* StJetCandidate::getTrackById(int id) const
{
  for (int i = 0; i < numberOfTracks(); ++i) {
    StJetTrack* t = track(i);
    if (t->id() == id) return t;
  }
  return 0;
}

StJetTower* StJetCandidate::getTowerById(int id) const
{
  for (int i = 0; i < numberOfTowers(); ++i) {
    StJetTower* t = tower(i);
    if (t->id() == id) return t;
  }
  return 0;
}

float StJetCandidate::getJetPatchPhi(int jetPatch)
{
  return TVector2::Phi_mpi_pi((150 - (jetPatch % 6) * 60) * TMath::DegToRad());
}

bool StJetCandidate::getBarrelJetPatchEtaPhi(int jetPatch, float& eta, float& phi)
{
  //
  // Pibero Djawotho <pibero@tamu.edu>
  // Texas A&M University
  // 5 September 2009
  // Revised 9 Feruary 2010 to include middle jet patches
  //

  // Sanity check
  if (jetPatch < 0 || jetPatch >= 18) return false;

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

  if (jetPatch >=  0 && jetPatch <  6) eta =  0.5;
  if (jetPatch >=  6 && jetPatch < 12) eta = -0.5;
  if (jetPatch >= 12 && jetPatch < 18) eta = -0.1;

  phi = getJetPatchPhi(jetPatch);

  return true;
}

bool StJetCandidate::getEndcapJetPatchEtaPhi(int jetPatch, float& eta, float& phi)
{
  if (jetPatch >= 0 && jetPatch < 6)
    eta = 1.5;
  else
    return false;

  phi = getJetPatchPhi(jetPatch);

  return true;
}

bool StJetCandidate::getOverlapJetPatchEtaPhi(int jetPatch, float& eta, float& phi)
{
  if (jetPatch >= 0 && jetPatch < 6)
    eta = 0.9;
  else
    return false;

  phi = getJetPatchPhi(jetPatch);

  return true;
}

bool StJetCandidate::getBarrelJetPatchId(float eta, float phi, int& id)
{
  //
  // Pibero Djawotho <pibero@tamu.edu>
  // Texas A&M University
  // 5 September 2009
  // To do: Need to include possibility of returning middle jet patch
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

bool StJetCandidate::getEndcapJetPatchId(float eta, float phi, int& id)
{
  // Jet patch id is left at -1 on failure
  id = -1;

  // Check range of eta
  if (eta < 1 || eta > 2) return false;

  // Map phi into [-pi,pi] if necessary
  if (phi < -M_PI || phi > M_PI) phi = TVector2::Phi_mpi_pi(phi);

  // Get jet patch id
  static const double PI_OVER_3 = M_PI/3;

  if (1 <= eta && eta <= 2) {
    if ( 2*PI_OVER_3 <= phi && phi <         M_PI) id = 0;
    if (   PI_OVER_3 <= phi && phi <  2*PI_OVER_3) id = 1;
    if (           0 <= phi && phi <    PI_OVER_3) id = 2;
    if (  -PI_OVER_3 <= phi && phi <            0) id = 3;
    if (-2*PI_OVER_3 <= phi && phi <   -PI_OVER_3) id = 4;
    if (       -M_PI <= phi && phi < -2*PI_OVER_3) id = 5;
  }

  return (0 <= id && id < 6);
}

bool StJetCandidate::getOverlapJetPatchId(float eta, float phi, int& id)
{
  // Jet patch id is left at -1 on failure
  id = -1;

  // Check range of eta
  if (eta < 0.4 || eta > 1.4) return false;

  // Map phi into [-pi,pi] if necessary
  if (phi < -M_PI || phi > M_PI) phi = TVector2::Phi_mpi_pi(phi);

  // Get jet patch id
  static const double PI_OVER_3 = M_PI/3;

  if (0.4 <= eta && eta <= 1.4) {
    if ( 2*PI_OVER_3 <= phi && phi <         M_PI) id = 0;
    if (   PI_OVER_3 <= phi && phi <  2*PI_OVER_3) id = 1;
    if (           0 <= phi && phi <    PI_OVER_3) id = 2;
    if (  -PI_OVER_3 <= phi && phi <            0) id = 3;
    if (-2*PI_OVER_3 <= phi && phi <   -PI_OVER_3) id = 4;
    if (       -M_PI <= phi && phi < -2*PI_OVER_3) id = 5;
  }

  return (0 <= id && id < 6);
}

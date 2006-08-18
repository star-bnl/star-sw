//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// November 17, 2005
//

// C++ STL
#include <set>

// ROOT
#include "TH1.h"
#include "TH2.h"

// STAR
#include "StEventTypes.h"

// Local
#include "Track.hh"
#include "TopologyMap.hh"
#include "StBeamBackMaker.h"

#define MAX_R_DISTANCE 5.	// cm
#define MIN_TRACK_SEED_HITS 100

struct LessHit {
  bool operator()(const StHit* hit, const StHit* hit2) const
  {
    return hit->position().z() < hit2->position().z();
  }
};

typedef multiset<StHit*, LessHit> HitSet;
typedef HitSet::iterator HitSetIter;

struct LessTrack {
  bool operator()(const Track* track, const Track* track2) const
  {
    return track->numberOfHits() < track2->numberOfHits();
  }
};

typedef multiset<Track*, LessTrack> TrackSet;
typedef TrackSet::iterator TrackSetIter;

ClassImp(StBeamBackMaker)

Int_t StBeamBackMaker::Init()
{
  fEventTime = new TH1F("fEventTime", ";Time [seconds]", 100, 0, 20);
  fHitMapBuildTime = new TH1F("fHitMapBuildTime", ";Time [seconds]", 100, 0, 20);
  fTrackSeedBuildTime = new TH1F("fTrackSeedBuildTime", ";Time [seconds]", 100, 0, 20);
  fTrackSeedFilterTime = new TH1F("fTrackSeedFilterTime", ";Time [seconds]", 100, 0, 20);
  fTrackExtendTime = new TH1F("fTrackExtendTime", ";Time [seconds]", 100, 0, 20);
  fTrackMergeTime = new TH1F("fTrackMergeTime", ";Time [seconds]", 100, 0, 20);
  fTrackRefitTime = new TH1F("fTrackRefitTime", ";Time [seconds]", 100, 0, 20);
  fTrackConvertTime = new TH1F("fTrackConvertTime", ";Time [seconds]", 100, 0, 20);
  fViewXY = new TH2F("viewXY", ";x [cm];y [cm]",
		     200, -200, 200, 200, -200, 200);
  fViewZX = new TH2F("viewZX", ";z [cm];x [cm]",
		     200, -200, 200, 200, -200, 200);
  fViewZY = new TH2F("viewZY", ";z [cm];y [cm]",
		     200, -200, 200, 200, -200, 200);
  return StMaker::Init();
}

Int_t StBeamBackMaker::Make()
{
  TStopwatch timer;
  Int_t status = makeHelper();
  fEventTime->Fill(timer.RealTime());
  return status;
}

Int_t StBeamBackMaker::makeHelper()
{
  StEvent* event = (StEvent*)GetInputDS("StEvent");
  if (!event) {
    { LOG_WARN << GetName() << "::Make() - No StEvent" << endm; }
    return kStWarn;
  }

  StTpcHitCollection* tpc = event->tpcHitCollection();
  if (!tpc) {
    return kStOk;
  }

  { LOG_INFO << GetName() << "::Make() - Total TPC hits:\t" << tpc->numberOfHits() << endm; }

  //
  // Collect all unused TPC hits, i.e. those that were not assigned to
  // any track by ITTF, into a set with the hit
  // with the least z-coordinate at the beginning and the hit with
  // the highest z-coordinate at the end.
  //
  TStopwatch timer;

  HitSet hits;
  for (UInt_t sector = 0; sector < tpc->numberOfSectors(); ++sector) {
    for (UInt_t padrow = 0; padrow < tpc->sector(sector)->numberOfPadrows(); ++padrow) {
      for (UInt_t i = 0; i < tpc->sector(sector)->padrow(padrow)->hits().size(); ++i) {
	StHit* hit = tpc->sector(sector)->padrow(padrow)->hits()[i];
	if (!hit->trackReferenceCount()) {
	  hits.insert(hit);
#if 0
	  fViewXY->Fill(hit->position().x(), hit->position().y());
	  fViewZX->Fill(hit->position().z(), hit->position().x());
	  fViewZY->Fill(hit->position().z(), hit->position().y());
#endif
	}
      }
    }
  }

  fHitMapBuildTime->Fill(timer.RealTime());

  { LOG_INFO << GetName() << "::Make() - Unused TPC hits:\t" << hits.size() << endm; }

  //
  // Find track seeds
  //
  timer.Start(kTRUE);

  TrackSet tracks;
  while (!hits.empty()) {
    Track* track = new Track;
    track->addHit(*hits.begin());
    hits.erase(hits.begin());
    // Compute initial centroid
    if (!track->numberOfHits()) continue;
    double sumX = 0;
    double sumY = 0;
    for (Track::iterator i = track->begin(); i != track->end(); ++i) {
      StHit* hit = *i;
      sumX += hit->position().x();
      sumY += hit->position().y();
    }
    double meanX = sumX / track->numberOfHits();
    double meanY = sumY / track->numberOfHits();
    // Add hits within MAX_R_DISTANCE of centroid to track
    for (HitSetIter i = hits.begin(); i != hits.end(); ++i) {
      StHit* hit = *i;
      double dx = meanX - hit->position().x();
      double dy = meanY - hit->position().y();
      double dr = hypot(dx, dy);
      if (dr < MAX_R_DISTANCE) {
	track->addHit(hit);
	hits.erase(i);
	// Update centroid
	sumX += hit->position().x();
	sumY += hit->position().y();
	meanX = sumX / track->numberOfHits();
	meanY = sumY / track->numberOfHits();
      }
    }
    tracks.insert(track);
  }

  fTrackSeedBuildTime->Fill(timer.RealTime());

  //
  // Pick only track seeds with at least MIN_TRACK_SEED_HITS hits.
  // The others are put back in the set of available hits.
  //
  timer.Start(kTRUE);

  for (TrackSetIter i = tracks.begin(); i != tracks.end(); ++i) {
    Track* track = *i;
    if (track->numberOfHits() < MIN_TRACK_SEED_HITS) {
      for (Track::iterator j = track->begin(); j != track->end(); ++j) {
	StHit* hit = *j;
	hits.insert(hit);
      }
      tracks.erase(i);
    }
  }

  fTrackSeedFilterTime->Fill(timer.RealTime());

  //
  // Try to fit track seeds to straight tracks by doing
  // parallel linear regression analyses in xz and yz.
  //
  timer.Start(kTRUE);

  vector<Track*> linearTracks;
  for (TrackSetIter i = tracks.begin(); i != tracks.end(); ++i) {
    Track* track = *i;
    if (track->fit() && track->ok()) {
      //
      // Try to extend the straight track by looking for hits in the
      // pool of available hits that are within 5 cm of the centroid
      // of the track in the xy-plane.
      //
      for (HitSetIter j = hits.begin(); j != hits.end(); ++j) {
	StHit* hit = *j;
	if (track->accept(hit)) {
	  track->addHit(hit);
	  hits.erase(j);
	}
      }
      linearTracks.push_back(track);
    }
  }

  fTrackExtendTime->Fill(timer.RealTime());

  //
  // Merge linear tracks if both end points of the first track
  // are within 5 cm of the centroid of the track in the xy-plane.
  //
  timer.Start(kTRUE);
  for (unsigned int i = 0; i < linearTracks.size(); ++i) {
    if (!linearTracks[i]) continue;
    for (unsigned int j = i + 1; j < linearTracks.size(); ++j) {
      if (!linearTracks[j]) continue;
      if (linearTracks[i]->accept(linearTracks[j]->firstHit()) &&
	  linearTracks[i]->accept(linearTracks[j]->lastHit())) {
	linearTracks[i]->merge(linearTracks[j]);
	delete linearTracks[j];
	linearTracks[j] = 0;
      }
    }
  }

  //
  // Compress vector of linear tracks (remove null entries)
  //
  linearTracks.erase(remove(linearTracks.begin(), linearTracks.end(), (Track*)0), linearTracks.end());
  fTrackMergeTime->Fill(timer.RealTime());

  //
  // Refit and remove outliers.
  //
  timer.Start(kTRUE);
  for (unsigned int i = 0; i < linearTracks.size(); ++i) {
    Track* track = linearTracks[i];
    if (track->fit()) {
      for (Track::iterator j = track->begin(); j != track->end();) {
	StHit* hit = *j;
	if (track->accept(hit)) {
	  ++j;
	}
	else {
	  Track::iterator k = j;
	  ++k;
	  track->removeHit(j);
	  j = k;
	  hits.insert(hit);
	}
      }
    }
  }
  fTrackRefitTime->Fill(timer.RealTime());

  //
  // Number of hits in linear tracks
  //
  int nHits = 0;
  for (unsigned int i = 0; i < linearTracks.size(); ++i)
    nHits += linearTracks[i]->numberOfHits();
  { LOG_INFO << GetName() << "::Make() - TPC hits in linear tracks:\t" << nHits << endm; }

  //
  // Track to StTrack conversion.
  //
  // Find the highest track key. Increment successively to assign
  // to new tracks.
  //
  timer.Start(kTRUE);
  unsigned short key = 0;
  for (unsigned int i = 0; i < event->trackNodes().size(); ++i) {
    unsigned short key2 = event->trackNodes()[i]->track(global)->key();
    if (key < key2) key = key2;
  }

  for (unsigned int i = 0; i < linearTracks.size(); ++i) {
    if (pileup(linearTracks[i])) continue;
    StTrack* track = createStTrack(linearTracks[i]);
    StTrackNode* trackNode = new StTrackNode;
    track->setKey(++key);
    trackNode->addTrack(track);
    event->trackNodes().push_back(trackNode);
    event->trackDetectorInfo().push_back(track->detectorInfo());
  }
  fTrackConvertTime->Fill(timer.RealTime());

  //
  // Clean up
  //
  for (unsigned int i = 0; i < linearTracks.size(); ++i)
    delete linearTracks[i];

  return kStOk;
}

StTrack* StBeamBackMaker::createStTrack(Track* track)
{
  StTrack* gTrack = new StGlobalTrack;
  gTrack->setLength(track->length());
  gTrack->setFlag(901);
  gTrack->setEncodedMethod(kLine3DId);
  // Inner geometry
  StThreeVectorF origin(track->x0(), track->y0(), 0);
  StThreeVectorF momentum(track->dxdz(), track->dydz(), 1);
  momentum.setMagnitude(999);	// 999 GeV/c, arbitrary
  gTrack->setGeometry(new StHelixModel(-1,        // Charge
				       M_PI_2,    // Psi
				       0,         // Curvature
				       M_PI_2,    // Dip angle
				       origin,    // Origin
				       momentum,  // Momentum
				       1));       // Helicity
  // Outer geometry
  gTrack->setOuterGeometry(gTrack->geometry()->copy());
  // Hack -- Store direction in origin of outer geometry
  gTrack->outerGeometry()->setOrigin(gTrack->outerGeometry()->momentum());
  // Detector info
  StTrackDetectorInfo* detInfo = new StTrackDetectorInfo;
  detInfo->setFirstPoint(track->lastHit()->position());
  detInfo->setLastPoint(track->firstHit()->position());
  for (Track::reverse_iterator i = track->rbegin(); i != track->rend(); ++i)
    detInfo->addHit(*i);
  detInfo->setNumberOfPoints(track->numberOfHits(), kTpcId);
  gTrack->setDetectorInfo(detInfo);
  // Fit traits
  StTrackFitTraits fitTraits;
  fitTraits.setNumberOfFitPoints(track->numberOfHits(), kTpcId);
  gTrack->setFitTraits(fitTraits);

  return gTrack;
}

bool StBeamBackMaker::pileup(Track* track) const
{
  TopologyMap topoMap(track);
  return (topoMap.nearEast() < 4 || topoMap.farEast() < 4 ||
	  topoMap.nearWest() < 4 || topoMap.farWest() < 4);
}

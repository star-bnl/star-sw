//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// November 17, 2005
//

// C++ STL
#include <vector>
#include <set>

// STAR
#include "StEventTypes.h"

// Local
#include "Line.hh"
#include "Track.hh"
#include "TopologyMap.hh"
#include "StBeamBackMaker.h"

#define MAX_R_DISTANCE 5.	// cm
#define MAX_Z_DISTANCE 10.	// cm
#define MIN_TRACK_SEED_HITS 60
using namespace std;
struct LessHit {
  bool operator()(const StHit* hit1, const StHit* hit2) const
  {
    return hit1->position().z() < hit2->position().z();
  }
};

typedef multiset<StHit*, LessHit> HitSet;
typedef HitSet::iterator HitSetIter;

struct LessTrack {
  bool operator()(const Track* track1, const Track* track2) const
  {
    return track1->size() < track2->size();
  }
};

typedef multiset<Track*, LessTrack> TrackSet;
typedef TrackSet::iterator TrackSetIter;

ClassImp(StBeamBackMaker)

Int_t StBeamBackMaker::Make()
{
  info() << "Processing run=" << GetRunNumber()
	 << ", event=" << GetEventNumber() << endm;

  StEvent* event = (StEvent*)GetInputDS("StEvent");
  if (!event) {
    warning("No StEvent");
    return kStWarn;
  }

  StTpcHitCollection* tpc = event->tpcHitCollection();
  if (!tpc) {
    info("No TPC hits");
    return kStOk;
  }

  info() << tpc->numberOfHits() << " TPC hits in event" << endm;

  //
  // Collect all unused TPC hits, i.e. those that were not assigned to
  // any track by ITTF, into a set with the hit
  // with the least z-coordinate at the beginning and the hit with
  // the highest z-coordinate at the end.
  //
  HitSet hits;
  for (UInt_t sector = 0; sector < tpc->numberOfSectors(); ++sector) {
    for (UInt_t padrow = 0; padrow < tpc->sector(sector)->numberOfPadrows(); ++padrow) {
      for (UInt_t i = 0; i < tpc->sector(sector)->padrow(padrow)->hits().size(); ++i) {
	StHit* hit = tpc->sector(sector)->padrow(padrow)->hits()[i];
	if (!hit->trackReferenceCount()) {
	  hits.insert(hit);
	}
      }
    }
  }
  info() << hits.size() << " unused TPC hits in event" << endm;

  //
  // Find track seeds
  //
  info("Find track seeds");
  // Allocate storage, but don't initialize
  Track* bufBeg = (Track*)malloc(sizeof(Track)*hits.size());
  Track* bufEnd = bufBeg;
  TrackSet tracks;
  while (!hits.empty()) {
    Track* track = bufEnd++;
    new (track) Track;
    StHit* hit = *hits.begin();
    track->push_back(hit);
    hits.erase(hits.begin());
    // Compute initial centroid
    double sumX = hit->position().x();
    double sumY = hit->position().y();
    double meanX = sumX;
    double meanY = sumY;
    // Add hits within MAX_R_DISTANCE of centroid to track
    for (HitSetIter i = hits.begin(); i != hits.end();) {
      StHit* hit = *i;
      double dz = hit->position().z() - track->lastHit()->position().z();
      if (fabs(dz) > MAX_Z_DISTANCE) break;
      double dx = meanX - hit->position().x();
      double dy = meanY - hit->position().y();
      double dr = hypot(dx, dy);
      if (dr < MAX_R_DISTANCE) {
	track->push_back(hit);
	HitSetIter next = i;
	++next;
	hits.erase(i);
	i = next;
	// Update centroid
	sumX += hit->position().x();
	sumY += hit->position().y();
	meanX = sumX / track->size();
	meanY = sumY / track->size();
      }
      else {
	++i;
      }
    }
    tracks.insert(track);
  }
  info() << tracks.size() << " track seeds found" << endm;

  //
  // Pick only track seeds with at least MIN_TRACK_SEED_HITS hits.
  // The others are put back in the set of available hits.
  //
  info() << "Removing track seeds with less than "
	 << MIN_TRACK_SEED_HITS << " hits" << endm;
  for (TrackSetIter i = tracks.begin(); i != tracks.end();) {
    Track* track = *i;
    if (track->size() < MIN_TRACK_SEED_HITS) {
      for (Track::iterator j = track->begin(); j != track->end(); ++j) {
	StHit* hit = *j;
	hits.insert(hit);
      }
      TrackSetIter next = i;
      ++next;
      tracks.erase(i);
      i = next;
    }
    else {
      ++i;
    }
  }
  info() << tracks.size() << " track seeds left with " << MIN_TRACK_SEED_HITS << " hits or more" << endm;

  //
  // Try to fit track seeds to straight tracks by doing
  // parallel linear regression analyses in xz and yz.
  //
  info("Find linear tracks");
  vector<Track*> linearTracks;
  for (TrackSetIter i = tracks.begin(); i != tracks.end(); ++i) {
    Track* track = *i;
    if (track->fit() && track->ok()) {
      //
      // Try to extend the straight track by looking for hits in the
      // pool of available hits that are within 5 cm of the centroid
      // of the track in the xy-plane.
      //
      for (HitSetIter j = hits.begin(); j != hits.end();) {
	StHit* hit = *j;
	if (track->accept(hit)) {
	  track->push_back(hit);
	  // Move added hit to its proper place
	  nth_element(track->begin(), track->rbegin().base(), track->end(), LessHit());
	  track->fit();
	  HitSetIter next = j;
	  ++next;
	  hits.erase(j);
	  j = next;
	}
	else {
	  ++j;
	}
      }
      linearTracks.push_back(track);
    }
  }
  info() << linearTracks.size() << " linear tracks found" << endm;

  //
  // Merge linear tracks if both end points of the first track
  // are within 5 cm of the centroid of the track in the xy-plane.
  //
  info("Start merging tracks");
  for (unsigned int i = 0; i < linearTracks.size(); ++i) {
    if (!linearTracks[i]) continue;
    for (unsigned int j = i + 1; j < linearTracks.size(); ++j) {
      if (!linearTracks[j]) continue;
      if (linearTracks[i]->accept(linearTracks[j]->firstHit()) &&
	  linearTracks[i]->accept(linearTracks[j]->lastHit())) {
	linearTracks[i]->merge(linearTracks[j]);
	linearTracks[j] = 0;
      }
    }
  }

  //
  // Compress vector of linear tracks (remove null entries)
  //
  linearTracks.erase(remove(linearTracks.begin(), linearTracks.end(),
			    (Track*)0), linearTracks.end());
  info() << linearTracks.size() << " merged tracks" << endm;

  //
  // Refit and remove outliers.
  //
  info("Refit and remove outliers");
  for (unsigned int i = 0; i < linearTracks.size(); ++i) {
    Track* track = linearTracks[i];
    if (track->fit()) {
      for (Track::iterator j = track->begin(); j != track->end();) {
	StHit* hit = *j;
	if (track->accept(hit)) {
	  ++j;
	}
	else {
	  j = track->erase(j);
	  hits.insert(hit);
	}
      }
    }
  }
  info() << hits.size() << " unused TPC hits" << endm;

  //
  // Number of hits in linear tracks
  //
  int nHits = 0;
  for (unsigned int i = 0; i < linearTracks.size(); ++i)
    nHits += linearTracks[i]->size();
  info() << nHits << " TPC hits in linear tracks" << endm;

  //
  // Track to StTrack conversion.
  //
  // Find the highest track key. Increment successively to assign
  // to new tracks.
  //
  info("Converting Track to StTrack");
  Int_t key = 0;
  for (unsigned int i = 0; i < event->trackNodes().size(); ++i) {
    Int_t key2 = event->trackNodes()[i]->track(global)->key();
    if (key < key2) key = key2;
  }

  Int_t nStTrack = 0;
  for (unsigned int i = 0; i < linearTracks.size(); ++i) {
    if (pileup(linearTracks[i])) continue;
    StTrack* track = createStTrack(linearTracks[i]);
    StTrackNode* trackNode = new StTrackNode;
    track->setKey(++key);
    trackNode->addTrack(track);
    event->trackNodes().push_back(trackNode);
    event->trackDetectorInfo().push_back(track->detectorInfo());
    ++nStTrack;
  }
  info() << nStTrack << " StTrack saved" << endm;

  //
  // Clean up
  //
  for (Track* track = bufBeg; track != bufEnd; ++track) track->~Track();
  free(bufBeg);

  return kStOk;
}

StTrack* StBeamBackMaker::createStTrack(Track* track)
{
  StTrack* gTrack = new StGlobalTrack;
  gTrack->setLength(track->length());
  gTrack->setFlag(901);
  // Inner geometry
  StThreeVectorF origin(track->x0(), track->y0(), 0);
  StThreeVectorF momentum(track->dxdz(), track->dydz(), 1);
  Line line(origin, momentum);
  momentum.setMag(999);		// Bogus
  double dipAngle = atan2(1, hypot(track->dxdz(), track->dydz()));
  gTrack->setGeometry(new StHelixModel(-1, // Charge
				       M_PI_2, // Psi
				       0, // Curvature
				       dipAngle, // Dip angle
				       line.perigee(track->firstHit()->position()), // Origin
				       momentum, // Momentum
				       1)); // Helicity
  gTrack->setEncodedMethod(kLine2StepId);
  // Outer geometry
  StTrackGeometry* outerGeometry = gTrack->geometry()->copy();
  outerGeometry->setOrigin(line.perigee(track->lastHit()->position()));
  gTrack->setOuterGeometry(outerGeometry);
  
  // Detector info
  StTrackDetectorInfo* detInfo = new StTrackDetectorInfo;
  detInfo->setFirstPoint(track->firstHit()->position());
  detInfo->setLastPoint(track->lastHit()->position());
  for (Track::iterator i = track->begin(); i != track->end(); ++i)
    detInfo->addHit(*i);
  // Number of points cannot be larger than 255 (unsigned char)
  detInfo->setNumberOfPoints(track->size() < 256 ? track->size() : 255, kTpcId);
  gTrack->setDetectorInfo(detInfo);
  // Fit traits
  StTrackFitTraits fitTraits;
  // Number of fit points cannot be larger than 255 (unsigned char)
  fitTraits.setNumberOfFitPoints(track->size() < 256 ? track->size() : 255, kTpcId);
  gTrack->setFitTraits(fitTraits);

  return gTrack;
}

inline bool StBeamBackMaker::pileup(Track* track) const
{
  TopologyMap topoMap(track);
  return (topoMap.nearEast() < 4 || topoMap.farEast() < 4 ||
	  topoMap.nearWest() < 4 || topoMap.farWest() < 4);
}

inline ostream& StBeamBackMaker::info(const Char_t* message)
{
  if (message)
    return gMessMgr->Info(Form("%s: %s", GetName(), message));
  return gMessMgr->Info() << GetName() << ": ";
}

inline ostream& StBeamBackMaker::warning(const Char_t* message)
{
  if (message)
    return gMessMgr->Warning(Form("%s: %s", GetName(), message));
  return gMessMgr->Warning() << GetName() << ": ";
}

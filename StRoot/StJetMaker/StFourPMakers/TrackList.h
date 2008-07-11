// -*- mode: c++;-*-
// $Id: TrackList.h,v 1.1 2008/07/11 23:35:27 tai Exp $
#ifndef TRACKLIST_H
#define TRACKLIST_H

#include <vector>

namespace StSpinJet {

struct Track {
  int            runNumber;
  int            eventId;
  double         px;
  double         py;
  double         pz;
  short          flag;
  unsigned short nHits;
  short          charge;
  unsigned short nHitsPoss;
  unsigned short nHitsDedx;
  unsigned short nHitsFit;
  double         nSigmaPion;
  double         Tdca;
  float          dcaZ;
  flot           dcaD;
  double         BField;
  double         bemcRadius;
  double         etaext;
  double         phiext;
  double         dEdx;
  int            trackIndex;
  short          id;
};

typedef std::vector<Track> TrackList;

}

#endif // TRACKLIST_H

// -*- mode: c++;-*-
// $Id: CollectChargedTracksFromTPC.h,v 1.1 2008/06/01 22:46:44 tai Exp $
#ifndef COLLECTCHARGEDTRACKSFROMTPC_H
#define COLLECTCHARGEDTRACKSFROMTPC_H


#include <vector>
#include <utility>

class StMuTrack;
class StMuDstMaker;

namespace StSpinJet {

class CollectChargedTracksFromTPC {

public:
  CollectChargedTracksFromTPC(StMuDstMaker* uDstMaker);
  virtual ~CollectChargedTracksFromTPC();

  typedef std::vector<std::pair<const StMuTrack*, int> > TrackList;

  TrackList Do();

  void setUse2006Cuts(bool v) { _use2006Cuts = v; }

private:

  TrackList getTracksFromTPC();

  TrackList selectTracksToPassToJetFinder(const TrackList& trackList);

  bool shoudNotPassToJetFinder(const StMuTrack& track) const;

  StMuDstMaker* _uDstMaker;

  bool _use2006Cuts;

};

}

#endif // COLLECTCHARGEDTRACKSFROMTPC_H

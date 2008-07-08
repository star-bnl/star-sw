// -*- mode: c++;-*-
// $Id: CollectChargedTracksFromTPC.h,v 1.5 2008/07/08 02:40:30 tai Exp $
#ifndef COLLECTCHARGEDTRACKSFROMTPC_H
#define COLLECTCHARGEDTRACKSFROMTPC_H


#include <vector>
#include <utility>

class StMuDstMaker;

namespace StSpinJet {

class StMuTrackEmu;

class CollectChargedTracksFromTPC {

public:

  CollectChargedTracksFromTPC(StMuDstMaker* uDstMaker);
  virtual ~CollectChargedTracksFromTPC();

  typedef std::vector<StSpinJet::StMuTrackEmu*> TrackList;

  TrackList Do();

  void setUse2006Cuts(bool v) { _use2006Cuts = v; }

private:

  TrackList getTracksFromTPC();

  TrackList selectTracksToPassToJetFinder(const TrackList& trackList);
  bool shoudNotPassToJetFinder(const StMuTrackEmu& track) const;

  StMuDstMaker* _uDstMaker;

  bool _use2006Cuts;

};

}

#endif // COLLECTCHARGEDTRACKSFROMTPC_H

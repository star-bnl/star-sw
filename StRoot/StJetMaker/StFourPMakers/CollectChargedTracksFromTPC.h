// -*- mode: c++;-*-
// $Id: CollectChargedTracksFromTPC.h,v 1.7 2008/07/08 04:16:04 tai Exp $
#ifndef COLLECTCHARGEDTRACKSFROMTPC_H
#define COLLECTCHARGEDTRACKSFROMTPC_H

#include <vector>
#include <utility>

class StMuDstMaker;

namespace StSpinJet {

class StMuTrackEmu;
class StJetTPC;

class CollectChargedTracksFromTPC {

public:

  CollectChargedTracksFromTPC(StJetTPC* tpc);
  virtual ~CollectChargedTracksFromTPC();

  typedef std::vector<StSpinJet::StMuTrackEmu*> TrackList;

  TrackList Do();

  void setUse2006Cuts(bool v) { _use2006Cuts = v; }

private:

  TrackList selectTracksToPassToJetFinder(const TrackList& trackList);
  bool shoudNotPassToJetFinder(const StMuTrackEmu& track) const;

  StJetTPC* _tpc;

  bool _use2006Cuts;

};

}

#endif // COLLECTCHARGEDTRACKSFROMTPC_H

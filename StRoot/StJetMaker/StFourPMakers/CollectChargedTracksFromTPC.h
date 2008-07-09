// -*- mode: c++;-*-
// $Id: CollectChargedTracksFromTPC.h,v 1.9 2008/07/09 23:53:28 tai Exp $
#ifndef COLLECTCHARGEDTRACKSFROMTPC_H
#define COLLECTCHARGEDTRACKSFROMTPC_H

#include <vector>
#include <utility>

namespace StSpinJet {

class StMuTrackEmu;
class StJetTPC;
class StJetTPCTrackCut;

class CollectChargedTracksFromTPC {

public:

  CollectChargedTracksFromTPC(StJetTPC* tpc, StJetTPCTrackCut* cut);
  virtual ~CollectChargedTracksFromTPC();

  typedef std::vector<StMuTrackEmu*> TrackList;

  TrackList Do();

  void setUse2006Cuts(bool v);

private:

  StJetTPC* _tpc;

  StJetTPCTrackCut& _cut;

};

}

#endif // COLLECTCHARGEDTRACKSFROMTPC_H

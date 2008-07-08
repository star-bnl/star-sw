// -*- mode: c++;-*-
// $Id: CollectChargedTracksFromTPC.h,v 1.8 2008/07/08 04:56:12 tai Exp $
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

  CollectChargedTracksFromTPC(StJetTPC* tpc);
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

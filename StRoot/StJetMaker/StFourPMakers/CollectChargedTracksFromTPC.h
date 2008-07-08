// -*- mode: c++;-*-
// $Id: CollectChargedTracksFromTPC.h,v 1.6 2008/07/08 03:14:41 tai Exp $
#ifndef COLLECTCHARGEDTRACKSFROMTPC_H
#define COLLECTCHARGEDTRACKSFROMTPC_H


#include <vector>
#include <utility>

class StMuDstMaker;

namespace StSpinJet {


class StMuTrackEmu;

class StJetTPC {

public:
  StJetTPC() { }
  virtual ~StJetTPC() { }

  typedef std::vector<StSpinJet::StMuTrackEmu*> TrackList;

  virtual TrackList getTrackList() = 0;
};

class StJetTPCMuDst : public StJetTPC {

public:
  StJetTPCMuDst(StMuDstMaker* uDstMaker);
  virtual ~StJetTPCMuDst() { }

  TrackList getTrackList();

private:
  StMuDstMaker* _uDstMaker;

};

class CollectChargedTracksFromTPC {

public:

  CollectChargedTracksFromTPC(StMuDstMaker* uDstMaker);
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

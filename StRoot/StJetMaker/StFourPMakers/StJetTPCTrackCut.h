// -*- mode: c++;-*-
// $Id: StJetTPCTrackCut.h,v 1.4 2008/07/13 08:23:29 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTPCTRACKCUT_H
#define STJETTPCTRACKCUT_H

#include "TrackList.h"

#include <vector>

namespace StJetTrackCut {

class TrackCut {

public:
  TrackCut() { }
  virtual ~TrackCut() { }

  virtual bool operator()(const StSpinJet::Track& track) = 0;

};

class TrackCutDca : public TrackCut {
  // to reduce pile up tracks

public:
  TrackCutDca(double max = 3.0, double min = 0.0) : _max(max), _min(min) { }
  virtual ~TrackCutDca() { }

  bool operator()(const StSpinJet::Track& track)
  {
    if(track.Tdca > _max) return true;

    if(track.Tdca < _min) return true;

    return false;
  }

private:

  double  _max;
  double  _min;

};

class TrackCutDcaPtDependent : public TrackCut {
  // to reduce pile up tracks

  //
  //
  //          dca
  //           |
  //           |
  //           |
  //           |                     pass
  //           |
  //           |
  //           |
  //     dca1  |----------- \
  //           |             \
  //           |              \
  //           |               \
  //     dca2  |                \----------------------------
  //           |
  //           |                   not pass
  //           |
  //          -+--------------------------------------------- pt
  //                      pt1 pt2
  //

public:
  TrackCutDcaPtDependent(double pt1 = 0.5, double dcaMax1 = 2.0, double pt2 = 1.0, double dcaMax2 = 1.0)
    : _pt1(pt1), _dcaMax1(dcaMax1), _pt2(pt2), _dcaMax2(dcaMax2) { }
  virtual ~TrackCutDcaPtDependent() { }

  bool operator()(const StSpinJet::Track& track)
  {
    if(track.pt < _pt1) {
      if(track.Tdca > _dcaMax1) return true;
    } else if(track.pt < _pt2) {
      if(track.Tdca*(_pt2 - _pt1) > (_pt2*_dcaMax1 - _pt1*_dcaMax2) + (_dcaMax2 - _dcaMax1)*track.pt) return true;
    } else {
       if(track.Tdca > _dcaMax2) return true;
    }

    return false;
  }

private:

  double  _pt1;
  double  _dcaMax1;
  double  _pt2;
  double  _dcaMax2;

};

class TrackCutEta : public TrackCut {

public:
  TrackCutEta(double min = -2.0, double max = 2.0) :_min(min), _max(max) { }
  virtual ~TrackCutEta() { }

  bool operator()(const StSpinJet::Track& track)
  {
    if(track.eta < _min) return true;

    if(track.eta > _max) return true;

    return false;
  }

private:

  double  _min;
  double  _max;

};

class TrackCutPossibleHitRatio : public TrackCut {
  // to avoid split tracks

public:
  TrackCutPossibleHitRatio(double minRatio = 0.51) :_minRatio(minRatio) { }
  virtual ~TrackCutPossibleHitRatio() { }

  bool operator()(const StSpinJet::Track& track)
  {
    if(static_cast<double>(track.nHits)/static_cast<double>(track.nHitsPoss) < .51) return true;

    return false;
  }

private:

  double  _minRatio;

};

}


namespace StSpinJet {

class StJetTPCTrackCut {

public:

  StJetTPCTrackCut() { }
  virtual ~StJetTPCTrackCut() { }

  TrackList operator()(const TrackList& trackList);

  void addCut(StJetTrackCut::TrackCut* cut) {
    _cutList.push_back(cut);
  }

  typedef std::vector<StJetTrackCut::TrackCut*> CutList;
  CutList getCutList() { return _cutList; }

private:

  bool shoudNotPass(const Track& track);

  CutList _cutList;

};

}

#endif // STJETTPCTRACKCUT_H

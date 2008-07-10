// -*- mode: c++;-*-
// $Id: StJetTPC.h,v 1.3 2008/07/10 19:35:30 tai Exp $
#ifndef STJETTPC_H
#define STJETTPC_H

#include <vector>

namespace StSpinJet {

class StMuTrackEmu;

class StJetTPC {

public:
  StJetTPC() { }
  virtual ~StJetTPC() { }

  virtual void Init() { }

  typedef std::vector<StMuTrackEmu*> TrackList;

  virtual TrackList getTrackList() = 0;

  virtual bool isUsed() const { return true; }
};


class StJetTPCNull : public StJetTPC {

public:
  StJetTPCNull() { }
  virtual ~StJetTPCNull() { }

  void Init() { }

  TrackList getTrackList() { return TrackList(); }

  bool isUsed() const { return false; }
};

}

#endif // STJETTPC_H

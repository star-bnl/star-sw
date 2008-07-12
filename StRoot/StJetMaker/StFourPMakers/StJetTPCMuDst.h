// -*- mode: c++;-*-
// $Id: StJetTPCMuDst.h,v 1.2 2008/07/12 01:32:07 tai Exp $
#ifndef STJETTPCMUDST_H
#define STJETTPCMUDST_H

#include "StJetTPC.h"

#include <vector>

class StMuDstMaker;

namespace StSpinJet {

class StJetTPCMuDst : public StJetTPC {

public:
  StJetTPCMuDst(StMuDstMaker* uDstMaker);
  virtual ~StJetTPCMuDst() { }

  TrackList getTrackList();

private:
  StMuDstMaker* _uDstMaker;

};

}

#endif // STJETTPCMUDST_H

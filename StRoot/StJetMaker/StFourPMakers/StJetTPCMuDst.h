// -*- mode: c++;-*-
// $Id: StJetTPCMuDst.h,v 1.1 2008/07/08 04:16:05 tai Exp $
#ifndef STJETTPCMUDST_H
#define STJETTPCMUDST_H

#include "StJetTPC.h"

#include <vector>

class StMuDstMaker;

namespace StSpinJet {

class StMuTrackEmu;

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

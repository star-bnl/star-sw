// -*- mode: c++;-*-
// $Id: StJetTPCMuDst.h,v 1.1 2008/07/21 17:25:16 tai Exp $
#ifndef STJETTPCMUDST_H
#define STJETTPCMUDST_H

#include "StJetTPC.h"

#include <vector>

class StMuDstMaker;
class StMuTrack;

namespace StSpinJet {

class StJetTPCMuDst : public StJetTPC {

public:
  StJetTPCMuDst(StMuDstMaker* uDstMaker);
  virtual ~StJetTPCMuDst() { }

  TrackList getTrackList();

private:

  Track createTrack(const StMuTrack* mutrack, int i, double magneticField);

  StMuDstMaker* _uDstMaker;

};

}

#endif // STJETTPCMUDST_H

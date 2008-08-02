// -*- mode: c++;-*-
// $Id: StjTPCMuDst.h,v 1.2 2008/08/02 19:23:20 tai Exp $
#ifndef STJETTPCMUDST_H
#define STJETTPCMUDST_H

#include "StjTPC.h"

#include <vector>

class StMuDstMaker;
class StMuTrack;

namespace StSpinJet {

class StjTPCMuDst : public StjTPC {

public:
  StjTPCMuDst(StMuDstMaker* uDstMaker);
  virtual ~StjTPCMuDst() { }

  StjTrackList getTrackList();

private:

  StjTrack createTrack(const StMuTrack* mutrack, int i, double magneticField);

  StMuDstMaker* _uDstMaker;

};

}

#endif // STJETTPCMUDST_H

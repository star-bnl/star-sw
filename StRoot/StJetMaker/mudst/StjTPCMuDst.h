// -*- mode: c++;-*-
// $Id: StjTPCMuDst.h,v 1.3 2008/08/02 22:43:38 tai Exp $
#ifndef STJTPCMUDST_H
#define STJTPCMUDST_H

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

#endif // STJTPCMUDST_H

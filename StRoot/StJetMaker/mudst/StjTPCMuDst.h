// -*- mode: c++;-*-
// $Id: StjTPCMuDst.h,v 1.4 2008/08/03 00:29:03 tai Exp $
#ifndef STJTPCMUDST_H
#define STJTPCMUDST_H

#include "StjTPC.h"

#include <vector>

class StMuDstMaker;
class StMuTrack;

class StjTPCMuDst : public StjTPC {

public:
  StjTPCMuDst(StMuDstMaker* uDstMaker);
  virtual ~StjTPCMuDst() { }

  StjTrackList getTrackList();

private:

  StjTrack createTrack(const StMuTrack* mutrack, int i, double magneticField);

  StMuDstMaker* _uDstMaker;

};

#endif // STJTPCMUDST_H

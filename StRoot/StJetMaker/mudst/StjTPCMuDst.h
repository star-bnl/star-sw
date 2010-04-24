// -*- mode: c++;-*-
// $Id: StjTPCMuDst.h,v 1.5 2010/04/24 04:15:39 pibero Exp $
#ifndef STJTPCMUDST_H
#define STJTPCMUDST_H

#include "StjTPC.h"

class StMuDstMaker;
class StMuTrack;

class StjTPCMuDst : public StjTPC {

public:
  StjTPCMuDst(StMuDstMaker*) {}
  virtual ~StjTPCMuDst() {}

  StjTrackList getTrackList();

private:

  StjTrack createTrack(const StMuTrack* mutrack, int i, double magneticField);
};

#endif // STJTPCMUDST_H

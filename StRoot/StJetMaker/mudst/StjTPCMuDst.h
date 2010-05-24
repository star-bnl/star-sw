// -*- mode: c++;-*-
// $Id: StjTPCMuDst.h,v 1.6 2010/05/24 17:42:39 pibero Exp $
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

protected:

  StjTrack createTrack(const StMuTrack* mutrack, int i, double magneticField);
};

#endif // STJTPCMUDST_H

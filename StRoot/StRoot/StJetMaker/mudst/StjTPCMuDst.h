// -*- mode: c++;-*-
// $Id: StjTPCMuDst.h,v 1.7 2010/05/30 07:10:06 pibero Exp $
#ifndef STJTPCMUDST_H
#define STJTPCMUDST_H

#include "StjTPC.h"

class StMuTrack;

class StjTPCMuDst : public StjTPC {

public:
  StjTPCMuDst() {}
  virtual ~StjTPCMuDst() {}

  int currentVertexIndex() const;
  void setVertexIndex(int i);
  int numberOfVertices() const;
  StjPrimaryVertex getVertex() const;
  StjTrackList getTrackList();

protected:

  StjTrack createTrack(const StMuTrack* mutrack, int i, double magneticField);
};

#endif // STJTPCMUDST_H

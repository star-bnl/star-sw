// -*- mode: c++;-*-
// $Id: StMuTrackEmuFactory.h,v 1.4 2008/07/07 17:55:24 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STMUTRACKEMUFACTORY_H
#define STMUTRACKEMUFACTORY_H

class StMuTrack;

namespace StSpinJet {

class StMuTrackEmu;

class StMuTrackEmuFactory {

public:

  StMuTrackEmuFactory() { }
  virtual ~StMuTrackEmuFactory() { }

  static StMuTrackEmu* createStMuTrackEmu(const StMuTrack* track, int trackIndex);

};

}

#endif // STMUTRACKEMUFACTORY_H

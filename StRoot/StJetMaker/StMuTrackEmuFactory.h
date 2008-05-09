// -*- mode: c++;-*-
// $Id: StMuTrackEmuFactory.h,v 1.2 2008/05/09 02:14:52 tai Exp $
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

  StMuTrackEmu* createStMuTrackEmu(const StMuTrack* track) const;

};

}

#endif // STMUTRACKEMUFACTORY_H

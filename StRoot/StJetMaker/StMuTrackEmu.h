// -*- mode: c++;-*-
// $Id: StMuTrackEmu.h,v 1.3 2008/05/09 02:14:52 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STMUTRACKEMU_H
#define STMUTRACKEMU_H

namespace StSpinJet {

class StMuTrackEmu {

public:

  StMuTrackEmu() { }
  virtual ~StMuTrackEmu() { }

  short flag() const { return _flag; }
  unsigned short nHits() const { return _nHits; }

private:

  friend class StMuTrackEmuFactory;

  short _flag;
  unsigned short _nHits;

};

}

#endif // STMUTRACKEMU_H

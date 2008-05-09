// -*- mode: c++;-*-
// $Id: StMuTrackEmu.h,v 1.1 2008/05/09 00:54:36 tai Exp $
#ifndef STMUTRACKEMU_H
#define STMUTRACKEMU_H

namespace StSpinJet {

class StMuTrackEmu {

public:

  StMuTrackEmu() { }
  virtual ~StMuTrackEmu() { }

  short flag() const { return _flag; }
  unsigned short nHits() const { return _NHits; }

private:

  short _flag;
  unsigned short _NHits;

};

}

#endif // STMUTRACKEMU_H

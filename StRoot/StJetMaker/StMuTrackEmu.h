// -*- mode: c++;-*-
// $Id: StMuTrackEmu.h,v 1.2 2008/05/09 02:11:54 tai Exp $
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

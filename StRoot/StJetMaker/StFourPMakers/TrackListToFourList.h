// -*- mode: c++;-*-
// $Id: TrackListToFourList.h,v 1.1 2008/07/09 10:41:27 tai Exp $
#ifndef TRACKLISTTOFOURLIST_H
#define TRACKLISTTOFOURLIST_H

#include <StJetFinder/AbstractFourVec.h>

#include <vector>

typedef std::vector<AbstractFourVec*> FourList;

namespace StSpinJet {

 class StMuTrackEmu;

class TrackListToFourList {

public:
  TrackListToFourList() { }
  virtual ~TrackListToFourList() { }

  typedef std::vector<StSpinJet::StMuTrackEmu*> TrackList;

  FourList operator()(const TrackList& trackList);

private:

};

}


#endif // TRACKLISTTOFOURLIST_H

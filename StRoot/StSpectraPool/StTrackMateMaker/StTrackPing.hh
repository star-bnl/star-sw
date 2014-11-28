//
// $Id: StTrackPing.hh,v 1.2 2009/11/10 20:57:28 fisyak Exp $
// simple struct
// for keeping track of common hits between tracks
//
#ifndef StTrackPing_hh
#define StTrackPing_hh
class StTrack;

struct StTrackPing {
    StTrack*  mTrack;
    unsigned int    mNPings;
};

bool compStTrackPing(StTrackPing& rhs, StTrackPing& lhs);
#endif

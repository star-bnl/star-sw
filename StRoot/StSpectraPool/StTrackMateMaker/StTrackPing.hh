//
// $Id: StTrackPing.hh,v 1.1 2004/09/13 22:04:53 calderon Exp $
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

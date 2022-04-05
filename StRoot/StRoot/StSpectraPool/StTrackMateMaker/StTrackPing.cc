//
// $Id: StTrackPing.cc,v 1.2 2009/11/10 20:57:28 fisyak Exp $
//
#include "StTrackPing.hh"
bool compStTrackPing(StTrackPing& rhs, StTrackPing& lhs){
    return rhs.mNPings<lhs.mNPings;
}

//
// $Id: StTrackPing.cc,v 1.1 2004/09/13 22:04:53 calderon Exp $
//
#include "StTrackPing.hh"
bool compStTrackPing(StTrackPing& rhs, StTrackPing& lhs){
    return rhs.mNPings<lhs.mNPings;
}

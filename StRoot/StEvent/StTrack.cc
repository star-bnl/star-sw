/***************************************************************************
 *
 * $Id: StTrack.cc,v 1.1 1999/01/15 20:40:11 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrack.cc,v $
 * Revision 1.1  1999/01/15 20:40:11  wenaus
 * Commit Thomas' original code
 *
 * Revision 1.2  1999/01/15 22:54:02  wenaus
#include "StTrack.hh"
#include "StEvent/StTrack.hh"


StTrack::StTrack(dst_track_st* trk) : mFitTraits(trk),
  mHelix(0, 0, 0, StThreeVector<double>())
{
{  
}


StTrack::~StTrack() { /* noop */ }

int StTrack::operator==(const StTrack& t) const
{
    return t.mHelix == mHelix;
}

int StTrack::operator!=(const StTrack& t) const
{
    return !(t == *this);
}

void StTrack::setHelix(const StPhysicalHelix& val) { mHelix = val; }

void StTrack::setStartVertex(StVertex* val) { mStartVertex = val; }

void StTrack::setStopVertex(StVertex* val) { mStopVertex = val; }

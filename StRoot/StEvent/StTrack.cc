/***************************************************************************
 *
 * $Id: StTrack.cc,v 1.2 1999/01/15 22:54:02 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 *
 * History:
 * 15/01/1999 T. Wenaus  Add table-based constructor
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrack.cc,v $
 * Revision 1.2  1999/01/15 22:54:02  wenaus
 * version with constructors for table-based loading
 *
 * Revision 1.2  1999/01/15 22:54:02  wenaus
 * version with constructors for table-based loading
 *
static const char rcsid[] = "$Id: StTrack.cc,v 1.2 1999/01/15 22:54:02 wenaus Exp $";
#include "StEvent/StTrack.hh"

static const char rcsid[] = "$Id: StTrack.cc,v 1.2 1999/01/15 22:54:02 wenaus Exp $";

StTrack::StTrack() : mHelix(0, 0, 0, StThreeVector<double>())
{
    mStartVertex = 0;
StTrack::StTrack(dst_track_st* trk) : mFitTraits(trk), mHelix(0, 0, 0, StThreeVector<double>())

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

/***************************************************************************
 *
 * $Id: StTrack.cc,v 1.4 1999/02/15 16:17:03 wenaus Exp $
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
 * Revision 1.4  1999/02/15 16:17:03  wenaus
 * fix double& -> double referencing bug
 *
 * Revision 1.4  1999/02/15 16:17:03  wenaus
 * fix double& -> double referencing bug
 *
 * Revision 1.3  1999/02/12 02:01:19  wenaus
 * New track constructor to load helix params independently of table
 *
 * Revision 1.2  1999/01/15 22:54:02  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StEvent/StTrack.hh"

static const char rcsid[] = "$Id: StTrack.cc,v 1.4 1999/02/15 16:17:03 wenaus Exp $";

StTrack::StTrack() : mHelix(0, 0, 0, StThreeVector<double>())
{
    mStartVertex = 0;
    mStopVertex   = 0;
}

StTrack::StTrack(dst_track_st* trk) : mFitTraits(trk),
  mHelix(0, 0, 0, StThreeVector<double>())
{
    mStartVertex = 0;
    mStopVertex   = 0;
}

StTrack::StTrack(dst_track_st* trk,
                 double curvature,
                 double dip,
                 double phase,
                 StThreeVector<double>& origin) : mFitTraits(trk),
  mHelix(curvature, dip, phase, origin)
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

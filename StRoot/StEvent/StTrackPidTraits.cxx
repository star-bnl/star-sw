/***************************************************************************
 *
 * $Id: StTrackPidTraits.cxx,v 2.4 1999/11/29 17:07:29 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackPidTraits.cxx,v $
 * Revision 2.4  1999/11/29 17:07:29  ullrich
 * Moved method() from StTrackPidTraits to StDedxPidTraits.cxx
 *
 * Revision 2.3  1999/11/29 16:53:24  ullrich
 * ADopted new encoding scheme for method().
 *
 * Revision 2.2  1999/11/15 18:48:25  ullrich
 * Adapted new enums for dedx and track reco methods.
 *
 * Revision 2.1  1999/10/28 22:27:49  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:43:05  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#include "tables/St_dst_dedx_Table.h"
#include "StTrackPidTraits.h"

ClassImp(StTrackPidTraits)

static const char rcsid[] = "$Id: StTrackPidTraits.cxx,v 2.4 1999/11/29 17:07:29 ullrich Exp $";

StTrackPidTraits::StTrackPidTraits() :
    mDetectorId(0) { /* noop */ }

StTrackPidTraits::StTrackPidTraits(StDetectorId det) :
    mDetectorId(det) { /* noop */ }

StTrackPidTraits::StTrackPidTraits(const dst_dedx_st& t) :
    mDetectorId(t.det_id) { /* noop */ }

StTrackPidTraits::~StTrackPidTraits() { /* noop */ }

Short_t
StTrackPidTraits::detector() const { return mDetectorId; }

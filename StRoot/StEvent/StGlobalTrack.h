/***************************************************************************
 *
 * $Id: StGlobalTrack.h,v 2.0 1999/10/12 18:42:15 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StGlobalTrack.h,v $
 * Revision 2.0  1999/10/12 18:42:15  ullrich
 * Completely Revised for New Version
 *
 * Revision 2.0  1999/10/12 18:42:15  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#ifndef StGlobalTrack_hh
#define StGlobalTrack_hh

#include "StTrack.h"

class StGlobalTrack : public StTrack {
public:
    StGlobalTrack();
    StGlobalTrack(const dst_track_st&);
    StGlobalTrack(const StGlobalTrack&);
    StGlobalTrack& operator=(const StGlobalTrack&);
    ~StGlobalTrack();

    StTrackType     type() const;
    const StVertex* vertex() const;

    ClassDef(StGlobalTrack,1)
};
#endif

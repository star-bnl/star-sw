/***************************************************************************
 *
 * $Id: StTptTrack.h,v 2.1 2000/08/17 00:10:38 ullrich Exp $
 *
 * Author: Thomas Ullrich, Aug 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTptTrack.h,v $
 * Revision 2.1  2000/08/17 00:10:38  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StTptTrack_hh
#define StTptTrack_hh

#include "StTrack.h"

class StTptTrack : public StTrack {
public:
    StTptTrack();
    StTptTrack(const dst_track_st&);
    StTptTrack(const StTptTrack&);
    StTptTrack& operator=(const StTptTrack&);
    ~StTptTrack();

    StTrackType     type() const;
    const StVertex* vertex() const;

protected:
    StObject* clone();
    ClassDef(StTptTrack,1)
};
#endif

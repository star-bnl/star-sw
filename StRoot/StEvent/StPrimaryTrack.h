/***************************************************************************
 *
 * $Id: StPrimaryTrack.h,v 2.1 1999/10/13 19:43:31 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPrimaryTrack.h,v $
 * Revision 2.1  1999/10/13 19:43:31  ullrich
 * Initial Revision
 *
 * Revision 2.1  1999/10/13 19:43:31  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StPrimaryTrack_hh
#define StPrimaryTrack_hh

#include "StTrack.h"
class StPrimaryVertex;

class StPrimaryTrack : public StTrack {
public:
    StPrimaryTrack();
    StPrimaryTrack(const dst_track_st&);
    StPrimaryTrack(const StPrimaryTrack&);
    StPrimaryTrack& operator=(const StPrimaryTrack&);
    ~StPrimaryTrack();

    StTrackType      type() const;
    const StVertex*  vertex() const;

    void setVertex(StVertex*);
    
private:
    StPrimaryVertex*  mVertex; //$LINK
    
    ClassDef(StPrimaryTrack,1)
};
#endif

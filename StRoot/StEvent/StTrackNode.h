/***************************************************************************
 *
 * $Id: StTrackNode.h,v 2.3 1999/11/05 15:27:12 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackNode.h,v $
 * Revision 2.3  1999/11/05 15:27:12  ullrich
 * Added non-const versions of several methods
 *
 * Revision 2.3  1999/11/05 15:27:12  ullrich
 * Added non-const versions of several methods
 *
 * Revision 2.2  1999/10/28 22:27:47  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:44:15  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StTrackNode_hh
#define StTrackNode_hh

#include "StObject.h"
#include "StContainers.h"
#include "StEnumerations.h"
class StTrack;

class StTrackNode : public StObject {
public:
    StTrackNode();
    virtual ~StTrackNode();

    void           addTrack(StTrack*);
    void           removeTrack(StTrack*);

    UInt_t         entries() const;
    StTrack*       track(UInt_t);
    const StTrack* track(UInt_t) const;

    UInt_t         entries(StTrackType) const;
    StTrack*       track(StTrackType, UInt_t = 0);
    const StTrack* track(StTrackType, UInt_t = 0) const;

    friend class StTrack;
    
private:
    StTrackNode(const StTrackNode&);
    StTrackNode& operator=(const StTrackNode&);
    void unlink(StTrack*);
    
    StSPtrVecTrack  mOwnedTracks;
    StPtrVecTrack   mReferencedTracks;

    ClassDef(StTrackNode,1)
};
#endif

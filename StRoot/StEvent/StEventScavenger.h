/***************************************************************************
 *
 * $Id: StEventScavenger.h,v 2.3 2000/10/16 21:06:32 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 2000
 ***************************************************************************
 *
 * Description:  
 *
 ***************************************************************************
 *
 * $Log: StEventScavenger.h,v $
 * Revision 2.3  2000/10/16 21:06:32  ullrich
 * Added new method: removeTpcHitsNotOnTracks()
 *
 * Revision 2.2  2000/09/27 02:53:23  ullrich
 * No delete, create only zombies.
 *
 * Revision 2.1  2000/09/25 18:03:34  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StEventScavenger_hh
#define StEventScavenger_hh

#include "StEnumerations.h"
class StEvent;
class StTrack;

class StEventScavenger {
public:
    static bool removeEventSummary(StEvent*);
    static bool removeSoftwareMonitor(StEvent*);
    static bool removeTpcHitCollection(StEvent*);
    static bool removeFtpcHitCollection(StEvent*);
    static bool removeSvtHitCollection(StEvent*);
    static bool removeSsdHitCollection(StEvent*);
    static bool removeEmcCollection(StEvent*);
    static bool removeRichCollection(StEvent*);
    static bool removeTriggerDetectorCollection(StEvent*);
    static bool removeL3Trigger(StEvent*);
    static bool removeV0Vertices(StEvent*);
    static bool removeXiVertices(StEvent*);
    static bool removeKinkVertices(StEvent*);

    static bool remove(StTrack*);

    static bool removeTpcHitsNotOnTracks(StEvent*);
};
#endif

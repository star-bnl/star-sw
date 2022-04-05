/*!
 * \class StEventScavenger
 * \author Thomas Ullrich, Sep 2000
 */
/***************************************************************************
 *
 * $Id: StEventScavenger.h,v 2.10 2015/05/13 17:06:13 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEventScavenger.h,v $
 * Revision 2.10  2015/05/13 17:06:13  ullrich
 * Added hooks and interfaces to Sst detector (part of HFT).
 *
 * Revision 2.9  2011/02/01 19:47:36  ullrich
 * Added HLT branch and hooks.
 *
 * Revision 2.8  2010/08/31 19:55:13  fisyak
 * Remove SoftwareMonitors
 *
 * Revision 2.7  2002/02/22 22:56:47  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.6  2002/01/19 00:14:17  ullrich
 * Corrected typo.
 *
 * Revision 2.5  2002/01/17 02:06:29  ullrich
 * Added the removal of objects recently added to StEvent.
 *
 * Revision 2.4  2001/04/05 04:00:36  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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
    static bool removeTpcHitCollection(StEvent*);
    static bool removeFtpcHitCollection(StEvent*);
    static bool removeSvtHitCollection(StEvent*);
    static bool removeSsdHitCollection(StEvent*);
    static bool removeSstHitCollection(StEvent*);
    static bool removeEmcCollection(StEvent*);
    static bool removeRichCollection(StEvent*);
    static bool removeTriggerDetectorCollection(StEvent*);
    static bool removeL3Trigger(StEvent*);
    static bool removeV0Vertices(StEvent*);
    static bool removeXiVertices(StEvent*);
    static bool removeKinkVertices(StEvent*);
    static bool removeFpdCollection(StEvent*);
    static bool removeTofCollection(StEvent*);
    static bool removeCalibrationVertices(StEvent*);
    static bool removeHltEvent(StEvent*);
 
    static bool remove(StTrack*);

    static bool removeTpcHitsNotOnTracks(StEvent*);
};
#endif

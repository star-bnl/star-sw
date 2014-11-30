/***************************************************************************
 *
 * $Id: StEventScavenger.cxx,v 2.10 2011/02/01 19:47:36 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEventScavenger.cxx,v $
 * Revision 2.10  2011/02/01 19:47:36  ullrich
 * Added HLT branch and hooks.
 *
 * Revision 2.9  2010/08/31 19:55:13  fisyak
 * Remove SoftwareMonitors
 *
 * Revision 2.8  2002/01/19 00:14:16  ullrich
 * Corrected typo.
 *
 * Revision 2.7  2002/01/17 02:06:29  ullrich
 * Added the removal of objects recently added to StEvent.
 *
 * Revision 2.6  2001/06/10 21:02:49  perev
 * Solaris: <typeinfo> removed
 *
 * Revision 2.5  2001/04/05 04:00:49  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.4  2001/03/16 04:59:24  ullrich
 * Added missing method removeTriggerDetectorCollection().
 *
 * Revision 2.3  2000/10/16 21:06:32  ullrich
 * Added new method: removeTpcHitsNotOnTracks()
 *
 * Revision 2.2  2000/09/27 02:53:23  ullrich
 * No delete, create only zombies.
 *
 * Revision 2.1  2000/09/25 18:03:13  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StEventScavenger.h"
#include "StEventTypes.h"

bool StEventScavenger::removeEventSummary(StEvent* evt)
{
    if (evt && evt->summary()) {
        evt->summary()->makeZombie();
        return true;
    }
    else
        return false;
}

bool StEventScavenger::removeL3Trigger(StEvent* evt)
{
    if (evt && evt->l3Trigger()) {
        evt->l3Trigger()->makeZombie();
        return true;
    }
    else
        return false;
}

bool StEventScavenger::removeV0Vertices(StEvent* evt)
{
    if (!evt) return false;
    StSPtrVecV0Vertex &vec = evt->v0Vertices();
    for (unsigned int i=0; i<vec.size(); i++)
        vec[i]->makeZombie();
    return true;
}

bool StEventScavenger::removeXiVertices(StEvent* evt)
{
    if (!evt) return false;
    StSPtrVecXiVertex &vec = evt->xiVertices();
    for (unsigned int i=0; i<vec.size(); i++)
        vec[i]->makeZombie();
    return true;
}

bool StEventScavenger::removeKinkVertices(StEvent* evt)
{
    if (!evt) return false;
    StSPtrVecKinkVertex &vec = evt->kinkVertices();
    for (unsigned int i=0; i<vec.size(); i++)
        vec[i]->makeZombie();
    return true;
}

bool StEventScavenger::removeCalibrationVertices(StEvent* evt)
{
    if (!evt) return false;
    for (unsigned int i=0; i<evt->numberOfCalibrationVertices(); i++)
        evt->calibrationVertex(i)->makeZombie();
    return true;
}

bool StEventScavenger::removeTpcHitCollection(StEvent* evt)
{
    if (evt && evt->tpcHitCollection()) {
        StTpcHitCollection *theHits = evt->tpcHitCollection();
        for (unsigned int n=0; n<theHits->numberOfSectors(); n++)
            for (unsigned int m=0; m<theHits->sector(n)->numberOfPadrows(); m++)
                for (unsigned int h=0; h<theHits->sector(n)->padrow(m)->hits().size(); h++)
                    theHits->sector(n)->padrow(m)->hits()[h]->makeZombie();
        theHits->makeZombie();
        return true;
    }
    else
        return false;
}

bool StEventScavenger::removeTpcHitsNotOnTracks(StEvent* evt)
{
    if (evt && evt->tpcHitCollection()) {
        // first remove all hits not associated with a track at all
        StTpcHitCollection *theHits = evt->tpcHitCollection();
        for (unsigned int n=0; n<theHits->numberOfSectors(); n++)
            for (unsigned int m=0; m<theHits->sector(n)->numberOfPadrows(); m++)
                for (unsigned int h=0; h<theHits->sector(n)->padrow(m)->hits().size(); h++)
                    if (theHits->sector(n)->padrow(m)->hits()[h]->trackReferenceCount() == 0)
                        theHits->sector(n)->padrow(m)->hits()[h]->makeZombie();
        // now all hits associated with zombie tracks
        StSPtrVecTrackNode& nodes = evt->trackNodes();
        for (unsigned int i = 0; i < nodes.size(); i++) {   // loop nodes
            StTrackNode* node = nodes[i];
            for (unsigned int j = 0; j < node->entries(); j++) {   // loop tracks in node
                if (node->track(j)->isZombie()) {
                    StTrack* track = node->track(j);
                    StTrackDetectorInfo* info = track->detectorInfo();
                    if (info) {
                        StPtrVecHit& hitList = info->hits();
                        for (unsigned int k = 0; k < hitList.size(); k++)   // loop hits
                            if (hitList[k]->detector() == kTpcId) hitList[k]->makeZombie();
                    }
                }
            }
        }
        return true;
    }
    else
        return false;
}

bool StEventScavenger::removeFtpcHitCollection(StEvent* evt)
{
    if (evt && evt->ftpcHitCollection()) {
        StFtpcHitCollection *theHits = evt->ftpcHitCollection();
        for (unsigned int n=0; n<theHits->numberOfPlanes(); n++)
            for (unsigned int m=0; m<theHits->plane(n)->numberOfSectors(); m++)
                for (unsigned int h=0; h<theHits->plane(n)->sector(m)->hits().size(); h++)
                    theHits->plane(n)->sector(m)->hits()[h]->makeZombie();
        theHits->makeZombie();
        return true;
    }
    else
        return false;
}

bool StEventScavenger::removeSvtHitCollection(StEvent* evt)
{
    if (evt && evt->svtHitCollection()) {
        StSvtHitCollection *theHits = evt->svtHitCollection();
        for (unsigned int n=0; n<theHits->numberOfBarrels(); n++)
            for (unsigned int m=0; m<theHits->barrel(n)->numberOfLadders(); m++)
                for (unsigned int k=0; k<theHits->barrel(n)->ladder(k)->numberOfWafers(); k++)
                    for (unsigned int h=0; h<theHits->barrel(n)->ladder(m)->wafer(k)->hits().size(); h++)
                    theHits->barrel(n)->ladder(m)->wafer(k)->hits()[h]->makeZombie();
        theHits->makeZombie();
        return true;
    }
    else
        return false;
}

bool StEventScavenger::removeSsdHitCollection(StEvent* evt)
{
    if (evt && evt->ssdHitCollection()) {
        StSsdHitCollection *theHits = evt->ssdHitCollection();
            for (unsigned int m=0; m<theHits->numberOfLadders(); m++)
                for (unsigned int k=0; k<theHits->ladder(k)->numberOfWafers(); k++)
                    for (unsigned int h=0; h<theHits->ladder(m)->wafer(k)->hits().size(); h++)
                    theHits->ladder(m)->wafer(k)->hits()[h]->makeZombie();
        theHits->makeZombie();
        return true;
    }
    else
        return false;
}

bool StEventScavenger::removeEmcCollection(StEvent* evt)
{
    if (evt && evt->emcCollection()) {
        evt->emcCollection()->makeZombie();
        return true;
    }
    else
        return false;
}

bool StEventScavenger::removeRichCollection(StEvent* evt)
{
    if (evt && evt->richCollection()) {
        evt->richCollection()->makeZombie();
        return true;
    }
    else
        return false;
}

bool StEventScavenger::removeTriggerDetectorCollection(StEvent* evt)
{
    if (evt && evt->triggerDetectorCollection()) {
        evt->triggerDetectorCollection()->makeZombie();
        return true;
    }
    else
        return false;
}

bool StEventScavenger::removeFpdCollection(StEvent* evt)
{
    if (evt && evt->fpdCollection()) {
        evt->fpdCollection()->makeZombie();
        return true;
    }
    else
        return false;
}

bool StEventScavenger::removeTofCollection(StEvent* evt)
{
    if (evt && evt->tofCollection()) {
        evt->tofCollection()->makeZombie();
        return true;
    }
    else
        return false;
}

bool StEventScavenger::removeHltEvent(StEvent* evt)
{
    if (evt && evt->hltEvent()) {
        evt->hltEvent()->makeZombie();
        return true;
    }
    else
        return false;
}

bool StEventScavenger::remove(StTrack* track)
{
    if (!track) return false;

    track->makeZombie();

    //
    //   Scan all tracks in node. If all are zombies we can
    //   make the node a zombie too. We also remove the detector
    //   info associated with the track if no other living
    //   track is using it.
    //
    StTrackNode* node = track->node();
    StTrackDetectorInfo* info = track->detectorInfo();
    unsigned int nTimesUsed  = 0;
    unsigned int nZombies = 0;
    StTrack *someTrack;
    if (node) {
        unsigned int i;
        for (i=0; i<node->entries(); i++) {
            someTrack = node->track(i);
            if (someTrack->isZombie()) nZombies++;
            if (someTrack != track && !someTrack->isZombie() &&
                someTrack->detectorInfo() == info) nTimesUsed++;
        }
        if (node->entries() == nZombies) node->makeZombie();
    }
    if (info && nTimesUsed < 1) info->makeZombie();

    return true;
}

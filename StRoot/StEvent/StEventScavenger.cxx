/***************************************************************************
 *
 * $Id: StEventScavenger.cxx,v 2.1 2000/09/25 18:03:13 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 2000
 ***************************************************************************
 *
 * Description:  
 *
 ***************************************************************************
 *
 * $Log: StEventScavenger.cxx,v $
 * Revision 2.1  2000/09/25 18:03:13  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include <typeinfo>
#include "StEventScavenger.h"
#include "StEventTypes.h"

bool StEventScavenger::removeEventSummary(StEvent* evt)
{
    if (!evt) return false;
    evt->setSummary(0);
    return true;
}

bool StEventScavenger::removeSoftwareMonitor(StEvent* evt)
{
    if (!evt) return false;
    evt->setSoftwareMonitor(0);
    return true;
}

bool StEventScavenger::removeL3Trigger(StEvent* evt)
{
    if (!evt) return false;
    evt->setL3Trigger(0);
    return true;
}

bool StEventScavenger::removeV0Vertices(StEvent* evt)
{
    if (!evt) return false;
    evt->v0Vertices().clear();
    return true;
}

bool StEventScavenger::removeXiVertices(StEvent* evt)
{
    if (!evt) return false;
    evt->xiVertices().clear();
    return true;
}

bool StEventScavenger::removeKinkVertices(StEvent* evt)
{
    if (!evt) return false;
    evt->kinkVertices().clear();
    return true;
}

bool StEventScavenger::removeTpcHitCollection(StEvent* evt)
{
    if (!evt) return false;
    evt->setTpcHitCollection(0);
    removeAllTrackReferences(evt, kTpcId);
    return true;
}

bool StEventScavenger::removeFtpcHitCollection(StEvent* evt)
{
    if (!evt) return false;
    evt->setFtpcHitCollection(0);
    removeAllTrackReferences(evt, kFtpcWestId);
    removeAllTrackReferences(evt, kFtpcEastId);
    return true;
}

bool StEventScavenger::removeSvtHitCollection(StEvent* evt)
{
    if (!evt) return false;
    evt->setSvtHitCollection(0);
    removeAllTrackReferences(evt, kSvtId);
    return true;
}

bool StEventScavenger::removeSsdHitCollection(StEvent* evt)
{
    if (!evt) return false;
    evt->setSsdHitCollection(0);
    removeAllTrackReferences(evt, kSsdId);
    return true;
}

bool StEventScavenger::removeEmcCollection(StEvent* evt)
{
    if (!evt) return false;
    evt->setEmcCollection(0);
    return true;
}

bool StEventScavenger::removeRichCollection(StEvent* evt)
{
    if (!evt) return false;
    evt->setRichCollection(0);
    return true;
}

bool StEventScavenger::removeAllTrackReferences(StEvent* evt, StDetectorId id)
{
    if (!evt) return false;

    StSPtrVecTrackDetectorInfo&  infos = evt->trackDetectorInfo();
    for (unsigned int i=0; i<infos.size(); i++) {
	StPtrVecHit&  hits = infos[i]->hits();
	StPtrVecHitIterator iter;
	for (iter = hits.begin(); iter < hits.end();) 
	    if ((*iter)->detector() == id)
		iter = hits.erase(iter);
	    else
		iter++;
    }
    return true;
}

bool StEventScavenger::removeAllTpcHitsNotOnTracks(StEvent* evt)
{
    if (!evt) return false;

    //
    //  Remove also all references to these hits
    //
    StSPtrVecTrackDetectorInfo&  infos = evt->trackDetectorInfo();
    for (unsigned int i=0; i<infos.size(); i++) {
	if (infos[i]->isZombie()) {
	    StPtrVecHit&  hits = infos[i]->hits();	
	    StPtrVecHitIterator iter;
	    for (iter = hits.begin(); iter < hits.end(); iter++)
		if ((*iter)->detector() == kTpcId)
		    (*iter)->makeZombie();
	}
    }
    return true;
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

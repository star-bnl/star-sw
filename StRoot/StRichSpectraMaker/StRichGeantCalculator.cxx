/***************************************************************************
 *
 * $Id: StRichGeantCalculator.cxx,v 1.1 2001/11/21 21:06:04 lasiuk Exp $
 *
 * Author:  bl Sept 10, 2001
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              
 ***************************************************************************
 *
 * $Log: StRichGeantCalculator.cxx,v $
 * Revision 1.1  2001/11/21 21:06:04  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#include "StRichGeantCalculator.h"
#include "StGlobals.hh"

StRichGeantCalculator::StRichGeantCalculator() {}
StRichGeantCalculator::~StRichGeantCalculator() {}

void StRichGeantCalculator::process(g2t_rch_hit_st *rch_hit, g2t_track_st *track)
{
    //
    // find the parent ID of the photon.  First get the id of
    // the track, then look at the g2t_track structures.
    //
    int volumeId = rch_hit->volume_id;
    if(!(volumeId == 2001 ||
	 volumeId == 2002 ||
	 volumeId == 2003 ||
	 volumeId == 2004)) {
// 	cout << "StRichGeantCalculator::process()\n";
// 	cout << "\tNot in CsI" << endl;
	return;
    }

    bool isPhoton = false;
    int index = rch_hit->track_p - 1;
    PR(index);

    if(rch_hit->de<0) {
	if(track[index].ge_pid != 50) {
	    // This should never ever be true
	    cout << "StRichGeantCalculator::process()\n";
	    cout << "\tde<0 && pid !=50" << endl;
	    abort();
	}
	isPhoton = true;
    }

    //
    // if the indexed track is from a shower,
    // it is bad
    //
    int parentIndex = track[index].next_parent_p - 1;
    
    if(track[parentIndex].is_shower) {
	this->addData(-999, track[parentIndex].ge_pid);
	return;
    }
    else if(!isPhoton) {
	this->addData(999, track[index].ge_pid);
	return;
    }
    
    //
    // else step thru the chain of parents
    //

//     cout << "Final index: " << index << endl;

//     PR(track[index].is_shower);
//     PR(track[index].next_parent_p);
    int parentPid = track[parentIndex].ge_pid;
//     cout << "ge+pid " << parentPid << endl;
    this->addData(parentIndex, parentPid);
    
}

bool StRichGeantCalculator::addData(int parentId, int parentGeantPid)
{

    //
    // Either add a photon to the existing gInfo structure
    //
    for(size_t ii=0; ii<mTracks.size(); ii++) {
	if(mTracks[ii].parentId == parentId) {
	    mTracks[ii].numberOfPhotons++;
	    return true;
	}
    }

    //
    // ...or make a new structure if the track
    // does not exist
    //
    gInfo theInfo;
    theInfo.parentId = parentId;
    theInfo.parentGeantPid = parentGeantPid;
    theInfo.numberOfPhotons = 1;
    mTracks.push_back(theInfo);
    return true;
}

void StRichGeantCalculator::status() const
{
    cout << "StRichGeantCalculator::status()" << endl;
    cout << mTracks.size() << " Tracks were found" << endl;
    for(size_t ii=0; ii<mTracks.size(); ii++) {
	cout << "-------------------------" << endl;
	cout << " Track:   " << mTracks[ii].parentId << endl;
	cout << " GID:     " << mTracks[ii].parentGeantPid << endl;
	cout << " Photons: " << mTracks[ii].numberOfPhotons << endl;
    }
    cout << "=============================================" << endl;
}

void StRichGeantCalculator::clearData()
{
    mTracks.clear();
}


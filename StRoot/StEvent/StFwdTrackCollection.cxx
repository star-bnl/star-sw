/***************************************************************************
 *
 * $Id: StFwdTrackCollection.cxx
 *
 * Author: jdb, Feb 2022
 ***************************************************************************
 *
 * Description: Forward Tracks
 *
 *
 **************************************************************************/

#include "StEvent/StFwdTrackCollection.h"
#include "StEvent/StFwdTrack.h"

ClassImp(StFwdTrackCollection)

StFwdTrackCollection::StFwdTrackCollection(){}

StFwdTrackCollection::~StFwdTrackCollection() {}

void StFwdTrackCollection::Clear( Option_t* ) {
	mTracks.clear();  // StSPtrVec::clear() deletes each owned pointer
}

void StFwdTrackCollection::addTrack( StFwdTrack *track ) {
	mTracks.push_back( track );
}

StSPtrVecFwdTrack& StFwdTrackCollection::tracks() {
	return mTracks;
}

const StSPtrVecFwdTrack& StFwdTrackCollection::tracks() const {
	return mTracks;
}

unsigned int StFwdTrackCollection::numberOfTracks() const {
	return mTracks.size();
}


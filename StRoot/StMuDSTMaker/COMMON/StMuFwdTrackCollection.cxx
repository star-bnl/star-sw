/***************************************************************************
 *
 * $Id: StMuFwdTrackCollection.cxx
 *
 * Author: jdb, 2021
 ***************************************************************************
 *
 * Description: Fwd Tracks
 *
 ***************************************************************************/

#include "StMuDSTMaker/COMMON/StMuFwdTrackCollection.h"
#include "StMuDSTMaker/COMMON/StMuFwdTrack.h"

#include "St_base/StMessMgr.h"

ClassImp(StMuFwdTrackCollection)

StMuFwdTrackCollection::StMuFwdTrackCollection() { mFwdTracks = 0;  }

StMuFwdTrackCollection::~StMuFwdTrackCollection() {
    delete mFwdTracks;
    mFwdTracks = nullptr;
}

void StMuFwdTrackCollection::init() {
    mFwdTracks     = new TClonesArray("StMuFwdTracks", 0);
}

StMuFwdTrack* StMuFwdTrackCollection::addFwdTrack(){
    if(!mFwdTracks) init();
    int counter = mFwdTracks->GetEntriesFast();
    StMuFwdTrack* newFwdTrack = new ((*mFwdTracks)[counter]) StMuFwdTrack();
    return newFwdTrack;
}

unsigned int StMuFwdTrackCollection::numberOfFwdTracks() const{
    if(!mFwdTracks) return 0;
    return mFwdTracks->GetEntriesFast();
}

StMuFwdTrack*  StMuFwdTrackCollection::getFwdTrack(int index){
    if(!mFwdTracks) return NULL;
    return (StMuFwdTrack*) mFwdTracks->At(index);
}

/***************************************************************************
 *
 * $Id: StMuFwdTrackCollection.h
 *
 * Author: jdb, 2023
 ***************************************************************************
 *
 * Description: Fwd Tracks
 *
 ***************************************************************************
 *
 *
 **************************************************************************/
#ifndef StMuFwdTrackCollection_hh
#define StMuFwdTrackCollection_hh

#include <TObject.h>
#include "TClonesArray.h"

class StMuFwdTrack;


class StMuFwdTrackCollection : public TObject {
public:
    StMuFwdTrackCollection();
    ~StMuFwdTrackCollection();
    
    void            init();
    StMuFwdTrack*  addFwdTrack();
    unsigned int    numberOfFwdTracks() const;
    void            setFwdTrackArray(TClonesArray *array) {mFwdTracks=array;};

    StMuFwdTrack*  getFwdTrack(int index);

    TClonesArray*   getFwdTrackArray() { return mFwdTracks; };

private:
    TClonesArray* mFwdTracks=0;

    ClassDef(StMuFwdTrackCollection,1)
};
#endif

#ifndef StBackgroundTrackCollection_h
#define StBackgroundTrackCollection_h

#include "StTrackForPool.h"
#include "StTrackForPoolCollection.h"
#include <vector>

class  StBackgroundTrackCollection {
public:
                    StBackgroundTrackCollection();
                    ~StBackgroundTrackCollection();
    void            AddTrackCollection(StTrackForPoolCollection tracks);
    StTrackForPool* GetTrack(int eventIndex, int trackIndex);
    StTrackForPoolCollection& GetTracks(int eventIndex);
    int             Size();
    void            Clear();

 vector<StTrackForPoolCollection> vec;

 private:
    
};



#endif

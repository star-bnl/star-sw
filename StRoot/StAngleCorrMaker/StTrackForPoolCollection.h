#ifndef StTrackForPoolCollection_h
#define StTrackForPoolCollection_h

#include "StTrackForPool.h"
#include <vector>

class  StTrackForPoolCollection {
public:
                    StTrackForPoolCollection();
                    ~StTrackForPoolCollection();
    void            AddTrack(StTrackForPool* track);
    StTrackForPool* GetTrack(int index);
    int             Size();
    void            Clear();

 vector<StTrackForPool*> vec;

 private:
    
};



#endif

#ifndef StTrackForPoolCollection_h
#define StTrackForPoolCollection_h

#include "StTrackForPool.h"
#include <vector>
#if !defined(ST_NO_NAMESPACES)
using std::string;
using std::vector;
#endif

class  StTrackForPoolCollection {
public:
                    StTrackForPoolCollection();
                    ~StTrackForPoolCollection();
    void            AddTrack(StTrackForPool* track);
    StTrackForPool* GetTrack(int index);
    int             Size();
    void            Clear();

#ifdef ST_NO_TEMPLATE_DEF_ARGS
    vector< StTrackForPool*, allocator<StTrackForPool*> > vec;
#else
    vector<StTrackForPool*> vec;
#endif 

 private:
    
};



#endif

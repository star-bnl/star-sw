//StiDynamicTrackFilter.h
//M.L. Miller (Yale Software)
//3/02

#ifndef StiDynamicTrackFilter_HH
#define StiDynamicTrackFilter_HH

#include <map>
using namespace std;
#include <iostream>
using namespace std;

#include "SubjectObserver.h"
#include "StiAbstractFilter.h"
#include "StiTrackFilter.h"

class ostream;
class StiTrack;

class StiDynamicTrackFilter : public Observer, public StiTrackFilter
{
public:

    typedef StiAbstractFilter<const StiTrack*> StiTrackFilter_t;
    typedef map<int, StiTrackFilter_t*> FilterMap;
    
    StiDynamicTrackFilter(StiIOBroker*);
    virtual ~StiDynamicTrackFilter();

    ///Define enumeration to filter types.
    enum FilterType {kPtFilter=0, kEtaFilter=1, kChi2Filter=2, kNptsFilter=3, kNFitPtsFilter=4,
		     kNGapsFilter=5, kFitPointRatioFilter=6, kPrimaryDcaFilter=7};

    ///Filter a track
    bool accept(const StiTrack*) const;

    void print() const;

private:

    ///Add a filter of a given type
    void addFilter(FilterType);

    ///Not implemented
    StiDynamicTrackFilter();
    
    void getNewState();
    
    void clearAndDestroy();
    
    StiIOBroker* mBroker;
    FilterMap mMap;
};

#endif

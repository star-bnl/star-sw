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

class ostream;
class StiTrack;

class StiDynamicTrackFilter : public Observer
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
    
    ///Implement the update() function from the base class.
    virtual void update(Subject*);
    virtual void forgetSubject(Subject*);
    void getNewState();
    
    void clearAndDestroy();
    
    Subject* mSubject;
    StiIOBroker* mBroker;
    FilterMap mMap;
};

//inlines

inline void StiDynamicTrackFilter::update(Subject* changedSubject)
{
    if (changedSubject==mSubject) {
	getNewState();
    }   
}

inline void StiDynamicTrackFilter::forgetSubject(Subject* obsolete)
{
    if (obsolete==mSubject) {
	mSubject=0;
    }
}

#endif

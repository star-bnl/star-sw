//Depracated - do not use.
//StiDynamicTrackFilter.cxx
//M.L. Miller (Yale Software)
//3/02

//std
#include <iostream>
using namespace std;
#include <vector>

//Sti
#include "StiTrack.h"
#include "SubjectObserver.h"
#include "StiIOBroker.h"
//#include "StiTrackFilters.h"
#include "StiDynamicTrackFilter.h"

StiDynamicTrackFilter::StiDynamicTrackFilter(StiIOBroker* b) : Observer(b), mBroker(b)
{
    cout <<"StiDynamicTrackFilter::StiDynamicTrackFilter()"<<endl;
    mSubject->attach(this);
    getNewState();
}

StiDynamicTrackFilter::~StiDynamicTrackFilter()
{
    cout <<"StiDynamicTrackFilter::~StiDynamicTrackFilter()"<<endl;
    if (mSubject) {
	mSubject->detach(this);
    }
    clearAndDestroy();
}

void StiDynamicTrackFilter::clearAndDestroy()
{
    for (FilterMap::iterator it=mMap.begin(); it!=mMap.end(); ++it) {
	delete (*it).second;
	(*it).second=0;
    }
    mMap.clear();
}

bool StiDynamicTrackFilter::accept(const StiTrack* t) const
{
    FilterMap::const_iterator it=mMap.begin();
    FilterMap::const_iterator end = mMap.end();
    bool accepted=true;
    
    while (accepted==true && it!=end) {
	accepted = (*it++).second->operator()(t);
    }
    return accepted;
}

void StiDynamicTrackFilter::addFilter(FilterType t)
{
  /*
    if (t==kPtFilter) {
	mMap[t] = new StiPtFilter(mSubject, mBroker);
    }
    else if (t==kEtaFilter) {
	mMap[t] = new StiEtaFilter(mSubject, mBroker);
    }
    else if (t==kChi2Filter) {
	mMap[t] = new StiChi2Filter(mSubject, mBroker);
    }
    else if (t==kNptsFilter) {
	mMap[t] = new StiNptsFilter(mSubject, mBroker);
    }
    else if (t==kNFitPtsFilter) {
	mMap[t] = new StiNFitPtsFilter(mSubject, mBroker);
    }
    else if (t==kNGapsFilter) {
	mMap[t] = new StiNGapsFilter(mSubject, mBroker);
    }
    else if (t==kFitPointRatioFilter) {
	mMap[t] = new StiFitPointRatioFilter(mSubject, mBroker);
    }
    else if (t==kPrimaryDcaFilter) {
	mMap[t] = new StiPrimaryDcaFilter(mSubject, mBroker);
    }
    else {
	cout <<"StiDynamicTrackFilter::addFilter().  ERROR:\t"
	     <<"Unknown filter type:\t"<<static_cast<int>(t)<<endl;
	     }*/
}

void StiDynamicTrackFilter::getNewState()
{
    cout <<"StiDynmaicTrackFilter::getNewState()"<<endl;
    clearAndDestroy();
    const vector<int>& vec = mBroker->filterTypes();
    for (vector<int>::const_iterator it=vec.begin(); it!=vec.end(); ++it) {
	const FilterType t = static_cast<const FilterType>((*it));
	addFilter(t);
    }
    for (FilterMap::iterator it2=mMap.begin(); it2!=mMap.end(); ++it2) {
	(*it2).second->getNewState();
    }
}

void StiDynamicTrackFilter::print() const
{
    cout <<"StiDynamicTrackFilter::print(ostream& os).  NFilter:\t"<<mMap.size()<<endl;
    for (FilterMap::const_iterator it=mMap.begin(); it!=mMap.end(); ++it) {
	(*it).second->print();
	cout <<endl;
    }
}



#include "StiTrackFilter.h"
#include "StiObjectFactoryInterface.h"

StiTrackFilter::StiTrackFilter()
  : analyzedTrackCount(0),
    acceptedTrackCount(0)
{}

StiTrackFilter::~StiTrackFilter()
{}

StiTrackFilterFactory::StiTrackFilterFactory(const string& newName,
					       int original,
					       int incremental, 
					       int maxInc)
  : StiObjectFactoryInterface<StiTrackFilter>(newName, 
					      original, 
					      incremental, 
					      maxInc)
{
  initialize();
}

StiTrackFilterFactory::~StiTrackFilterFactory()
{
  // cout <<"StiTrackFilterFactory::~StiTrackFilterFactory()"<<endl;
}

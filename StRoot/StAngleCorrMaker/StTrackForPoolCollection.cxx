#include "StTrackForPoolCollection.h"
#include "StTrackForPool.h"

StTrackForPoolCollection::StTrackForPoolCollection() {}

StTrackForPoolCollection::~StTrackForPoolCollection() {}

void
StTrackForPoolCollection::AddTrack(StTrackForPool* track)
{
  vec.push_back(track);    
}

StTrackForPool*
StTrackForPoolCollection::GetTrack(int index)
{
  return vec[index];
}

int 
StTrackForPoolCollection::Size() 
{
  return vec.size();
}

void 
StTrackForPoolCollection::Clear() 
{
  if (vec.size()!=0) vec.clear();
}

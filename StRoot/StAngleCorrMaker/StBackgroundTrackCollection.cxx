#include "StBackgroundTrackCollection.h"
#include "StTrackForPoolCollection.h"
#include "StTrackForPool.h"

StBackgroundTrackCollection::StBackgroundTrackCollection() {}

StBackgroundTrackCollection::~StBackgroundTrackCollection() {}

void
StBackgroundTrackCollection::AddTrackCollection(StTrackForPoolCollection tracks)
{
  vec.push_back(tracks);    
}

StTrackForPool*
StBackgroundTrackCollection::GetTrack(int eventIndex, int trackIndex)
{
  return vec[eventIndex].GetTrack(trackIndex);
}

StTrackForPoolCollection&
StBackgroundTrackCollection::GetTracks(int eventIndex)
{
  return vec[eventIndex];
}

int 
StBackgroundTrackCollection::Size() 
{
  return vec.size();
}

void 
StBackgroundTrackCollection::Clear() 
{
  if (vec.size()!=0) vec.clear();
}

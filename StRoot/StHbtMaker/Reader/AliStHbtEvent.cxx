// any problems, send an email to chajecki@mps.ohio-state.edu
#include "AliStHbtEvent.h"
#include "AliStHbtTrack.h"
#include "TClonesArray.h"

ClassImp(AliStHbtEvent)

AliStHbtEvent::AliStHbtEvent() 
    {
      fTracks = new TClonesArray("AliStHbtTrack", 20000);
      mNTracks = 0;
    }

int AliStHbtEvent::GetRunNumber() 
{
  return mRunNumber;
}

void AliStHbtEvent::SetRunNumber(int number) 
{
  mRunNumber = number;
}

int AliStHbtEvent::GetEventNumber() 
{
  return mEventNumber;
}

void AliStHbtEvent::SetEventNumber(int number) 
{
  mEventNumber = number;
}

void AliStHbtEvent::Clear(const Option_t* option="")
{
  fTracks->Clear("C");
  mNTracks = 0;
}

AliStHbtTrack* AliStHbtEvent::AddTrack()
{
  TClonesArray &tracks = *fTracks;
  AliStHbtTrack *track = new(tracks[mNTracks++]) AliStHbtTrack;
  return track;
}


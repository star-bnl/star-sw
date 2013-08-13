///\file StiSeedFinder.cxx 
///\author Victor Perev(BNL) Nov 2012
#include "StiSeedFinder.h"


//______________________________________________________________________________
StiSeedFinder::StiSeedFinder(): StiTrackFinder()
{
  reset();
}

//______________________________________________________________________________
StiSeedFinder::~StiSeedFinder()
{
}

//______________________________________________________________________________
/// Produce the next track seed 
//______________________________________________________________________________
StiTrack* StiSeedFinder::findTrack(double rMin)
{
   for (;mIdx<(int)mVec.size();mIdx++) {
     StiTrack* tk = mVec[mIdx]->findTrack(rMin);
     if (tk) return tk;
   }
   return 0;
}
//______________________________________________________________________________
void StiSeedFinder::reset()
{
   mIdx=0;
   for (int i=0;i<(int)mVec.size();i++){mVec[i]->reset();}

}
//______________________________________________________________________________
void StiSeedFinder::add(StiTrackFinder *seedFinder)
{
  mVec.push_back(seedFinder);
}

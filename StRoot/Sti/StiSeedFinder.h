/// \File StiSeedFinder.h
/// \author Victor Perev (BNL Software) 11/2012
#ifndef StiSeedFinder_HH
#define StiSeedFinder_HH
#include "Stiostream.h"
#include <vector>
#include "Sti/Base/EditableParameters.h"
#include "Sti/StiTrackFinder.h"


/// \class StiSeedFinder
/// StiSeedFinder is a container of concrete implementations of seed finder.
//// \author Victor Perev (BNL) Nov 2012

class StiSeedFinder : public StiTrackFinder
{
public:
  StiSeedFinder();
  virtual ~StiSeedFinder();
  StiTrack* findTrack(double rMin=0);
  virtual void reset();
  void add(StiTrackFinder *seedFinder);
   int size() const {return mVec.size();}
protected:
std::vector<StiTrackFinder*> mVec;
int mIdx;


};


#endif

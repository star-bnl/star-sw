#ifndef StiTrackFilter_H
#define StiTrackFilter_H 1

#include <iostream.h>
#include <stdlib.h>

class StiTrack;

class StiTrackFilter 
{
 public:

  StiTrackFilter();
  
  virtual void setDefaults();
  virtual bool accept(StiTrack * track);
  void reset();

  int  getAnalyzedTrackCount();
  int  getAcceptedTrackCount();

 protected:

  int analyzedTrackCount;
  int acceptedTrackCount;

};

#endif

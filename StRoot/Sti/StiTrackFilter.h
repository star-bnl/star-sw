#ifndef StiTrackFilter_H
#define StiTrackFilter_H 1

#include <iostream.h>
#include <stdlib.h>
#include "TObject.h"
#include "StiTrack.h"

class StiTrackFilter : public TObject
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

  ClassDef(StiTrackFilter, 1)

};

#endif

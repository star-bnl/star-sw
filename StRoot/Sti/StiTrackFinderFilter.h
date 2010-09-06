#ifndef StiTrackFinderFilter_H_INCLUDED
#define StiTrackFinderFilter_H_INCLUDED
#include <string>
#include "StiTrack.h"
#include "Base/Filter.h"
////////#include "Base/Parameter.h"
class StiTrackFinderFilter : public Filter<StiTrack>
{
 public:
  StiTrackFinderFilter();
  StiTrackFinderFilter(const string & name, const string & description);
  virtual ~StiTrackFinderFilter();
  bool accept(const StiTrack *filtered) const;
  virtual void initialize();
  virtual void setDefaults();

  typedef enum{ // type of return value for the fit() procedure
    kNoErrors = 0,
    kNoEnoughValidHits,
    kNoEnoughFittedValidHits,
    kWeird
  } TAcceptStatus;
  
 protected:
	int _minPtsCount;
	int _minFitPtsCount;
	float _minFitToPtsRatio;

};

#endif

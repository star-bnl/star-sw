#ifndef StiDefaultTrackFilter_H_INCLUDED
#define StiDefaultTrackFilter_H_INCLUDED 1

#include "StiTrack.h"
#include "EditableFilter.h"

typedef Filter<StiTrack> TrackFilter;

class StiDefaultTrackFilter : public EditableFilter<StiTrack>
{
 public:
  StiDefaultTrackFilter();
  StiDefaultTrackFilter(const string & name, const string & description);
  virtual ~StiDefaultTrackFilter();
  bool accept(const StiTrack *filtered) const;
  virtual void initialize();
  virtual void setDefaults();
};

inline bool StiDefaultTrackFilter::accept(const StiTrack * t) const
{
  //cout << "StiDefaultTrackFilter::accept(t) - INFO - Starting" << endl;
  double v,low,high;
  ParameterConstIterator iter = begin();
  Parameter * parUse;
  Parameter * parLow;
  Parameter * parHi;
  while (iter!=end())
    {
      parUse = *iter; iter++;
      parLow = *iter; iter++;
      parHi  = *iter; iter++;
      if (parUse&&parLow&&parHi)
	{
	  if (parUse->getValue())
	    {
	      //cout << "/"<<count++;
	      v = t->getValue(parUse->getKey());
	      low = parLow->getValue();
	      high = parHi->getValue();
	      if (v<low || v>high)
		{
		  //cout<<"/false"<<endl;
		  return false;
		}
	    }
	}
      else
	{
	  //cout << "StiDefaultTrackFilter::accept(StiTrack * t) - INFO - Internal Error" << endl;
	}
    }
  //cout<<"/true"<<endl;
  return true;
}


inline void StiDefaultTrackFilter::setDefaults()
{
}


#endif

#ifndef StiDefaultHitFilter_H_INCLUDED
#define StiDefaultHitFilter_H_INCLUDED 1

#include "StiHit.h"
#include "Base/EditableFilter.h"
#include "Base/Parameter.h"

class StiDefaultHitFilter : public EditableFilter<StiHit>
{
 public:
  StiDefaultHitFilter();
  StiDefaultHitFilter(const string & name, const string & description);
  virtual ~StiDefaultHitFilter();
  bool accept(const StiHit *filtered) const;
  virtual void initialize();
  virtual void setDefaults();
};

inline bool StiDefaultHitFilter::accept(const StiHit * t) const
{
  //cout << "StiDefaultHitFilter::accept(t) - INFO - Starting" << endl;
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
	  if (parUse->getBoolValue())
	    {
	      //cout << "/"<<count++;
	      v = t->getValue(parUse->getKey());
	      low = parLow->getDoubleValue();
	      high = parHi->getDoubleValue();
	      if (v<low || v>high)
		{
		  //cout<<"/false"<<endl;
		  return false;
		}
	    }
	}
      else
	{
	  //cout << "StiDefaultHitFilter::accept(StiHit * t) - INFO - Internal Error" << endl;
	}
    }
  //cout<<"/true"<<endl;
  return true;
}


inline void StiDefaultHitFilter::setDefaults()
{}


#endif

#ifndef StiDefaultTrackFilter_H_INCLUDED
#define StiDefaultTrackFilter_H_INCLUDED 1
#include "StiTrack.h"
#include "Base/EditableFilter.h"
#include "Base/Parameter.h"

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

#endif

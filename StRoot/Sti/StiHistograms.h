#ifndef StiHistograms_H_INCLUDED
#define StiHistograms_H_INCLUDED
#include "Sti/Base/HistogramGroup.h"
class StiTrackContainer;

class StiHistograms : public HistogramGroup
{
 public:
  StiHistograms();
  StiHistograms(const string & name, const string & description);
  virtual ~StiHistograms();
  virtual void initialize();
  virtual void fill(StiTrackContainer* container);
};



#endif

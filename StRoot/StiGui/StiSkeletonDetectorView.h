#ifndef StiSkeletonDetectorView_h_INCLUDED
#define StiSkeletonDetectorView_h_INCLUDED
#include "StiGui/StiDetectorView.h"

//Abstract base class defing the notion of a detector view.
class StiSkeletonDetectorView : public StiDetectorView
{
 public:
  StiSkeletonDetectorView(const string & name, const string & description, StiDetectorBuilder*builder);
  virtual ~StiSkeletonDetectorView();
  virtual void activate();
};

#endif



#ifndef StiAllVisibleDetectorView_h_INCLUDED
#define StiAllVisibleDetectorView_h_INCLUDED
#include "StiGui/StiDetectorView.h"

//Abstract base class defing the notion of a detector view.
class StiAllVisibleDetectorView : public StiDetectorView
{
 public:
  StiAllVisibleDetectorView(const string & name, const string & description, StiDetectorBuilder*builder);
  virtual ~StiAllVisibleDetectorView();
  virtual void activate();
};

#endif



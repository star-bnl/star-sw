#ifndef StiAllInvisibleDetectorView_h_INCLUDED
#define StiAllInvisibleDetectorView_h_INCLUDED
#include "StiGui/StiDetectorView.h"

//Abstract base class defing the notion of a detector view.
class StiAllInvisibleDetectorView : public StiDetectorView
{
 public:
  StiAllInvisibleDetectorView(const string & name, const string & description, StiDetectorBuilder*builder);
  virtual ~StiAllInvisibleDetectorView();
  virtual void activate();
};

#endif




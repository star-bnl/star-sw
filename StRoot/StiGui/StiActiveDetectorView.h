#ifndef StiActiveDetectorView_h_INCLUDED
#define StiActiveDetectorView_h_INCLUDED
#include "StiGui/StiDetectorView.h"

//Abstract base class defing the notion of a detector view.
class StiActiveDetectorView : public StiDetectorView
{
 public:
  StiActiveDetectorView(const string & name, const string & description, StiDetectorBuilder*builder);
  virtual ~StiActiveDetectorView();
  virtual void activate();
};

#endif



#ifndef StiDetectorView_h_INCLUDED
#define StiDetectorView_h_INCLUDED
#include "Sti/Base/Named.h"
#include "Sti/Base/Described.h"
#include "Sti/StiDetectorBuilder.h"

//Abstract base class defing the notion of a detector view.
class StiDetectorView : public Named, public Described
{
 public:
  StiDetectorView(const string & name, const string & description, StiDetectorBuilder*builder);
  virtual void activate()=0;
  StiDetectorBuilder * getBuilder();

 protected:

  StiDetectorBuilder * _builder;
};

inline StiDetectorBuilder * StiDetectorView::getBuilder()
{
  return _builder;
}


#endif



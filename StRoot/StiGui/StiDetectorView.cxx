#include "StiDetectorView.h"

StiDetectorView::StiDetectorView(const string & name, const string & description, StiDetectorBuilder*builder)
  : Named(name),
    Described(description),
    _builder(builder)
{}

  

#ifndef StiDetectorViews_H
#define StiDetectorViews_H
#include <vector>
#include "Sti/Base/Named.h"
#include "Sti/Base/Described.h"
#include "StiGui/StiDetectorView.h"

class StiDetectorViews : public Named, public Described, public vector<StiDetectorView*>
{
public:
    StiDetectorViews(const string &name,const string &description);
    virtual ~StiDetectorViews(){}; 
    virtual StiDetectorView* add(StiDetectorView*view);
    StiDetectorView * getDefaultView() const;
    void setDefaultView(StiDetectorView *defaultView);
 protected:
    StiDetectorView * _defaultView;
};

inline StiDetectorView * StiDetectorViews::add(StiDetectorView*view)
{
  push_back(view);
  return view;
}

inline StiDetectorView * StiDetectorViews::getDefaultView() const
{
  return _defaultView;
}

inline void StiDetectorViews::setDefaultView(StiDetectorView *defaultView)
{
  _defaultView = defaultView;
}

#endif 

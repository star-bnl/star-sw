#ifndef StiView_h_INCLUDED
#define StiView_h_INCLUDED

//Define some simple classes for controlling the "view" that we're in
class StiView
{
public:
    virtual void setToDefault() = 0;
};

//Padrow 45, svt, ssd
class StiSkeletonView : public StiView
{
 public:
  virtual void setToDefault();
};

//Padrow 1, svt, ssd
class StiZoomSkeletonView : public StiView
{
 public:
  virtual void setToDefault();
};

//Only those detectors which satisfy isOn()==true
class StiManualView : public StiView
{
 public:
  virtual void setToDefault();
};

#endif



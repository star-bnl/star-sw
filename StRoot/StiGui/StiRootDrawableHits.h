#ifndef StiRootDrawableHits_H_INCLUDED
#define StiRootDrawableHits_H_INCLUDED
#include <vector>
#include "StiGui/StiDrawable.h"
#include "StiGui/StiTPolyMarker3D.h"
using namespace std;
class StiRootDrawableHits : public StiDrawable, public vector<double>
{
public:
    StiRootDrawableHits();
    virtual ~StiRootDrawableHits();
    virtual void draw();
    virtual void reset();
    virtual void setColor(int val);
    virtual void setStyle(int val);
    virtual void setSize(double val);
    virtual void setVisible(bool val);
    virtual void add(double x, double y, double z);
protected: 
    bool _visible;
    int  _color;
    int  _style;
    StiTPolyMarker3D _markers;
};

#endif

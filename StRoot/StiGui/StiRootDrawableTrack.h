#ifndef StiRootDrawableTrack_H_INCLUDED
#define StiRootDrawableTrack_H_INCLUDED
#include <vector>
#include <deque>
#include "StiGui/StiDrawable.h"
#include "StiGui/StiTPolyLine3D.h"
using namespace std;

/*! \class StiRootDrawableTrack
  Concrete class used to draw tracks
  \author Claude A Pruneau
*/
class StiRootDrawableTrack : public StiDrawable, public vector<double>
{
public:
    StiRootDrawableTrack();
    virtual ~StiRootDrawableTrack();
    virtual void draw();
    virtual void reset();
    virtual void setColor(int val);
    virtual void setStyle(int val);
    virtual void setSize(double val);
    virtual void setVisible(bool val);
    virtual void add(double x, double y, double z, int direction=1);
protected:
    bool _visible;
    double _rMin;
    double _rMax;
    deque<double> *_data;
    StiTPolyLine3D *_line;
};

#endif

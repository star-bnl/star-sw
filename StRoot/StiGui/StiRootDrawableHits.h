//StiRootDrawableHits.h
//M.L. Miller (Yale Software)
//07/01

#ifndef StiRootDrawableHits_HH
#define StiRootDrawableHits_HH

#include "StiDrawableHits.h"
class StiTPolyMarker3D;

class StiRootDrawableHits : public StiDrawableHits
{
public:
    
    StiRootDrawableHits();
    virtual ~StiRootDrawableHits();

    virtual void fillHitsForDrawing();

    virtual void draw();
    virtual void update();
    virtual void setColor(int val);
    virtual void setVisibility(bool val);

    virtual void setMarkerStyle(unsigned int);
    virtual void setMarkerSize(double);
    virtual void add(double x, double y, double z);

protected:
    StiTPolyMarker3D* mpoly;
    int mcolor;
    bool mvisible;
    unsigned int mmarker;
    
private:

};

#endif

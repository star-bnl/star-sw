//StiDrawableHits.h
//M.L. Miller (Yale Software)
//07/01

#ifndef StiDrawableHits_HH
#define StiDrawableHits_HH

#include <vector>

#include "StiDrawable.h"

class StiHit;
class StThreeVectorD;

typedef vector<StiHit*> const_hit_vector;

class StiDrawableHits : public StiDrawable, public const_hit_vector
{
public:

    StiDrawableHits();
    virtual ~StiDrawableHits();

    //Most drawing libraries require an array of points to be drawn
    virtual void fillHitsForDrawing() = 0;
    
    virtual void draw() = 0;
    virtual void update() = 0;
    virtual void setColor(int val) = 0;
    virtual void setVisibility(bool val) = 0;

    virtual void setMarkerStyle(unsigned int) = 0;
    virtual void setMarkerSize(double) = 0;

protected:
    
private:

};

#endif

//StiDrawableHits.h
//M.L. Miller (Yale Software)
//07/01

#ifndef StiDrawableHits_HH
#define StiDrawableHits_HH

#include <vector>
using std::vector;

#include "StiDrawable.h"

class StiHit;
class StThreeVectorD;

#ifndef __CINT__
typedef double ThreeDimPoint_t; //we'll have to change this if we ever leave root
typedef vector<ThreeDimPoint_t> ThreeDimPointVec_t;

#else
class ThreeDimPoint_t;
class ThreeDimPointVec_t;
#endif

class StiDrawableHits : public StiDrawable, public ThreeDimPointVec_t
{
public:

    StiDrawableHits();
    virtual ~StiDrawableHits();
    virtual void fillHitsForDrawing() = 0;
    virtual void draw() = 0;
    virtual void update() = 0;
    virtual void setColor(int val) = 0;
    virtual void setType(int val) = 0;
    virtual void setSize(double val) = 0;
    virtual void setVisibility(bool val) = 0;
    virtual void add(double x, double y, double z)=0;
};

#endif

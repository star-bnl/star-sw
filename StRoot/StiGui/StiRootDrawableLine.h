//StiRootDrawableLine.h
//M.L. Miller (Yale Software)
//07/01

#ifndef StiRootDrawableLine_HH
#define StiRootDrawableLine_HH

#include "StiDrawableHits.h"
#include "StThreeVector.hh"

class StiTPolyLine3D;

class StiRootDrawableLine : public StiDrawableHits
{
public:
    
    StiRootDrawableLine();
    virtual ~StiRootDrawableLine();

    virtual void fillHitsForDrawing();

    virtual void draw();
    virtual void update();
    virtual void setColor(int val);
    virtual void setVisibility(bool val);
    
    virtual void setMarkerStyle(unsigned int val);
    virtual void setMarkerSize(double);
    
    virtual void setLineStyle(unsigned int val);
    virtual void setLineWidth(double);
    
    virtual void clearLine(); //clear graphical rep
    
protected:
    StiTPolyLine3D* mline;
    int mcolor;
    bool mvisible;
    
private:

};

#endif

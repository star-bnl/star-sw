//StiRootDrawableLine.h
//M.L. Miller (Yale Software)
//07/01

#ifndef StiRootDrawableLine_HH
#define StiRootDrawableLine_HH

#include "StiDrawableHits.h"
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

    virtual const char* name() const;

protected:
    StiTPolyLine3D* mline;
    double* marray;
    int mcolor;
    bool mvisible;
    char* mname;
    
private:

};

#endif

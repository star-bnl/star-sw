//StiRootDrawableDetector.h
//M.L. Miller (Yale Software)
//04/01

#ifndef StiRootDrawableDetector_HH
#define StiRootDrawableDetector_HH

#include "Sti/StiDetector.h"

#include "StiRootDrawable.h"

class StiRootDrawableDetector : public StiDetector , public StiRootDrawable
{
public:

    StiRootDrawableDetector();
    virtual ~StiRootDrawableDetector();
    
protected:

    //Implement StiRootDrawable interface
    virtual void draw();
    virtual void update();

    //Overide StiDetector methods
    virtual void build();
    
protected:
    virtual void makeShape();
};

#endif

//StiDrawableDetector.h
//M.L. Miller (Yale Software)
//04/01

#ifndef StiDrawableDetector_HH
#define StiDrawableDetector_HH

#include "Sti/StiDetector.h"
#include "StiDrawable.h"

class StiDrawableDetector : public StiDetector , public StiDrawable
{
public:

    StiDrawableDetector();
    virtual ~StiDrawableDetector();
    
protected:

    //Implement StiDrawable interface
    virtual void draw();
    virtual void update();
    virtual const char* name() const;

    //Overide StiDetector methods
    virtual void build(const char* infile);
    
protected:
    virtual void makeShape();
};

#endif

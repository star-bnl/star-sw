//StiRootDrawable.h
//M.L. Miller (Yale Softwarw)
//06/01

//Abstract base class for objects that are drawable using  ROOT libraries

#ifndef StiRootDrawable_HH
#define StiRootDrawable_HH

#include "StiDrawable.h"

class StiRootDrawable : public StiDrawable
{
public:
    StiRootDrawable();
    virtual ~StiRootDrawable();
    
    virtual void draw() = 0;
    virtual void update() = 0;
    virtual void setColor(int val);
    virtual void setVisibility(bool val);
    
    virtual const char* name() const = 0;
    
protected:
    virtual void makeShape() = 0;
    
    TRotMatrix* mrotation;
    TShape* mshape;
    TNode* mnode;
private:    
};
#endif

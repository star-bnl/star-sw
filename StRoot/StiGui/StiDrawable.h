//StiDrawable.h
//M.L. Miller (Yale Software)
//04/01

//abstract base class for an sti displayable class

#ifndef StiDrawable_HH
#define StiDrawable_HH

class TRotMatrix;
class TShape;
class TNode;

class StiDrawable
{
public:
    StiDrawable();
    virtual ~StiDrawable();

    virtual void draw() = 0;
    virtual void update() = 0;
    virtual void setColor(int val) = 0;
    virtual void setVisibility(bool val) = 0;
    
    virtual const char* name() const = 0;
    
protected:
};

#endif

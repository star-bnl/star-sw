///\file StiRootDrawable.h
///\author M.L. Miller (Yale Softwarw)
///\date 06/2001


#ifndef StiRootDrawable_HH
#define StiRootDrawable_HH

#include "StiDrawable.h"
#include "StThreeVector.hh"

class TShape;
class TVolume;
class TNode;
class TRotMatrix;

///Abstract base class for objects that are drawable using  ROOT libraries
class StiRootDrawable : public StiDrawable
{
public:
    StiRootDrawable();
    virtual ~StiRootDrawable();
    virtual void setColor(int val);
    virtual void setStyle(int val);
    virtual void setSize(double val);
    virtual void setVisible(bool val);
    TRotMatrix* rotation() const {return mrotation;}
    TShape* shape() const {return mshape;}
    TVolume* volume() const {return mnode;}
    const StThreeVector<double>& position() const {return mposition;}
protected:
    virtual void makeShape() = 0;
    TRotMatrix* mrotation;
    TShape* mshape;
    TVolume* mnode;
    TVolume* mselfnode; //used for local rotations
    TRotMatrix* mselfrotation; //ibid
    StThreeVector<double> mposition;
};

inline void StiRootDrawable::setStyle(int val)
{}

inline void StiRootDrawable::setSize(double val)
{}

#endif

///\file StiRootDrawableDetector.h
///\author M.L. Miller (Yale Software)
///\date 04/2001
#ifndef StiRootDrawableDetector_HH
#define StiRootDrawableDetector_HH
#include "Sti/StiDetector.h"
#include "StiRootDrawable.h"

///\class StiRootDrawableDetector
/// Class defining a root drawable detector volume. The properties of the StiDetector
/// are used to set and define a ROOT drawable volume.
class StiRootDrawableDetector : public StiDetector , public StiRootDrawable
{
public:
    StiRootDrawableDetector();
    virtual ~StiRootDrawableDetector();
    virtual void reset();
    virtual void draw();
 protected:
    virtual void build();
    virtual void makeShape();
};

#endif

///\file StiRootDrawableHitContainer.h
///\author M.L. Miller (Yale Software)
///\date 09/2001
#ifndef StiRootDrawableHitContainer_HH
#define StiRootDrawableHitContainer_HH
#include "Sti/StiHitContainer.h"
#include "StiGui/StiRootDrawableHits.h"

class StiRootDrawableHitContainer : public StiHitContainer, public StiRootDrawableHits
{
public:
    StiRootDrawableHitContainer(const string & name, const string & description);
    virtual ~StiRootDrawableHitContainer();
    virtual void clear();
    virtual void update();
};

#endif

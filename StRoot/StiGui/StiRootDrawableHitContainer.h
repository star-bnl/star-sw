//StiRootDrawableHitContainer.h
//M.L. Miller (Yale Software)
//09/01

#ifndef StiRootDrawableHitContainer_HH
#define StiRootDrawableHitContainer_HH

//Sti
#include "Sti/StiHitContainer.h"

//StiGui
#include "StiRootDrawableHits.h"

class StiRootDrawableHitContainer : public StiHitContainer, public StiRootDrawableHits
{
public:
    friend class StiHitContainer;

    virtual void clear();
    virtual void update();
    
protected:
    StiRootDrawableHitContainer();
    virtual ~StiRootDrawableHitContainer();
private:
};

#endif

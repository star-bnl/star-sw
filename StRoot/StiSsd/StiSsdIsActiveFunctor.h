/**
 * file StiSsdIsActiveFunctor.h
 * class StiSsdIsActiveFunctor
 * brief function object for determine a SSD ladder's active regions
 */

#ifndef STI_SSD_IS_ACTIVE_FUNCTOR
#define STI_SSD_IS_ACTIVE_FUNCTOR

#include "Sti/StiIsActiveFunctor.h"

struct StiSsdIsActiveFunctor : public StiIsActiveFunctor
{
    StiSsdIsActiveFunctor();
    virtual ~StiSsdIsActiveFunctor();
    virtual bool operator()(double dYlocal, double dZlocal) const;
};

#endif // defined STI_SSD_IS_ACTIVE_FUNCTOR

/**
 * @file StiSvtIsActiveFunctor.h
 * @class StiSvtIsActiveFunctor
 * @brief function object for determine a SVT ladder's active regions
 *
 * @author Ben Norman, Kent State University
 * @date March 2002
 */

#ifndef STI_SVT_IS_ACTIVE_FUNCTOR
#define STI_SVT_IS_ACTIVE_FUNCTOR

#include "Sti/StiIsActiveFunctor.h"

struct StiSvtIsActiveFunctor : public StiIsActiveFunctor{
    StiSvtIsActiveFunctor();
    virtual ~StiSvtIsActiveFunctor();
    virtual bool operator()(double dYlocal, double dZlocal);
};

#endif // defined STI_SVT_IS_ACTIVE_FUNCTOR
